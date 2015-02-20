(*
 * Copyright (C) 2009-2013 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
open Lvm
open Logging
open Vg

let dummy_mode = ref false
let dummy_base = ref "/tmp"

let rec mkdir_p x =
  if Sys.file_exists x
  then ()
  else
    let parent = Filename.dirname x in
    if not(Sys.file_exists parent) then mkdir_p parent;
    Unix.mkdir x 0o0755

type devices = (Uuid.t * string) list

let with_block filename f =
  let open Lwt in
  Block.connect filename
  >>= function
  | `Error _ -> fail (Failure (Printf.sprintf "Unable to read %s" filename))
  | `Ok x ->
    Lwt.catch (fun () -> f x) (fun e -> Block.disconnect x >>= fun () -> fail e)

let read devices =
  let open Lwt in
  let module Label_IO = Label.Make(Block) in
  Lwt_list.map_p (fun device ->
    with_block device
      (fun t ->
        Label_IO.read t
        >>= function
        | `Error x -> fail (Failure x)
        | `Ok x -> return (x.Label.pv_header.Label.Pv_header.id, device)
      )
  ) devices

let to_targets id_to_device vg lv =
  let segments = Lv.Segment.sort lv.Lv.segments in

  (* Sanity check - make sure the segments are logically contiguous *)

  let extent_to_phys_sector pv extent = Int64.add pv.Pv.pe_start (Int64.mul extent vg.extent_size) in
  let extent_to_sector extent = (Int64.mul extent vg.extent_size) in

  let rec test expected_start segs =
    match segs with
      | s::ss ->
          if s.Lv.Segment.start_extent <> expected_start
          then failwith "Segments aren't contiguous!";
          test (Int64.add expected_start s.Lv.Segment.extent_count) ss
      | _ -> ()
  in

  test 0L segments;

  let rec construct_dm_map segs =
    let open Devmapper in
    match segs with
      | s::ss ->
          let start = extent_to_sector s.Lv.Segment.start_extent in
          let size = extent_to_sector s.Lv.Segment.extent_count in
          { Target.start;
            size;
            kind =
              match s.Lv.Segment.cls with
                | Lv.Segment.Linear l ->
                    let pv = List.find (fun pv -> pv.Pv.name=l.Lv.Linear.name) vg.pvs in
                    if List.mem_assoc pv.Pv.id id_to_device
                    then
                      Target.Linear {
                        Location.device = Location.Path (List.assoc pv.Pv.id id_to_device);
                        offset = extent_to_phys_sector pv l.Lv.Linear.start_extent }
                    else
                      failwith (Printf.sprintf "Unable to find a device containing PV with id %s" (Uuid.to_string pv.Pv.id))
                | Lv.Segment.Striped st ->
                    failwith "Not implemented"
          }::construct_dm_map ss
      | _ -> []
  in

  construct_dm_map segments

let dash = Re_str.regexp_string "-"

let name_of vg lv =
  let vgname = String.concat "--" (Re_str.split_delim dash vg.name) in
  let lvname = String.concat "--" (Re_str.split_delim dash lv.Lv.name) in
  Printf.sprintf "%s-%s" vgname lvname

let single_dash = Re_str.regexp "[^-]-[^-]"
let double_dash = Re_str.regexp_string "--"

let vg_lv_of_name path =
  let name = Filename.basename path in
  let dash_idx = Re_str.search_forward single_dash name 0 + 1 in
  let vgname = String.sub name 0 dash_idx in
  let lvname = String.sub name (dash_idx + 1) (String.length name - dash_idx - 1) in
  let unescape x = String.concat "-" (Re_str.split_delim double_dash x) in
  unescape vgname, unescape lvname

let dev_path_of vg lv =
  if !dummy_mode then begin
    let fname = Printf.sprintf "%s/%s/%s" (!dummy_base) (!Constants.mapper_name) (name_of vg lv) in
    let dirname = Filename.dirname fname in
    mkdir_p dirname;
    fname
  end else
    Printf.sprintf "/dev/mapper/%s" (name_of vg lv)

let dev_path_of_dm_name dm_name =
  if !dummy_mode then
    Printf.sprintf "%s/%s/%s" (!dummy_base) (!Constants.mapper_name) dm_name
  else
    Printf.sprintf "/dev/mapper/%s" dm_name
(*
let lv_activate_internal name dm_map use_tmp dev =
  let realname = if use_tmp then Uuidm.to_string (Uuidm.create `V4) else name in
  let nod = dev_path_of_dm_name realname in
  debug "Using dm_name=%s (use_tmp=%b)" realname use_tmp;
  if not !dummy_mode then begin
    Camldm.create realname dm_map dereference_table;
    let s = Camldm.table realname in
    let (major,minor) = s.Camldm.major,s.Camldm.minor in
    Camldm.mknod nod 0o644 (Int32.to_int major) (Int32.to_int minor);
  end else begin
    let fname = (Printf.sprintf "%s/%s/%s" !dummy_base dev name) in
    (* Make sure that a file corresponding to the LV is existant *)
    begin
      try
        ignore(Unix.stat fname);
      with _ ->
        let fd = Unix.openfile fname [Unix.O_RDWR; Unix.O_CREAT] 0o644 in
(*      let size = Int64.mul Constants.extent_size (Lv.size_in_extents lv) in
        if !Constants.full_provision
        then ignore(Unix.LargeFile.lseek fd (Int64.sub size 1L) Unix.SEEK_SET);*)
        ignore(Unix.write fd "\000" 0 1);
        Unix.close fd;
    end;
    (* Let's also make sure that the dir exists for the dev node! *)
    mkdir_p (Filename.dirname nod);
    (try Unix.unlink nod with _ -> ());
    Unix.symlink fname nod;
  end;
  (nod,realname)

let lv_activate vg lv =
  let name = dm_name_of vg lv in
  let dm_map = dm_map_of_lv vg lv false in
  let dev = (List.hd vg.pvs).Pv.stored_device in
  fst (lv_activate_internal name dm_map false dev)

let lv_deactivate_internal nod dm_name =
  let nod = match nod with None -> dev_path_of_dm_name dm_name | Some x -> x in
  if not !dummy_mode then Camldm.remove dm_name;
  Unix.unlink nod
let lv_deactivate vg lv =
        let dm_name = dm_name_of vg lv in
        (ignore (dev_path_of_dm_name dm_name);
         lv_deactivate_internal None dm_name)

let lv_change_internal dm_name dm_map dereference_table =
  if not !dummy_mode then begin
    Camldm.reload dm_name dm_map dereference_table;
    Camldm.suspend dm_name;
    Camldm.resume dm_name
  end

let finally f g =
  try
    let result = f () in
    g ();
    result
  with e ->
    g ();
    raise e

let with_active_lv vg lv use_tmp fn =
  let name = dm_name_of vg lv in
  let dm_map = dm_map_of_lv vg lv false in
  let dev = (List.hd vg.pvs).Pv.stored_device in
  let (nod,name) = lv_activate_internal name dm_map [] use_tmp dev in
  finally
    (fun () -> fn nod)
    (fun () -> lv_deactivate_internal (Some nod) name)

let get_absolute_pos_of_sector vg lv sector_num =
  let map = dm_map_of_lv vg lv false in
  let rec find i offset =
    if offset > map.(i).Camldm.len
    then find (i+1) (Int64.sub offset map.(i).Camldm.len)
    else
      let (device,offset) = Camldm.get_sector_pos_of map.(i) offset [] in
      (device,Int64.mul offset (Int64.of_int Constants.sector_size))
  in
  find 0 sector_num
*)
