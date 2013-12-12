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

open Logging
open Vg

let dm_map_of_lv vg lv use_pv_id =
  let segments = List.sort (fun s1 s2 -> compare s1.Lv.s_start_extent s2.Lv.s_start_extent)
    (lv.Lv.segments) in

  (* Sanity check - make sure the segments are logically contiguous *)

  let extent_to_phys_sector pv extent = Int64.add pv.Pv.pe_start (Int64.mul extent vg.extent_size) in
  let extent_to_sector extent = (Int64.mul extent vg.extent_size) in

  let rec test expected_start segs =
    match segs with
      | s::ss ->
          if s.Lv.s_start_extent <> expected_start
          then failwith "Segments aren't contiguous!";
          test (Int64.add expected_start s.Lv.s_extent_count) ss
      | _ -> ()
  in

  test 0L segments;

  let rec construct_dm_map segs =
    match segs with
      | s::ss ->
          let start = extent_to_sector s.Lv.s_start_extent in
          let len = extent_to_sector s.Lv.s_extent_count in
          { Camldm.start=start;
            len = len;
            map =
              match s.Lv.s_cls with
                | Lv.Linear l ->
                    let pv = List.find (fun pv -> pv.Pv.name=l.Lv.l_pv_name) vg.pvs in
                    Camldm.Linear {
                      Camldm.device =
                        if use_pv_id
                        then Camldm.Dereferenced (Lvm_uuid.to_string pv.Pv.label.Label.pv_header.Label.pvh_id)
                        else Camldm.Real pv.Pv.dev;
                      offset=extent_to_phys_sector pv l.Lv.l_pv_start_extent }
                | Lv.Striped st ->
                    failwith "Not implemented"
          }::construct_dm_map ss
      | _ -> []
  in

  Array.of_list (construct_dm_map segments)

let dash = Re_str.regexp_string "-"

let dm_name_of vg lv =
  let vgname = String.concat "--" (Re_str.split_delim dash vg.name) in
  let lvname = String.concat "--" (Re_str.split_delim dash lv.Lv.name) in
  Printf.sprintf "%s-%s" vgname lvname

let dev_path_of vg lv =
  if !Constants.dummy_mode then begin
    let fname = Printf.sprintf "%s/%s/%s" (!Constants.dummy_base) (!Constants.mapper_name) (dm_name_of vg lv) in
    let dirname = Filename.dirname fname in
    Unixext.mkdir_rec dirname 0o755;
    fname
  end else
    Printf.sprintf "/dev/mapper/%s" (dm_name_of vg lv)

let dev_path_of_dm_name dm_name =
  if !Constants.dummy_mode then
    Printf.sprintf "%s/%s/%s" (!Constants.dummy_base) (!Constants.mapper_name) dm_name
  else
    Printf.sprintf "/dev/mapper/%s" dm_name

let lv_activate_internal name dm_map dereference_table use_tmp dev =
  let realname = if use_tmp then Uuidm.to_string (Uuidm.create `V4) else name in
  let nod = dev_path_of_dm_name realname in
  debug "Using dm_name=%s (use_tmp=%b)" realname use_tmp;
  if not !Constants.dummy_mode then begin
    Camldm.create realname dm_map dereference_table;
    let s = Camldm.table realname in
    let (major,minor) = s.Camldm.major,s.Camldm.minor in
    Camldm.mknod nod 0o644 (Int32.to_int major) (Int32.to_int minor);
  end else begin
    let fname = (Printf.sprintf "%s/%s/%s" !Constants.dummy_base dev name) in
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
    Unixext.mkdir_rec (Filename.dirname nod) 0o755;
    Unixext.unlink_safe nod;
    Unix.symlink fname nod;
  end;
  (nod,realname)

let lv_activate vg lv =
  let name = dm_name_of vg lv in
  let dm_map = dm_map_of_lv vg lv false in
  let dev = (List.hd vg.pvs).Pv.dev in
  fst (lv_activate_internal name dm_map [] false dev)

let lv_deactivate_internal nod dm_name =
  let nod = match nod with None -> dev_path_of_dm_name dm_name | Some x -> x in
  if not !Constants.dummy_mode then Camldm.remove dm_name;
  Unix.unlink nod
let lv_deactivate vg lv =
        let dm_name = dm_name_of vg lv in
        (ignore (dev_path_of_dm_name dm_name);
         lv_deactivate_internal None dm_name)

let lv_change_internal dm_name dm_map dereference_table =
  if not !Constants.dummy_mode then begin
    Camldm.reload dm_name dm_map dereference_table;
    Camldm.suspend dm_name;
    Camldm.resume dm_name
  end

let with_active_lv vg lv use_tmp fn =
  let name = dm_name_of vg lv in
  let dm_map = dm_map_of_lv vg lv false in
  let dev = (List.hd vg.pvs).Pv.dev in
  let (nod,name) = lv_activate_internal name dm_map [] use_tmp dev in
  Pervasiveext.finally
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
