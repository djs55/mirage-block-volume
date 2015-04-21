(*
 * Copyright (C) 2011-2013 Citrix Inc
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

open Common
open Cmdliner
open Lwt
open Lvm

module Time = struct
  type 'a io = 'a Lwt.t
  let sleep = Lwt_unix.sleep
end

let require name arg = match arg with
  | None -> failwith (Printf.sprintf "Please supply a %s argument" name)
  | Some x -> x

let (>>*=) m f = match m with
  | `Error (`Msg e) -> fail (Failure e)
  | `Error (`DuplicateLV x) -> fail (Failure (Printf.sprintf "%s is a duplicate LV name" x))
  | `Error (`OnlyThisMuchFree x) -> fail (Failure (Printf.sprintf "There is only %Ld free" x))
  | `Error (`UnknownLV x) -> fail (Failure (Printf.sprintf "I couldn't find an LV named %s" x))
  | `Ok x -> f x
let (>>|=) m f = m >>= fun x -> x >>*= f

let apply common =
  ()

let add_prefix x xs = List.map (function
  | [] -> []
  | y :: ys -> (x ^ "/" ^ y) :: ys
) xs

let table_of_pv_header prefix pvh = add_prefix prefix [
  [ "id"; Uuid.to_string pvh.Label.Pv_header.id; ];
  [ "device_size"; Int64.to_string pvh.Label.Pv_header.device_size; ];
  [ "extents"; string_of_int (List.length pvh.Label.Pv_header.extents) ];
  [ "metadata_areas"; string_of_int (List.length pvh.Label.Pv_header.metadata_areas) ];
]

let table_of_pv pv = add_prefix (Pv.Name.to_string pv.Pv.name) [
  [ "name"; Pv.Name.to_string pv.Pv.name; ];
  [ "id"; Uuid.to_string pv.Pv.id; ];
  [ "status"; String.concat ", " (List.map Pv.Status.to_string pv.Pv.status) ];
  [ "size_in_sectors"; Int64.to_string pv.Pv.size_in_sectors ];
  [ "pe_start"; Int64.to_string pv.Pv.pe_start ];
  [ "pe_count"; Int64.to_string pv.Pv.pe_count; ]
] @ (table_of_pv_header (Pv.Name.to_string pv.Pv.name ^ "/label") pv.Pv.label.Label.pv_header)

let table_of_lv lv = add_prefix lv.Lv.name [
  [ "name"; lv.Lv.name; ];
  [ "id"; Uuid.to_string lv.Lv.id; ];
  [ "tags"; String.concat ", " (List.map Name.Tag.to_string lv.Lv.tags) ];
  [ "status"; String.concat ", " (List.map Lv.Status.to_string lv.Lv.status) ];
  [ "segments"; Sexplib.Sexp.to_string (Lv.Segment.sexp_of_ts lv.Lv.segments) ];
]

let table_of_vg vg =
  let pvs = List.flatten (List.map table_of_pv vg.Vg.pvs) in
  let lvs = Vg.LVs.fold (fun _ lv acc -> lv :: acc) vg.Vg.lvs [] in
  let lvs = List.flatten (List.map table_of_lv lvs) in [
  [ "name"; vg.Vg.name ];
  [ "id"; Uuid.to_string vg.Vg.id ];
  [ "status"; String.concat ", " (List.map Vg.Status.to_string vg.Vg.status) ];
  [ "extent_size"; Int64.to_string vg.Vg.extent_size ];
  [ "max_lv"; string_of_int vg.Vg.max_lv ];
  [ "max_pv"; string_of_int vg.Vg.max_pv ];
] @ pvs @ lvs @ [
  [ "free_space"; Int64.to_string (Pv.Allocator.size vg.Vg.free_space) ];
]

let with_block filename f =
  let open Lwt in
  Block.connect filename
  >>= function
  | `Error _ -> fail (Failure (Printf.sprintf "Unable to read %s" filename))
  | `Ok x ->
    Lwt.catch (fun () -> f x) (fun e -> Block.disconnect x >>= fun () -> fail e)

module Log = struct
  let debug fmt = Printf.ksprintf (fun s -> print_endline s) fmt
  let info  fmt = Printf.ksprintf (fun s -> print_endline s) fmt
  let error fmt = Printf.ksprintf (fun s -> print_endline s) fmt
end

let read common filename =
  apply common;
  let module Vg_IO = Vg.Make(Log)(Block)(Time)(Clock) in
  try
    let filename = require "filename" filename in
    let t =
      with_block filename
        (fun x ->
          Vg_IO.connect [ x ] `RO >>|= fun vg ->
          return vg 
        )in
    let vg = Lwt_main.run t in
    Common.print_table [ "key"; "value" ] (table_of_vg (Vg_IO.metadata_of vg));
    `Ok ()
  with
    | Failure x ->
      `Error(true, x)

let format common filename vgname pvname journalled =
  apply common;
  let module Vg_IO = Vg.Make(Log)(Block)(Time)(Clock) in
  try
    let filename = require "filename" filename in
    begin match Pv.Name.of_string pvname with
    | `Error (`Msg x) -> failwith x
    | `Ok pvname ->
      let t =
        with_block filename
          (fun x ->
            Vg_IO.format vgname ~magic:(if journalled then `Journalled else `Lvm) [ pvname, x ] >>|= fun () ->
            return ()
          ) in
      Lwt_main.run t;
      `Ok ()
    end
  with
  | Failure x ->
    `Error(true, x)

let map common filename lvname =
  apply common;
  let module Vg_IO = Vg.Make(Log)(Block)(Time)(Clock) in
  try
    let filename = require "filename" filename in
    let t =
      with_block filename
        (fun x ->
          Vg_IO.connect [ x ] `RO >>|= fun vg ->
          let lv = Vg.LVs.find_by_name lvname (Vg_IO.metadata_of vg).Vg.lvs in
          List.iter (fun seg ->
            Printf.printf "start %Ld, count %Ld %s\n" seg.Lv.Segment.start_extent seg.Lv.Segment.extent_count
              (match seg.Lv.Segment.cls with
               | Lv.Segment.Linear x ->
                 Printf.sprintf "from %s starting at %Ld" (Pv.Name.to_string x.Lv.Linear.name) x.Lv.Linear.start_extent)
          ) lv.Lv.segments;
          return ()
        ) in
    Lwt_main.run t;
    `Ok ()
  with
    | Failure x ->
      `Error(true, x)


let update_vg common filename f =
  apply common;
  let module Vg_IO = Vg.Make(Log)(Block)(Time)(Clock) in
  try
    let filename = require "filename" filename in
    let t =
      with_block filename
        (fun x ->
          let devices = [ x ] in
          Vg_IO.connect devices `RW >>|= fun vg ->
          f (Vg_IO.metadata_of vg) >>*= fun (_,op) ->
          Vg_IO.update vg [ op ] >>|= fun () ->
          Vg_IO.sync vg >>|= fun () ->
          return ()
        ) in
    Lwt_main.run t;
    `Ok ()
  with
    | Failure x ->
      `Error(true, x)

let create common filename lvname size =
  let size_in_bytes = Common.parse_size size in
  update_vg common filename (fun vg -> Vg.create vg lvname size_in_bytes)

let rename common filename lvname newname =
  update_vg common filename (fun vg -> Vg.rename vg lvname newname)

let resize common filename lvname newsize =
  match newsize with
  | "" ->
    `Error(true, "Please supply a new size for the volume")
  | size ->
    let size_in_bytes = Common.parse_size size in
    update_vg common filename (fun vg -> Vg.resize vg lvname size_in_bytes)

let remove common filename lvname =
  update_vg common filename (fun vg -> Vg.remove vg lvname)

let add_tag common filename lvname tag =
  update_vg common filename (fun vg -> Vg.add_tag vg lvname tag)

let remove_tag common filename lvname tag =
  update_vg common filename (fun vg -> Vg.remove_tag vg lvname tag)
