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

let require name arg = match arg with
  | None -> failwith (Printf.sprintf "Please supply a %s argument" name)
  | Some x -> x

let (>>|=) m f = m >>= function
  | `Error e -> fail (Failure e)
  | `Ok x -> f x
let (>>*=) m f = match m with
  | `Error e -> fail (Failure e)
  | `Ok x -> f x

module Disk_mirage_unix = Disk_mirage.Make(Block)(Io_page)

let apply common =
  if common.Common.debug
  then Logging.destination := (fun s -> Printf.fprintf stderr "%s\n" s);
  if common.Common.dummy
  then (module Disk_dummy: S.DISK)
  else (module Disk_mirage_unix: S.DISK)

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

let table_of_pv pv = add_prefix pv.Pv.name [
  [ "name"; pv.Pv.name; ];
  [ "id"; Uuid.to_string pv.Pv.id; ];
  [ "stored_device"; pv.Pv.stored_device ];
  [ "real_device"; pv.Pv.real_device ];
  [ "status"; String.concat ", " (List.map Pv.Status.to_string pv.Pv.status) ];
  [ "size_in_sectors"; Int64.to_string pv.Pv.size_in_sectors ];
  [ "pe_start"; Int64.to_string pv.Pv.pe_start ];
  [ "pe_count"; Int64.to_string pv.Pv.pe_count; ]
] @ (table_of_pv_header (pv.Pv.name ^ "/label") pv.Pv.label.Label.pv_header)

let table_of_lv lv = add_prefix lv.Lv.name [
  [ "name"; lv.Lv.name; ];
  [ "id"; Uuid.to_string lv.Lv.id; ];
  [ "tags"; String.concat ", " (List.map Tag.to_string lv.Lv.tags) ];
  [ "status"; String.concat ", " (List.map Lv.Status.to_string lv.Lv.status) ];
  [ "segments"; string_of_int (List.length lv.Lv.segments) ];
]

let table_of_vg vg =
  let pvs = List.flatten (List.map table_of_pv vg.Vg.pvs) in
  let lvs = List.flatten (List.map table_of_lv vg.Vg.lvs) in [
  [ "name"; vg.Vg.name ];
  [ "id"; Uuid.to_string vg.Vg.id ];
  [ "status"; String.concat ", " (List.map Vg.Status.to_string vg.Vg.status) ];
  [ "extent_size"; Int64.to_string vg.Vg.extent_size ];
  [ "max_lv"; string_of_int vg.Vg.max_lv ];
  [ "max_pv"; string_of_int vg.Vg.max_pv ];
] @ pvs @ lvs @ [
  [ "free_space"; Int64.to_string (Pv.Allocator.size vg.Vg.free_space) ];
]

let read common filename =
  let module Disk = (val apply common: S.DISK) in
  let module Vg_IO = Vg.Make(Disk) in
  try
    let filename = require "filename" filename in
    let t =
      Vg_IO.read [ filename ] >>|= fun vg ->
      return vg in
    let vg = Lwt_main.run t in
    Common.print_table [ "key"; "value" ] (table_of_vg vg);
    `Ok ()
  with
    | Failure x ->
      `Error(true, x)

let format common filename vgname pvname =
  let module Disk = (val apply common: S.DISK) in
  let module Vg_IO = Vg.Make(Disk) in
  try
    let filename = require "filename" filename in
    let t =
      Vg_IO.format vgname [ filename, pvname ] >>|= fun () ->
      return () in
    Lwt_main.run t;
    `Ok ()
  with
    | Failure x ->
      `Error(true, x)

let map common filename lvname =
  let module Disk = (val apply common: S.DISK) in
  let module Vg_IO = Vg.Make(Disk) in
  try
    let filename = require "filename" filename in
    let t =
      Vg_IO.read [ filename ] >>|= fun vg ->
      let lv = List.find (fun lv -> lv.Lv.name = lvname) vg.Vg.lvs in
      List.iter (fun seg ->
        Printf.printf "start %Ld, count %Ld %s\n" seg.Lv.Segment.start_extent seg.Lv.Segment.extent_count
          (match seg.Lv.Segment.cls with
           | Lv.Segment.Linear x ->
             Printf.sprintf "from %s starting at %Ld" x.Lv.Linear.name x.Lv.Linear.start_extent
           | Lv.Segment.Striped _ -> "striped")
      ) lv.Lv.segments;
      return () in
    Lwt_main.run t;
    `Ok ()
  with
    | Failure x ->
      `Error(true, x)


let update_vg common filename f =
  let module Disk = (val apply common: S.DISK) in
  let module Vg_IO = Vg.Make(Disk) in
  try
    let filename = require "filename" filename in
    let t =
      Vg_IO.read [ filename ] >>|= fun vg ->
      f vg >>*= fun vg ->
      Vg_IO.write vg >>|= fun _ ->
      return () in
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
  update_vg common filename (fun vg -> Vg.add_tag vg lvname (Tag.of_string tag))

let remove_tag common filename lvname tag =
  update_vg common filename (fun vg -> Vg.remove_tag vg lvname (Tag.of_string tag))
