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

let apply common =
  Constants.dummy_mode := common.Common.dummy;
  if common.Common.debug
  then Logging.destination := (fun s -> Printf.fprintf stderr "%s\n" s)

let add_prefix x xs = List.map (function
  | [] -> []
  | y :: ys -> (x ^ "/" ^ y) :: ys
) xs

let table_of_pv pv = add_prefix pv.Pv.name [
  [ "name"; pv.Pv.name; ];
  [ "id"; Uuid.to_string pv.Pv.id; ];
  [ "stored_device"; pv.Pv.stored_device ];
  [ "real_device"; pv.Pv.real_device ];
  [ "status"; String.concat ", " (List.map Pv.Status.to_string pv.Pv.status) ];
  [ "size_in_sectors"; Int64.to_string pv.Pv.size_in_sectors ];
  [ "pe_start"; Int64.to_string pv.Pv.pe_start ];
  [ "pe_count"; Int64.to_string pv.Pv.pe_count; ];
  (*
  [ "label"; "" ];
  [ "headers"; "" ];
  *)
]

let table_of_vg vg =
  let pvs = List.flatten (List.map table_of_pv vg.Vg.pvs) in [
  [ "name"; vg.Vg.name ];
  [ "id"; Uuid.to_string vg.Vg.id ];
  [ "status"; String.concat ", " (List.map Vg.Status.to_string vg.Vg.status) ];
  [ "extent_size"; Int64.to_string vg.Vg.extent_size ];
  [ "max_lv"; string_of_int vg.Vg.max_lv ];
  [ "max_pv"; string_of_int vg.Vg.max_pv ];
] @ pvs @ [
  (*
  [ "lvs"; "" ];
  [ "free_space"; "" ];
  *)
]

let read common filename =
  apply common;
  try
    let filename = require "filename" filename in
    let t =
      Vg.read [ filename ] >>|= fun vg ->
      return vg in
    let vg = Lwt_main.run t in
    Common.print_table [ "key"; "value" ] (table_of_vg vg);
    `Ok ()
  with
    | Failure x ->
      `Error(true, x)

let format common filename vgname pvname =
  apply common;
  try
    let filename = require "filename" filename in
    let t =
      Vg.format vgname [ filename, pvname ] >>|= fun () ->
      return () in
    Lwt_main.run t;
    `Ok ()
  with
    | Failure x ->
      `Error(true, x)

let update_vg filename f =
  try
    let filename = require "filename" filename in
    let t =
      Vg.read [ filename ] >>|= fun vg ->
      f vg >>*= fun vg ->
      Vg.write vg >>|= fun _ ->
      return () in
    Lwt_main.run t;
    `Ok ()
  with
    | Failure x ->
      `Error(true, x)

let create common filename lvname size =
  apply common;
  update_vg filename (fun vg -> Vg.create vg lvname size)

let rename common filename lvname newname =
  apply common;
  update_vg filename (fun vg -> Vg.rename vg lvname newname)

let resize common filename lvname newsize =
  apply common;
  update_vg filename (fun vg -> Vg.resize vg lvname newsize)

let remove common filename lvname =
  apply common;
  update_vg filename (fun vg -> Vg.remove vg lvname)

let add_tag common filename lvname tag =
  apply common;
  update_vg filename (fun vg -> Vg.add_tag vg lvname (Tag.of_string tag))

let remove_tag common filename lvname tag =
  apply common;
  update_vg filename (fun vg -> Vg.remove_tag vg lvname (Tag.of_string tag))