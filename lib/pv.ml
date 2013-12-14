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


(** Physical Volume module *)

open Absty
open Logging

(** Start with the meta-metadata - this is how we actually locate the 
    metadata on disk. It's a bit backwards, because a PV is part of a 
    volume group, but it's the PV that contains the volume group info *)

open Device
open Lvmmarshal

  (** Here's the actual PV data that's part of the volume group *)
  
type status = 
    | Allocatable
	
and physical_volume = {
  name : string;
  id : Lvm_uuid.t;
  dev : string;
  real_device : string; (* Actual device we're reading/writing to/from *)
  status : status list;
  dev_size : int64;
  pe_start : int64;
  pe_count : int64;
  label : Label.t;  (* The one label for this PV *)
  mda_headers : Metadata.Header.t list; 
} with rpc 

let status_to_string s =
  match s with
    | Allocatable -> "ALLOCATABLE"

open Result

let status_of_string s =
  match s with
    | "ALLOCATABLE" -> return Allocatable
    | x -> fail (Printf.sprintf "Bad PV status string: %s" x)

let write_to_buffer b pv =
  let bprintf = Printf.bprintf in
  bprintf b "\n%s {\nid = \"%s\"\ndevice = \"%s\"\n\n" pv.name (Lvm_uuid.to_string pv.id) pv.dev;
  bprintf b "status = [%s]\ndev_size = %Ld\npe_start = %Ld\npe_count = %Ld\n}\n" 
    (String.concat ", " (List.map (o quote status_to_string) pv.status))
    pv.dev_size pv.pe_start pv.pe_count

let of_metadata name config pvdatas =
  expect_mapped_string "id" config >>= fun id ->
  let id = Lvm_uuid.of_string id in
  expect_mapped_string "device" config >>= fun dev ->
  map_expected_mapped_array "status" 
    (fun a -> expect_string "status" a >>= fun x ->
              status_of_string x) config >>= fun status ->
  expect_mapped_int "dev_size" config >>= fun dev_size ->
  expect_mapped_int "pe_start" config >>= fun pe_start ->
  expect_mapped_int "pe_count" config >>= fun pe_count ->
  let open Label in
  ( try 
      let res = List.find (fun (label,mdahs) -> id=Label.get_pv_id label) pvdatas in
      Printf.fprintf stderr "Found cached PV label data\n";
      return res
    with Not_found -> 
      Printf.fprintf stderr "No cached PV data found - loading from device '%s'\n" dev;
      let open Label in
      read dev >>= fun label ->
      let mda_locs = get_metadata_locations label in
      Metadata.Header.read_all dev mda_locs >>= fun mdahs ->
      return (label,mdahs)
  ) >>= fun (label, mda_headers) ->
  let real_device = Label.get_device label in
  if real_device <> dev then
    Printf.fprintf stderr "WARNING: PV.device and real_device are not the same";
  return { name; id; dev; real_device; status; dev_size; pe_start; pe_count; label; mda_headers }

(** Find the metadata area on a device and return the text of the metadata *)
let find_metadata device =
  let open Label in
  read device >>= fun label ->
  debug "Label found: \n%s\n" (to_string label);
  let mda_locs = get_metadata_locations label in
  Metadata.Header.read_all device mda_locs >>= fun mdahs ->
  let mdt = Metadata.read device (List.hd mdahs) 0 in  
  return (mdt, (label, mdahs))

let human_readable pv =
  let label=pv.label in
  let b=Buffer.create 1000 in
  let label_str=Label.to_string label in
  let mdah_ascii = String.concat "\n" (List.map Metadata.Header.to_string pv.mda_headers) in
  write_to_buffer b pv;
  Printf.sprintf "Label:\n%s\nMDA Headers:\n%s\n%s\n" 
    label_str mdah_ascii (Buffer.contents b)

let create_new dev name =
  let size = Device.get_size dev in
  (* Arbitrarily put the MDA at 4096. We'll have a 10 meg MDA too *)
  let dev_size = Int64.div size (Int64.of_int Constants.sector_size) in
  let mda_pos = Metadata.default_start in
  let mda_len = Metadata.default_size in
  let pe_start_byte = 
    Utils.int64_round_up (Int64.add mda_pos mda_len) Constants.pe_align in
  let pe_start_sector = Int64.div pe_start_byte 
    (Int64.of_int Constants.sector_size) in
  let pe_count = Int64.div (Int64.sub size pe_start_byte) Constants.extent_size in
  let mda_len = Int64.sub pe_start_byte mda_pos in
  let id=Lvm_uuid.create () in
  let label = Label.create dev id size mda_pos mda_len in
  let mda_header = Metadata.Header.create () in
  Label.write label;
  Metadata.Header.write mda_header dev;
  let pv = { name=name;
	     id=id;
	     dev=dev;
	     real_device=dev;
	     status=[Allocatable];
	     dev_size = dev_size;
	     pe_start=pe_start_sector;
	     pe_count=pe_count;
	     label = label;
	     mda_headers = [mda_header]; }
  in
  pv
      
      
