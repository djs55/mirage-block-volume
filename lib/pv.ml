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


(** Physical Volumes:
    Note we start with a physical volume and then use it to discover
    the volume group. *)

open Absty
open Logging

open Result

module Status = struct  
  type t = 
    | Allocatable
  with rpc

  let to_string = function
    | Allocatable -> "ALLOCATABLE"

  let of_string = function
    | "ALLOCATABLE" -> return Allocatable
    | x -> fail (Printf.sprintf "Bad PV status string: %s" x)
end

type t = {
  name : string;
  id : Uuid.t;
  dev : string;
  real_device : string; (* Actual device we're reading/writing to/from *)
  status : Status.t list;
  dev_size : int64;
  pe_start : int64;
  pe_count : int64;
  label : Label.t;  (* The one label for this PV *)
  mda_headers : Metadata.Header.t list; 
} with rpc 

let to_buffer b pv =
  let bprintf = Printf.bprintf in
  bprintf b "\n%s {\nid = \"%s\"\ndevice = \"%s\"\n\n" pv.name (Uuid.to_string pv.id) pv.dev;
  bprintf b "status = [%s]\ndev_size = %Ld\npe_start = %Ld\npe_count = %Ld\n}\n" 
    (String.concat ", " (List.map (o quote Status.to_string) pv.status))
    pv.dev_size pv.pe_start pv.pe_count

let of_metadata name config pvdatas =
  let open IO.FromResult in
  expect_mapped_string "id" config >>= fun id ->
  Uuid.of_string id >>= fun id ->
  expect_mapped_string "device" config >>= fun dev ->
  map_expected_mapped_array "status" 
    (fun a -> let open Result in expect_string "status" a >>= fun x ->
              Status.of_string x) config >>= fun status ->
  expect_mapped_int "dev_size" config >>= fun dev_size ->
  expect_mapped_int "pe_start" config >>= fun pe_start ->
  expect_mapped_int "pe_count" config >>= fun pe_count ->
  let open IO in
  ( try 
      let res = List.find (fun (label,mdahs) -> id=Label.get_pv_id label) pvdatas in
      Printf.fprintf stderr "Found cached PV label data\n";
      return res
    with Not_found -> 
      Printf.fprintf stderr "No cached PV data found - loading from device '%s'\n" dev;
      Label.read dev >>= fun label ->
      let mda_locs = Label.get_metadata_locations label in
      Metadata.Header.read_all dev mda_locs >>= fun mdahs ->
      return (label,mdahs)
  ) >>= fun (label, mda_headers) ->
  let real_device = Label.get_device label in
  if real_device <> dev then
    Printf.fprintf stderr "WARNING: PV.device and real_device are not the same";
  return { name; id; dev; real_device; status; dev_size; pe_start; pe_count; label; mda_headers }

(** Find the metadata area on a device and return the text of the metadata *)
let find_metadata device =
  let open IO in
  Label.read device >>= fun label ->
  debug "Label found: \n%s\n" (Label.to_string label);
  let mda_locs = Label.get_metadata_locations label in
  Metadata.Header.read_all device mda_locs >>= fun mdahs ->
  Metadata.read device (List.hd mdahs) 0 >>= fun mdt ->
  return (mdt, (label, mdahs))

let to_string pv =
  let label=pv.label in
  let b=Buffer.create 1000 in
  let label_str=Label.to_string label in
  let mdah_ascii = String.concat "\n" (List.map Metadata.Header.to_string pv.mda_headers) in
  to_buffer b pv;
  Printf.sprintf "Label:\n%s\nMDA Headers:\n%s\n%s\n" 
    label_str mdah_ascii (Buffer.contents b)

let create_new real_device name =
  let open IO in
  IO.get_size real_device >>= fun size ->
  (* Arbitrarily put the MDA at 4096. We'll have a 10 meg MDA too *)
  let dev_size = Int64.div size (Int64.of_int Constants.sector_size) in
  let mda_pos = Metadata.default_start in
  let mda_len = Metadata.default_size in
  let pe_start_byte = 
    Utils.int64_round_up (Int64.add mda_pos mda_len) Constants.pe_align in
  let pe_start = Int64.(div pe_start_byte (of_int Constants.sector_size)) in
  let pe_count = Int64.(div (sub size pe_start_byte) Constants.extent_size) in
  let mda_len = Int64.sub pe_start_byte mda_pos in
  let id=Uuid.create () in
  let label = Label.create real_device id size mda_pos mda_len in
  let mda_header = Metadata.Header.create () in
  Label.write label >>= fun () ->
  Metadata.Header.write mda_header real_device >>= fun () ->
  return { name; id; dev = real_device; real_device; status=[Status.Allocatable]; dev_size;
           pe_start; pe_count; label; mda_headers = [mda_header]; }      
