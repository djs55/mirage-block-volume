(*
 * Copyright (C) 2009-2015 Citrix Systems Inc.
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
open Sexplib.Std

(** Physical Volumes:
    Note we start with a physical volume and then use it to discover
    the volume group. *)
open Lvm_internal
open Absty
open Expect

open Result

type error = [
  | `Msg of string
]

type 'a result = ('a, error) Result.result

let open_error = function
  | `Ok x -> `Ok x
  | `Error (`Msg x) -> `Error (`Msg x)

let fail msg = `Error (`Msg msg)

module Status = struct  
  type t = 
    | Allocatable
  with sexp

  let to_string = function
    | Allocatable -> "ALLOCATABLE"

  let of_string = function
    | "ALLOCATABLE" -> return Allocatable
    | x -> fail (Printf.sprintf "Bad PV status string: %s" x)
end

module Name = struct

  type t = string with sexp

  let compare (a: t) (b: t) = compare a b

  let to_string x = x

  let of_string x = `Ok x
end

type t = {
  name : string;
  id : Uuid.t;
  status : Status.t list;
  size_in_sectors : int64;
  pe_start : int64;
  pe_count : int64;
  label : Label.t;  (* The one label for this PV *)
  headers : Metadata.Header.t list; 
} with sexp

let marshal pv b =
  let ofs = ref 0 in
  let bprintf fmt = Printf.kprintf (fun s ->
    let len = String.length s in
    Cstruct.blit_from_string s 0 b !ofs len;
    ofs := !ofs + len
  ) fmt in
  bprintf "\n%s {\nid = \"%s\"\ndevice = \"%s\"\n\n" pv.name (Uuid.to_string pv.id) "/dev/null";
  bprintf "status = [%s]\ndev_size = %Ld\npe_start = %Ld\npe_count = %Ld\n}\n" 
    (String.concat ", " (List.map (o quote Status.to_string) pv.status))
    pv.size_in_sectors pv.pe_start pv.pe_count;
  Cstruct.shift b !ofs

module Make(Block: S.BLOCK) = struct

module Label_IO = Label.Make(Block)
module Metadata_IO = Metadata.Make(Block)
module Header_IO = Metadata.Header.Make(Block)
module B = UnalignedBlock.Make(Block)

let read device name config =
  let open IO.FromResult in
  expect_mapped_string "id" config >>= fun id ->
  Uuid.of_string id >>= fun id ->
  expect_mapped_string "device" config >>= fun _stored_device ->
  map_expected_mapped_array "status" 
    (fun a -> let open Result in expect_string "status" a >>= fun x ->
              Status.of_string x) config >>= fun status ->
  expect_mapped_int "dev_size" config >>= fun size_in_sectors ->
  expect_mapped_int "pe_start" config >>= fun pe_start ->
  expect_mapped_int "pe_count" config >>= fun pe_count ->
  let open IO in
      Label_IO.read device >>= fun label ->
      let mda_locs = Label.get_metadata_locations label in
      Header_IO.read_all device mda_locs >>= fun headers ->
  return { name; id; status; size_in_sectors; pe_start; pe_count; label; headers }

(** Find the metadata area on a device and return the text of the metadata *)
let read_metadata device =
  let open IO in
  Label_IO.read device >>= fun label ->
  let mda_locs = Label.get_metadata_locations label in
  Header_IO.read_all device mda_locs >>= fun mdahs ->
  Metadata_IO.read device (List.hd mdahs) 0 >>= fun mdt ->
  return mdt

let rotate buf n =
  for i = 0 to Cstruct.len buf - 1 do
    let c = Cstruct.get_uint8 buf i in
    Cstruct.set_uint8 buf i ((c + n) mod 256)
  done

let wipe device =
  let open IO in
  Label_IO.read device >>= fun label ->
  let mda_locs = Label.get_metadata_locations label in
  IO.FromResult.all (
    Lwt_list.map_s (fun l ->
      B.read device l.Label.Location.offset (Int64.to_int l.Label.Location.size)
      >>= fun buf ->
      rotate buf 1;
      B.write device l.Label.Location.offset buf
    ) mda_locs
  ) >>= fun (_: unit list) ->
  B.read device 0L Constants.label_scan_size
  >>= fun buf ->
  rotate buf 1;
  B.write device 0L buf

let unwipe device =
  let open IO in
  B.read device 0L Constants.label_scan_size
  >>= fun buf ->
  rotate buf (-1);
  B.write device 0L buf
  >>= fun () ->
  Label_IO.read device >>= fun label ->
  let mda_locs = Label.get_metadata_locations label in
  IO.FromResult.all (
    Lwt_list.map_s (fun l ->
      B.read device l.Label.Location.offset (Int64.to_int l.Label.Location.size)
      >>= fun buf ->
      rotate buf (-1);
      B.write device l.Label.Location.offset buf
    ) mda_locs
  ) >>= fun (_: unit list) ->
  return ()

let format device ?(magic=`Lvm) name =
  let open IO in
  B.get_size device >>= fun size ->
  (* Arbitrarily put the MDA at 4096. We'll have a 10 meg MDA too *)
  let size_in_sectors = Int64.div size (Int64.of_int Constants.sector_size) in
  let mda_pos = Metadata.default_start in
  let mda_len = Metadata.default_size in
  let pe_start_byte = 
    Utils.int64_round_up (Int64.add mda_pos mda_len) Constants.pe_align in
  let pe_start = Int64.(div pe_start_byte (of_int Constants.sector_size)) in
  let pe_count = Int64.(div (sub size pe_start_byte) Constants.extent_size) in
  let mda_len = Int64.sub pe_start_byte mda_pos in
  let id=Uuid.create () in
  let label = Label.create ~magic id size mda_pos mda_len in
  let mda_header = Metadata.Header.create magic in
  Label_IO.write device label >>= fun () ->
  Header_IO.write mda_header device >>= fun () ->
  return { name; id; status=[Status.Allocatable];
           size_in_sectors; pe_start; pe_count; label; headers = [mda_header]; }
end

module Allocator = Allocator.Make(Name)
