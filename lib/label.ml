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

(** Physical Volume module *)

open Absty
open IO

let crc_pos = 8 + 8
let do_crc_from = crc_pos + 4

let label_type            = "LVM2 001"
let journalled_label_type = "JOUR 001"

module Label_header = struct
  type t = {
    id : string; (* 8 bytes, equal to label_id in Constants *)
    sector : int64;
    crc : int32;
    offset : int32;
    ty : string (* 8 bytes, equal to "LVM2 001" - label_type*)
  } with sexp

  let equals a b =
    a.id = b.id
    && (a.sector = b.sector)
    && (a.crc = b.crc)
    && (a.offset = b.offset)
    && (a.ty = b.ty)

  let create magic = {
    id=Constants.label_id;
    sector=1L;
    crc=0l;
    offset=32l;
    ty=(if magic = `Lvm then label_type else journalled_label_type);
  }
    
  let unmarshal b0 =
    let id = Cstruct.(to_string (sub b0 0 8)) in
    let sector_xl = Cstruct.LE.get_uint64 b0 8 in
    let crc_xl = Cstruct.LE.get_uint32 b0 crc_pos in
    let offset = Cstruct.LE.get_uint32 b0 20 in
    let ty = Cstruct.(to_string (sub b0 24 8)) in
    let crc_check = Cstruct.sub b0 20 (Constants.label_size - 20) in
    let calculated_crc = Crc.crc crc_check in
    if calculated_crc <> crc_xl
    then `Error (Printf.sprintf "Label_header: bad checksum, expected %08lx, got %08lx" calculated_crc crc_xl)
    else `Ok ({id=id;
     sector=sector_xl;
     crc=crc_xl;
     offset=offset;
     ty=ty}, Cstruct.shift b0 32)

  let marshal label buf =
    assert(label.offset=32l);
    Cstruct.blit_from_string label.id 0 buf 0 8;
    Cstruct.LE.set_uint64 buf 8 label.sector;
    Cstruct.LE.set_uint32 buf 16 0l;
    Cstruct.LE.set_uint32 buf 20 label.offset;
    Cstruct.blit_from_string label.ty 0 buf 24 8;
    Cstruct.shift buf 32

  let to_string t = Sexplib.Sexp.to_string_hum (sexp_of_t t)

  include Result
end

module Location = struct
  type t = {
    offset : int64;
    size : int64;
  } with sexp
end

module Pv_header = struct
  type t = {
    id : Uuid.t;
    device_size : int64;
    extents: Location.t list;
    metadata_areas: Location.t list;
  } with sexp

  let equals a b =
    a.id = b.id
    && (a.device_size = b.device_size)
    && (a.extents = b.extents)
    && (a.metadata_areas = b.metadata_areas)

  let create id device_size mda_start mda_size = {
    id; device_size;
    extents=[{Location.offset=(Int64.add mda_start mda_size); size=0L}];
    metadata_areas=[{Location.offset=mda_start; size=mda_size}];
  }

  let unmarshal b =
    let open Uuid in
    unmarshal b >>= fun (id, b) ->
    let device_size = Cstruct.LE.get_uint64 b 0 in
    let b = Cstruct.shift b 8 in
    let rec do_disk_locn b acc =
      let offset = Cstruct.LE.get_uint64 b 0 in
      let b = Cstruct.shift b 8 in
      if offset=0L 
      then (List.rev acc,Cstruct.shift b 8) 
      else
        let size = Cstruct.LE.get_uint64 b 0 in
        let b = Cstruct.shift b 8 in
        do_disk_locn b ({Location.offset; size}::acc)
    in 
    let extents,b = do_disk_locn b [] in
    let metadata_areas,b = do_disk_locn b [] in
    return ({ id; device_size; extents; metadata_areas }, b)

  let marshal t buf =
    let buf = Uuid.marshal t.id buf in
    Cstruct.LE.set_uint64 buf 0 t.device_size;
    let buf = Cstruct.shift buf 8 in
    
    let do_disk_locn buf l =
      let buf = List.fold_left (fun buf e ->
        Cstruct.LE.set_uint64 buf 0 e.Location.offset;
        Cstruct.LE.set_uint64 buf 8 e.Location.size;
        Cstruct.shift buf 16) buf l in
      Cstruct.LE.set_uint64 buf 0 0L;
      Cstruct.LE.set_uint64 buf 8 0L;
      Cstruct.shift buf 16 in
    
    let buf = do_disk_locn buf t.extents in
    let buf = do_disk_locn buf t.metadata_areas in
    buf

  let to_string t = Sexplib.Sexp.to_string_hum (sexp_of_t t)
 
  include Result
end

type t = {
  label_header : Label_header.t;
  pv_header : Pv_header.t;
} with sexp

let equals a b =
     (Label_header.equals a.label_header b.label_header)
  && (Pv_header.equals a.pv_header b.pv_header)

let sizeof = Constants.sector_size

let marshal t buf =
  let buf' = Label_header.marshal t.label_header buf in
  assert(t.label_header.Label_header.offset=32l);
  let buf' = Pv_header.marshal t.pv_header buf' in
  (* Now calc CRC *)
  let crc = Crc.crc (Cstruct.sub buf do_crc_from (Constants.label_size - do_crc_from)) in
  Cstruct.LE.set_uint32 buf crc_pos crc;
  buf'

let unmarshal buf =
  let open Label_header in
  let rec find n =
    if n > 3
    then `Error "No PV label found in any of the first 4 sectors"
    else begin
      let b = Cstruct.shift buf (n * Constants.sector_size) in
      if Cstruct.(to_string (sub b 0 8)) = Constants.label_id then begin
        unmarshal b >>= fun (lh, _) ->
        return (lh, b)
      end else find (n + 1)
    end in
  find 0 >>= fun (label, buf) ->
  let buf = Cstruct.shift buf (Int32.to_int label.Label_header.offset) in
  let open Pv_header in
  unmarshal buf >>= fun (pvh, buf) ->
  return ({
    label_header = label;
    pv_header = pvh; }, buf)

include Result

let get_metadata_locations label = 
  label.pv_header.Pv_header.metadata_areas

let create ?(magic = `Lvm) id size mda_start mda_size =
  let label = Label_header.create magic in
  let pvh = Pv_header.create id size mda_start mda_size in
  { label_header = label;
    pv_header = pvh }

let to_string label = Sexplib.Sexp.to_string_hum (sexp_of_t label)

module Make(Block: S.BLOCK) = struct
module B = UnalignedBlock.Make(Block)

let read device =
  let open IO in
  B.read device 0L Constants.label_scan_size >>= fun buf ->
  let open IO.FromResult in
  unmarshal buf >>= fun (t, _) ->
  let open IO in
  return t
      
let write device t =
  let buf = Cstruct.create 512 in
  Utils.zero buf;
  let _ = marshal t buf in
  
  let pos = Int64.mul t.label_header.Label_header.sector (Int64.of_int Constants.sector_size) in
  B.write device pos buf
end
