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
open IO

let crc_pos = 8 + 8
let do_crc_from = crc_pos + 4

module Label_header = struct
  type t = {
    id : string; (* 8 bytes, equal to label_id in Constants *)
    sector : int64;
    crc : int32;
    offset : int32;
    ty : string (* 8 bytes, equal to "LVM2 001" - Constants.label_type*)
  } with rpc

  let equals a b =
    a.id = b.id
    && (a.sector = b.sector)
    && (a.crc = b.crc)
    && (a.offset = b.offset)
    && (a.ty = b.ty)

  let create () = {
    id=Constants.label_id;
    sector=1L;
    crc=0l;
    offset=32l;
    ty=Constants.label_type;
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

  let to_string t =
    Printf.sprintf "id: %s\nsector: %Ld\ncrc: %ld\noffset: %ld\nty: %s\n"
      t.id t.sector t.crc t.offset t.ty

  include Result
end

type disk_locn = {
  dl_offset : int64;
  dl_size : int64;
} with rpc

module Pv_header = struct
  type t = {
    pvh_id : Uuid.t;
    pvh_device_size : int64;
    pvh_extents: disk_locn list;
    pvh_metadata_areas: disk_locn list;
  } with rpc

  let equals a b =
    a.pvh_id = b.pvh_id
    && (a.pvh_device_size = b.pvh_device_size)
    && (a.pvh_extents = b.pvh_extents)
    && (a.pvh_metadata_areas = b.pvh_metadata_areas)

  let create id size mda_start mda_size = {
    pvh_id=id;
    pvh_device_size=size;
    pvh_extents=[{dl_offset=(Int64.add mda_start mda_size); dl_size=0L}];
    pvh_metadata_areas=[{dl_offset=mda_start; dl_size=mda_size}];
  }

  let unmarshal b =
    let open Uuid in
    unmarshal b >>= fun (id, b) ->
    let size = Cstruct.LE.get_uint64 b 0 in
    let b = Cstruct.shift b 8 in
    let rec do_disk_locn b acc =
      let offset = Cstruct.LE.get_uint64 b 0 in
      let b = Cstruct.shift b 8 in
      if offset=0L 
      then (List.rev acc,Cstruct.shift b 8) 
      else
        let size = Cstruct.LE.get_uint64 b 0 in
        let b = Cstruct.shift b 8 in
        do_disk_locn b ({dl_offset=offset; dl_size=size}::acc)
    in 
    let disk_areas,b = do_disk_locn b [] in
    let disk_areas2,b = do_disk_locn b [] in
    return ({ pvh_id=id;
      pvh_device_size=size;
      pvh_extents=disk_areas;
      pvh_metadata_areas=disk_areas2},b)

  let marshal t buf =
    let buf = Uuid.marshal t.pvh_id buf in
    Cstruct.LE.set_uint64 buf 0 t.pvh_device_size;
    let buf = Cstruct.shift buf 8 in
    
    let do_disk_locn buf l =
      let buf = List.fold_left (fun buf e ->
        Cstruct.LE.set_uint64 buf 0 e.dl_offset;
        Cstruct.LE.set_uint64 buf 8 e.dl_size;
        Cstruct.shift buf 16) buf l in
      Cstruct.LE.set_uint64 buf 0 0L;
      Cstruct.LE.set_uint64 buf 8 0L;
      Cstruct.shift buf 16 in
    
    let buf = do_disk_locn buf t.pvh_extents in
    let buf = do_disk_locn buf t.pvh_metadata_areas in
    buf

  let to_string t =
    let disk_area_list_to_ascii l =
      (String.concat "," (List.map (fun da -> Printf.sprintf "{offset=%Ld,size=%Ld}" da.dl_offset da.dl_size) l)) in  
    Printf.sprintf "pvh_id: %s\npvh_device_size: %Ld\npvh_areas1: %s\npvh_areas2: %s\n"
      (Uuid.to_string t.pvh_id) t.pvh_device_size 
      (disk_area_list_to_ascii t.pvh_extents)
      (disk_area_list_to_ascii t.pvh_metadata_areas)

  include Result
end

type t = {
  device : string;
  label_header : Label_header.t;
  pv_header : Pv_header.t;
} with rpc 

let equals a b =
  a.device = b.device
  && (Label_header.equals a.label_header b.label_header)
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
  return ({ device = "";
    label_header = label;
    pv_header = pvh; }, buf)

include Result

let get_metadata_locations label = 
  label.pv_header.Pv_header.pvh_metadata_areas

let get_pv_id label =
  label.pv_header.Pv_header.pvh_id

let get_device label = 
  label.device

let read device =
  let open IO in
  get_label device >>= fun buf ->
  let open IO.FromResult in
  unmarshal buf >>= fun (t, _) ->
  let open IO in
  return { t with device }
      
let write t =
  let buf = Cstruct.create 512 in
  Utils.zero buf;
  let _ = marshal t buf in
  
  let pos = Int64.mul t.label_header.Label_header.sector (Int64.of_int Constants.sector_size) in
  IO.put_label t.device pos buf

let create device id size mda_start mda_size =
  let label = Label_header.create () in
  let pvh = Pv_header.create id size mda_start mda_size in
  { device = device;
    label_header = label;
    pv_header = pvh }

let to_string label =
  Printf.sprintf "Label header:\n%s\nPV Header:\n%s\n" 
    (Label_header.to_string label.label_header)
    (Pv_header.to_string label.pv_header)
