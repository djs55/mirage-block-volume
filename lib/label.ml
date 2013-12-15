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
open Lvmmarshal

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
    let id,b = unmarshal_string 8 b0 in
    let sector_xl,b = unmarshal_uint64 b in
    let crc_xl,b = unmarshal_uint32 b in
    let offset,b = unmarshal_uint32 b in
    let ty,b = unmarshal_string 8 b in
    let crc_check = String.sub (fst b0) (20 + snd b0) (Constants.label_size - 20) in (* wtf? *)
    let calculated_crc = Crc.crc crc_check in
    if calculated_crc <> crc_xl
    then `Error (Printf.sprintf "Label_header: bad checksum, expected %08lx, got %08lx" calculated_crc crc_xl)
    else `Ok ({id=id;
     sector=sector_xl;
     crc=crc_xl;
     offset=offset;
     ty=ty}, b)

  let marshal label buf =
    assert(label.offset=32l);
    
    let buf = marshal_string buf label.id in
    let buf = marshal_int64  buf label.sector in
  
    assert(snd buf = crc_pos); (* fill in later *)
    let buf = marshal_int32  buf 0l in
    
    let buf = marshal_int32  buf label.offset in
    let buf = marshal_string buf label.ty in
    
    assert(snd buf = 32);
    buf

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
    pvh_id : Lvm_uuid.t;
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
    let open Lvm_uuid in
    unmarshal b >>= fun (id, b) ->
    let size,b = unmarshal_uint64 b in
    let rec do_disk_locn b acc =
      let offset,b = unmarshal_uint64 b in
      if offset=0L 
      then (List.rev acc,skip 8 b) 
      else 
        let size,b = unmarshal_uint64 b in
        do_disk_locn b ({dl_offset=offset; dl_size=size}::acc)
    in 
    let disk_areas,b = do_disk_locn b [] in
    let disk_areas2,b = do_disk_locn b [] in
    return ({ pvh_id=id;
      pvh_device_size=size;
      pvh_extents=disk_areas;
      pvh_metadata_areas=disk_areas2},b)

  let marshal t buf =
    let buf = Lvm_uuid.marshal t.pvh_id buf in
    let buf = marshal_int64 buf t.pvh_device_size in
    
    let do_disk_locn buf l =
      let buf = List.fold_left (fun buf e ->
        let buf = marshal_int64 buf e.dl_offset in
        marshal_int64 buf e.dl_size) buf l
      in
      marshal_int64 (marshal_int64 buf 0L) 0L
    in
    
    let buf = do_disk_locn buf t.pvh_extents in
    let buf = do_disk_locn buf t.pvh_metadata_areas in
    buf

  let to_string t =
    let disk_area_list_to_ascii l =
      (String.concat "," (List.map (fun da -> Printf.sprintf "{offset=%Ld,size=%Ld}" da.dl_offset da.dl_size) l)) in  
    Printf.sprintf "pvh_id: %s\npvh_device_size: %Ld\npvh_areas1: %s\npvh_areas2: %s\n"
      (Lvm_uuid.to_string t.pvh_id) t.pvh_device_size 
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
  let buf = Label_header.marshal t.label_header buf in
  assert(t.label_header.Label_header.offset=32l);
  assert(snd buf = 32);
  let buf = Pv_header.marshal t.pv_header buf in
  (* Now calc CRC *)
  let crc = Crc.crc (String.sub (fst buf) do_crc_from (Constants.label_size - do_crc_from)) in
  ignore(marshal_int32 (fst buf, crc_pos) crc);
  buf

let unmarshal (buf, ofs) =
  let open Label_header in
  let rec find n =
    if n > 3
    then `Error "No PV label found in any of the first 4 sectors"
    else begin
      let b = (buf,ofs + n*Constants.sector_size) in
      let (s,b') = unmarshal_string 8 b in
      if s=Constants.label_id
      then
        unmarshal b >>= fun (lh, _) ->
        return (lh, b)
      else find (n + 1)
    end in
  find 0 >>= fun (label, buf) ->
  let buf = skip (Int32.to_int label.Label_header.offset) buf in
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
  let buf = get_label device in
  unmarshal (buf, 0) >>= fun (t, _) ->
  return { t with device }
      
let write t =
  let buf = String.make 512 '\000' in
  let _ = marshal t (buf, 0) in
  
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
