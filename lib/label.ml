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
open Device
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
    if calculated_crc <> crc_xl then
      failwith "Bad checksum in PV Label";
    {id=id;
     sector=sector_xl;
     crc=crc_xl;
     offset=offset;
     ty=ty}

  let marshal label =
    let header = (String.make Constants.sector_size '\000', 0) in (* label header is 1 sector long *)
    assert(label.offset=32l);
    
    let header = marshal_string header label.id in
    let header = marshal_int64  header label.sector in
  
    assert(snd header = crc_pos); (* fill in later *)
    let header = marshal_int32  header 0l in
    
    let header = marshal_int32  header label.offset in
    let header = marshal_string header label.ty in
    
    assert(snd header = 32);
    header

  let to_string t =
    Printf.sprintf "id: %s\nsector: %Ld\ncrc: %ld\noffset: %ld\nty: %s\n"
      t.id t.sector t.crc t.offset t.ty
      
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
    let id,b = unmarshal_string 32 b in
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
    { pvh_id=Lvm_uuid.unmarshal id;
      pvh_device_size=size;
      pvh_extents=disk_areas;
      pvh_metadata_areas=disk_areas2},b

  let marshal t buf =
    let buf = marshal_string buf (Lvm_uuid.marshal t.pvh_id) in
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

end

type t = {
  device : string;
  label_header : Label_header.t;
  pv_header : Pv_header.t;
} with rpc 

      
let find_label dev =
  let buf = get_label dev in
  let rec find n =
    if n>3 then failwith "No label found" else begin
      let b = (buf,n*Constants.sector_size) in
      let (s,b') = unmarshal_string 8 b in
      Printf.fprintf stderr "String='%s' (looking for %s)\n" s Constants.label_id;
      if s=Constants.label_id then (Label_header.unmarshal b,b) else find (n+1)
    end
  in find 0
    

let write_label_and_pv_header l =
  let label = l.label_header in
  let pvh = l.pv_header in
  let dev = l.device in

  let header = Label_header.marshal label in
  assert(label.Label_header.offset=32l);
    
  assert(snd header = 32);

  debug "write_label_and_pv_header:\nPV header:\n%s" (Pv_header.to_string pvh);

  let header = Pv_header.marshal pvh header in
    
  (* Now calc CRC *)
  let crc = Crc.crc (String.sub (fst header) do_crc_from (Constants.label_size - do_crc_from)) in
  ignore(marshal_int32 (fst header, crc_pos) crc);
  
  let pos = Int64.mul label.Label_header.sector (Int64.of_int Constants.sector_size) in
  Device.put_label dev pos (fst header)

let get_metadata_locations label = 
  label.pv_header.Pv_header.pvh_metadata_areas

let get_pv_id label =
  label.pv_header.Pv_header.pvh_id

let get_device label = 
  label.device

let find device =
  let label,b = find_label device in
  let b = skip (Int32.to_int label.Label_header.offset) b in
  let pvh,b' = Pv_header.unmarshal b in    
  { device = device;
    label_header = label;
    pv_header = pvh; }
      
let create device id size extents_start extents_size mda_start mda_size =
  let label = Label_header.create () in
  let pvh = Pv_header.create id size mda_start mda_size in
  { device = device;
    label_header = label;
    pv_header = pvh }

let to_ascii label =
  Printf.sprintf "Label header:\n%s\nPV Header:\n%s\n" 
    (Label_header.to_string label.label_header)
    (Pv_header.to_string label.pv_header)
