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

type label_header = {
  id : string; (* 8 bytes, equal to label_id in Constants *)
  sector : int64;
  crc : int32;
  offset : int32;
  ty : string (* 8 bytes, equal to "LVM2 001" - Constants.label_type*)
}
      
and disk_locn = {
  dl_offset : int64;
  dl_size : int64;
}

and pv_header = {
  pvh_id : Lvm_uuid.t;
  pvh_device_size : int64;
  pvh_extents: disk_locn list;
  pvh_metadata_areas: disk_locn list;
}

and t = {
  device : string;
  label_header : label_header;
  pv_header : pv_header;
} with rpc 

let unmarshal_header b0 =
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
      
let find_label dev =
  let buf = get_label dev in
  let rec find n =
    if n>3 then failwith "No label found" else begin
      let b = (buf,n*Constants.sector_size) in
      let (s,b') = unmarshal_string 8 b in
      Printf.fprintf stderr "String='%s' (looking for %s)\n" s Constants.label_id;
      if s=Constants.label_id then (unmarshal_header b,b) else find (n+1)
    end
  in find 0
    
let label_header_to_ascii label =
  Printf.sprintf "id: %s\nsector: %Ld\ncrc: %ld\noffset: %ld\nty: %s\n" label.id label.sector label.crc label.offset label.ty
      
let read_pv_header label b =
  let b = skip (Int32.to_int label.offset) b in
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

let pvh_to_ascii pvh =
  let disk_area_list_to_ascii l =
    (String.concat "," (List.map (fun da -> Printf.sprintf "{offset=%Ld,size=%Ld}" da.dl_offset da.dl_size) l)) in  
  Printf.sprintf "pvh_id: %s\npvh_device_size: %Ld\npvh_areas1: %s\npvh_areas2: %s\n"
    (Lvm_uuid.to_string pvh.pvh_id) pvh.pvh_device_size 
    (disk_area_list_to_ascii pvh.pvh_extents)
    (disk_area_list_to_ascii pvh.pvh_metadata_areas)

let write_label_and_pv_header l =
  let label = l.label_header in
  let pvh = l.pv_header in
  let dev = l.device in

  let header = (String.make Constants.sector_size '\000', 0) in (* label header is 1 sector long *)
  let pos = Int64.mul label.sector (Int64.of_int Constants.sector_size) in
  assert(label.offset=32l);
    
  let header = marshal_string header label.id in
  let header = marshal_int64  header label.sector in
    
  let crc_pos = header in (* Set later! *)
  let header = marshal_int32  header 0l in
    
  let (do_crc_str,do_crc_from) = header in
  let header = marshal_int32  header label.offset in
  let header = marshal_string header label.ty in
    
  assert(snd header = 32);

  debug "write_label_and_pv_header:\nPV header:\n%s" (pvh_to_ascii pvh);

  (* PV header *)
  let header = marshal_string header (Lvm_uuid.marshal pvh.pvh_id) in   let header = marshal_int64 header pvh.pvh_device_size in
    
  let do_disk_locn header l =
    let header = List.fold_left (fun header e ->
      let header = marshal_int64 header e.dl_offset in
      marshal_int64 header e.dl_size) header l
    in
    marshal_int64 (marshal_int64 header 0L) 0L
  in
    
  let header = do_disk_locn header pvh.pvh_extents in
  let header = do_disk_locn header pvh.pvh_metadata_areas in
    
  (* Now calc CRC *)
  let crc = Crc.crc (String.sub do_crc_str do_crc_from (Constants.label_size - do_crc_from)) in
  ignore(marshal_int32 crc_pos crc);
  
  Device.put_label dev pos (fst header)

let get_metadata_locations label = 
  label.pv_header.pvh_metadata_areas

let get_pv_id label =
  label.pv_header.pvh_id

let get_device label = 
  label.device

let find device =
  let label,b = find_label device in
  let pvh,b' = read_pv_header label b in    
  { device = device;
    label_header = label;
    pv_header = pvh; }
      
let create device id size extents_start extents_size mda_start mda_size =
  let label = {
    id=Constants.label_id;
    sector=1L;
    crc=0l;
    offset=32l;
    ty=Constants.label_type;
  } in
  let pvh = {
    pvh_id=id;
    pvh_device_size=size;
    pvh_extents=[{dl_offset=(Int64.add mda_start mda_size); dl_size=0L}];
    pvh_metadata_areas=[{dl_offset=mda_start; dl_size=mda_size}];
  } in
  { device = device;
    label_header = label;
    pv_header = pvh }

let to_ascii label =
  Printf.sprintf "Label header:\n%s\nPV Header:\n%s\n" 
    (label_header_to_ascii label.label_header)
    (pvh_to_ascii label.pv_header)
