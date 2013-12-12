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


open Absty
open Logging

(** Start with the meta-metadata - this is how we actually locate the 
    metadata on disk. It's a bit backwards, because a PV is part of a 
    volume group, but it's the PV that contains the volume group info *)

open Device
open Lvmmarshal

  let mda_header_size = Constants.sector_size

  type mda_raw_locn = {
    mrl_offset: int64;
    mrl_size: int64;
    mrl_checksum: int32;
    mrl_filler: int32;
  }

  and mda_header = {
    mdah_checksum : int32;
    mdah_magic : string;
    mdah_version : int32;
    mdah_start: int64;
    mdah_size: int64;
    mdah_raw_locns : mda_raw_locn list;
  } with rpc

  let unmarshal_mda_header device location =
    let buf = get_mda_header device location.Label.dl_offset mda_header_size in 
    let checksum,b = unmarshal_uint32 (buf,0) in
    let magic,b = unmarshal_string 16 b in
    let version,b = unmarshal_uint32 b in
    let start,b = unmarshal_uint64 b in
    let size,b = unmarshal_uint64 b in
    let rec read_raw_locns b acc =
      let offset,b = unmarshal_uint64 b in
      let size,b = unmarshal_uint64 b in
      let checksum,b = unmarshal_uint32 b in
      let filler,b = unmarshal_uint32 b in
      if (offset=0L) 
      then (List.rev acc),b
      else 
	read_raw_locns b ({mrl_offset=offset;mrl_size=size;mrl_checksum=checksum;mrl_filler=filler}::acc)
    in
    let raw_locns,b = read_raw_locns b [] in
    let crc_to_check = String.sub buf 4 (mda_header_size - 4) in
    let crc = Crc.crc crc_to_check in
    if crc <> checksum then
      failwith "Bad checksum in MDA header";
    {mdah_checksum=checksum;
     mdah_magic=magic;
     mdah_version=version;
     mdah_start=start;
     mdah_size=size;
     mdah_raw_locns=raw_locns}

  let to_ascii mdah =
    let rl2ascii r = Printf.sprintf "{offset:%Ld,size:%Ld,checksum:%ld,filler:%ld}" r.mrl_offset r.mrl_size r.mrl_checksum r.mrl_filler in
    Printf.sprintf "checksum: %ld\nmagic: %s\nversion: %ld\nstart: %Ld\nsize: %Ld\nraw_locns:[%s]\n"
      mdah.mdah_checksum mdah.mdah_magic mdah.mdah_version mdah.mdah_start mdah.mdah_size (String.concat "," (List.map rl2ascii mdah.mdah_raw_locns))
          
  let write_mda_header mdah device  =
    debug "Writing MDA header";
    debug "Writing: %s" (to_ascii mdah);
    let realheader = (String.make mda_header_size '\000', 0) in (* Mda header is 1 sector long *)
    let header = marshal_int32 realheader 0l in (* Write the checksum later *)
    let header = marshal_string header mdah.mdah_magic in
    let header = marshal_int32 header mdah.mdah_version in
    let header = marshal_int64 header mdah.mdah_start in
    let header = marshal_int64 header mdah.mdah_size in
    let write_raw_locn header locn =
      let header = marshal_int64 header locn.mrl_offset in
      let header = marshal_int64 header locn.mrl_size in
      let header = marshal_int32 header locn.mrl_checksum in
      let header = marshal_int32 header locn.mrl_filler in
      header
    in
    let header = List.fold_left write_raw_locn header mdah.mdah_raw_locns in
    let header = write_raw_locn header {mrl_offset=0L; mrl_size=0L; mrl_checksum=0l; mrl_filler=0l} in
    let crcable = String.sub (fst realheader) 4 (mda_header_size - 4) in
    let crc = Crc.crc crcable in
    let _ = marshal_int32 realheader crc in

    let header = String.sub (fst header) 0 mda_header_size in   
    put_mda_header device mdah.mdah_start header

let read_md dev mdah n =
	let locn = List.nth mdah.mdah_raw_locns n in
	let firstbit, secondbit =
		if Int64.add locn.mrl_offset locn.mrl_size > mdah.mdah_size
		then
			let firstbit = Int64.(to_int (sub mdah.mdah_size locn.mrl_offset)) in
			firstbit, Int64.to_int locn.mrl_size - firstbit
		else Int64.to_int locn.mrl_size, 0 in
	let md = Device.get_md dev (Int64.add mdah.mdah_start locn.mrl_offset) (Int64.add mdah.mdah_start 512L) firstbit secondbit in
	let checksum = Crc.crc md in
	if checksum <> locn.mrl_checksum then
		Printf.fprintf stderr "Checksum invalid in metadata: Found %lx, expecting %lx\n" checksum locn.mrl_checksum;
	md
      
  let write_md device mdah md =
    (* Find the current raw location of the metadata, assuming there's only one copy *)
    let current = List.hd mdah.mdah_raw_locns in
    
    (* Find the new place to write (as an offset from the position of the metadata area)
     * LVM always rounds up to the next sector, so we'll do the same. *)
    let newpos = Utils.int64_round_up (Int64.add current.mrl_offset current.mrl_size) 512L in

    (* Check if we've gone outside the mda *)
    let newpos = 
      if newpos >= mdah.mdah_size then
	(Int64.add 512L (Int64.sub newpos mdah.mdah_size))
      else 
	newpos
    in

    (* Add on the position of the metadata area *)
    let absnewpos = Int64.add newpos mdah.mdah_start in

    let size = String.length md in
    let firstbit, secondbit =
      if Int64.add newpos (Int64.of_int size) > mdah.mdah_size
      then
        let firstbit = Int64.to_int (Int64.sub mdah.mdah_size newpos) in
        let secondbit = size - firstbit in
        firstbit, secondbit
      else
        size, 0 in
    let firstbitstr = String.sub md 0 firstbit in
    let secondbitstr = String.sub md firstbit secondbit in

    Device.put_md device absnewpos (Int64.add mdah.mdah_start 512L) firstbitstr secondbitstr;

    (* Now we have to update the crc and pointer to the metadata *)
    
    let checksum = Crc.crc md in
    let new_raw_locn = {
      mrl_offset=newpos;
      mrl_size=Int64.of_int size;
      mrl_checksum=checksum;
      mrl_filler=0l;
    } in

    let mdah = {mdah with mdah_raw_locns=[new_raw_locn]} in
    write_mda_header mdah device;
    mdah


  let create_blank () =
    let mda_raw_locn = {
      mrl_offset = 512L;
      mrl_size = 0L;
      mrl_checksum = 0l;
      mrl_filler=0l;
    } in
    let mda_header = {
      mdah_checksum = 0l;
      mdah_magic = Constants.fmtt_magic;
      mdah_version = 1l;
      mdah_start = Constants.mdah_start;
      mdah_size = Constants.mdah_size;
      mdah_raw_locns = [mda_raw_locn]
    } in
    mda_header
