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

open Absty
open Logging

(** Start with the meta-metadata - this is how we actually locate the 
    metadata on disk. It's a bit backwards, because a PV is part of a 
    volume group, but it's the PV that contains the volume group info *)

open IO

let default_start = 4096L
let default_size = Int64.mul 10240L 1024L (* 10 MiB *)

module Header = struct
  let sizeof = Constants.sector_size

  type mda_raw_locn = {
    mrl_offset: int64;
    mrl_size: int64;
    mrl_checksum: int32;
    mrl_filler: int32;
  }

  and t = {
    mdah_checksum : int32;
    mdah_magic : Magic.t;
    mdah_version : int32;
    mdah_start: int64;
    mdah_size: int64;
    mdah_raw_locns : mda_raw_locn list;
  } with sexp

  include Result

  let equals a b =
    (* the checksum is filled in by marshal, and verified by unmarshal *)
    a.mdah_magic = b.mdah_magic
    && (a.mdah_version = b.mdah_version)
    && (a.mdah_start = b.mdah_start)
    && (a.mdah_size = b.mdah_size)
    && (a.mdah_raw_locns = b.mdah_raw_locns)

  let unmarshal buf =
    let open Result in
    let mdah_checksum = Cstruct.LE.get_uint32 buf 0 in
    Magic.unmarshal (Cstruct.sub buf 4 16)
    >>= fun (mdah_magic, _) ->
    let mdah_version = Cstruct.LE.get_uint32 buf 20 in
    let mdah_start = Cstruct.LE.get_uint64 buf 24 in
    let mdah_size = Cstruct.LE.get_uint64 buf 32 in
    let rec read_raw_locns b acc =
      let mrl_offset = Cstruct.LE.get_uint64 b 0 in
      let mrl_size = Cstruct.LE.get_uint64 b 8 in
      let mrl_checksum = Cstruct.LE.get_uint32 b 16 in
      let mrl_filler = Cstruct.LE.get_uint32 b 20 in
      let b = Cstruct.shift b 24 in
      if mrl_offset = 0L
      then List.rev acc, b
      else read_raw_locns b ({ mrl_offset; mrl_size; mrl_checksum; mrl_filler } :: acc) in
    let mdah_raw_locns, b = read_raw_locns (Cstruct.shift buf 40) [] in
    let crc_to_check = Cstruct.sub buf 4 (sizeof - 4) in
    let crc = Crc.crc crc_to_check in
    if crc <> mdah_checksum
    then `Error (Printf.sprintf "Bad checksum in metadata area: expected %08lx, got %08lx" mdah_checksum crc)
    else `Ok ({mdah_checksum; mdah_magic; mdah_version; mdah_start; mdah_size; mdah_raw_locns}, b)

  let to_string mdah = Sexplib.Sexp.to_string_hum (sexp_of_t mdah)

  let marshal mdah buf =
    let _ = Magic.marshal mdah.mdah_magic (Cstruct.shift buf 4) in
    Cstruct.LE.set_uint32 buf 20 mdah.mdah_version;
    Cstruct.LE.set_uint64 buf 24 mdah.mdah_start;
    Cstruct.LE.set_uint64 buf 32 mdah.mdah_size;
    let write_raw_locn buf locn =
      Cstruct.LE.set_uint64 buf 0 locn.mrl_offset;
      Cstruct.LE.set_uint64 buf 8 locn.mrl_size;
      Cstruct.LE.set_uint32 buf 16 locn.mrl_checksum;
      Cstruct.LE.set_uint32 buf 20 locn.mrl_filler;
      Cstruct.shift buf 24 in
    let buf' = List.fold_left write_raw_locn (Cstruct.shift buf 40) mdah.mdah_raw_locns in
    let buf' = write_raw_locn buf' { mrl_offset = 0L; mrl_size = 0L; mrl_checksum = 0l; mrl_filler = 0l } in
    let crcable = Cstruct.sub buf 4 (sizeof - 4) in
    let crc = Crc.crc crcable in
    Cstruct.LE.set_uint32 buf 0 crc;
    buf'

  module Make(Block: S.BLOCK) = struct
  module B = UnalignedBlock.Make(Block)
  let read device location =
    let open IO in
    B.read device location.Label.Location.offset sizeof >>= fun buf ->
    let open IO.FromResult in
    unmarshal buf >>= fun (t, _) ->
    return t

  let read_all device locations =
    let open IO in
    let rec loop acc = function
    | [] -> return (List.rev acc)
    | m :: ms ->
      read device m >>= fun m' ->
      loop (m' :: acc) ms in
    loop [] locations

  let write mdah device  =
    debug "Writing MDA header";
    debug "Writing: %s" (to_string mdah);
    let sector = Cstruct.create sizeof in
    Utils.zero sector;
    let _ = marshal mdah sector in
    B.write device mdah.mdah_start sector
  end

  let create magic =
    let mda_raw_locn = {
      mrl_offset = 512L;
      mrl_size = 0L;
      mrl_checksum = 0l;
      mrl_filler=0l;
    } in
    let mda_header = {
      mdah_checksum = 0l;
      mdah_magic = magic;
      mdah_version = 1l;
      mdah_start = default_start;
      mdah_size = default_size;
      mdah_raw_locns = [mda_raw_locn]
    } in
    mda_header

end

open Header

module Make(Block: S.BLOCK) = struct

module Header_IO = Header.Make(Block)

module B = UnalignedBlock.Make(Block)

let read dev mdah n =
  let locn = List.nth mdah.mdah_raw_locns n in
  let firstbit, secondbit =
    if Int64.add locn.mrl_offset locn.mrl_size > mdah.mdah_size
    then
      let firstbit = Int64.(to_int (sub mdah.mdah_size locn.mrl_offset)) in
      firstbit, Int64.to_int locn.mrl_size - firstbit
    else Int64.to_int locn.mrl_size, 0 in
  let offset = Int64.add mdah.mdah_start locn.mrl_offset in
  let offset' = Int64.add mdah.mdah_start 512L in
  let open IO in
  B.read dev offset firstbit >>= fun buf ->
  B.read dev offset' secondbit >>= fun buf' ->
  let result = Cstruct.create (firstbit + secondbit) in
  Cstruct.blit buf 0 result 0 firstbit;
  Cstruct.blit buf' 0 result firstbit secondbit;
  let checksum = Crc.crc result in
    if checksum <> locn.mrl_checksum then
    warn "Ignoring invalid checksum in metadata: Found %lx, expecting %lx" checksum locn.mrl_checksum;
  return result
      
let write device mdah md =
  (* Find the current raw location of the metadata, assuming there's only one copy *)
  let current = List.hd mdah.mdah_raw_locns in
    
  (* Find the new place to write (as an offset from the position of the metadata area)
     LVM always rounds up to the next sector, so we'll do the same. *)
  let mrl_offset = Utils.int64_round_up (Int64.add current.mrl_offset current.mrl_size) 512L in

  (* Check if we've gone outside the mda *)
  let mrl_offset = 
    if mrl_offset >= mdah.mdah_size then
      (Int64.add 512L (Int64.sub mrl_offset mdah.mdah_size))
    else 
      mrl_offset in

  let mrl_size = Cstruct.len md in
  let firstbit, secondbit =
    if Int64.(add mrl_offset (of_int mrl_size)) > mdah.mdah_size
    then
      let firstbit = Int64.(to_int (sub mdah.mdah_size mrl_offset)) in
      let secondbit = mrl_size - firstbit in
      firstbit, secondbit
    else
      mrl_size, 0 in
  let firstbitbuf = Cstruct.sub md 0 firstbit in
  let secondbitbuf = Cstruct.sub md firstbit secondbit in

  let absnewpos = Int64.add mrl_offset mdah.mdah_start in
  let open IO in
  B.write device absnewpos firstbitbuf >>= fun () ->
  B.write device (Int64.add mdah.mdah_start 512L) secondbitbuf >>= fun () ->

  (* Now we have to update the crc and pointer to the metadata *)
    
  let mrl_checksum = Crc.crc md in
  let new_raw_locn = { mrl_offset; mrl_size=Int64.of_int mrl_size; mrl_checksum; mrl_filler=0l; } in

  let mdah = {mdah with mdah_raw_locns=[new_raw_locn]} in
  Header_IO.write mdah device >>= fun () ->
  return mdah
end
