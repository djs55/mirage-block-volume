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

open Lwt

let block_error = function
  | `Unknown x -> `Error x
  | `Unimplemented -> `Error "unimplemented"
  | `Is_read_only -> `Error "device is read-only"
  | `Disconnected -> `Error "disconnected"

module IO = struct
  let ( >>= ) m f = m >>= function
  | `Error e -> return (block_error e)
  | `Ok x -> f x
end

module Make(B: S.BLOCK) = struct

  let get_size device =
    let open IO in
    B.connect device >>= fun t ->
    let open Lwt in
    B.get_info t >>= fun info ->
    B.disconnect t >>= fun () ->
    return (`Ok (Int64.(mul info.B.size_sectors (of_int info.B.sector_size))))

  let read device offset length =
    let open IO in
    B.connect device >>= fun t ->
    let open Lwt in
    B.get_info t >>= fun info ->
    let start_sector = Int64.(div offset (of_int info.B.sector_size)) in
    let start_offset = Int64.(to_int (rem offset (of_int info.B.sector_size))) in
    (* We need to read 'start_offset's worth of useless junk at the beginning *)
    let length' = length + start_offset in
    (* We need to round our buffers up to the next sector size, might as well use a page *)
    let pages = (length' + 4095) / 4096 in
    let ba = Io_page.get pages in
    let buf = Io_page.to_cstruct ba in
    let open IO in
    B.read t start_sector [ buf ] >>= fun () ->
    let open Lwt in
    B.disconnect t >>= fun () ->
    let open IO in
    return (`Ok (Cstruct.sub buf start_offset length))

  let write device offset buf =
    let open IO in
    B.connect device >>= fun t ->
    let open Lwt in
    B.get_info t >>= fun info ->
    let start_sector = Int64.(div offset (of_int info.B.sector_size)) in
    let start_offset = Int64.(to_int (rem offset (of_int info.B.sector_size))) in
    (* We need to read 'start_offset's worth of useless junk at the beginning *)
    let length' = Cstruct.len buf + start_offset in
    (* We need to round our buffers up to the next sector size, might as well use a page *)
    let pages = (length' + 4095) / 4096 in
    let ba = Io_page.get pages in
    let aligned_buf = Io_page.to_cstruct ba in
    let open IO in
    B.read t start_sector [ aligned_buf ] >>= fun () ->
    Cstruct.blit buf 0 aligned_buf start_offset (Cstruct.len buf);
    B.write t start_sector [ aligned_buf ] >>= fun () ->
    let open Lwt in
    B.disconnect t >>= fun () ->
    let open IO in
    return (`Ok ())
end
