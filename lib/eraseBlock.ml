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
  | `Unknown x -> `Error (`Msg x)
  | `Unimplemented -> `Error (`Msg "unimplemented")
  | `Is_read_only -> `Error (`Msg "device is read-only")
  | `Disconnected -> `Error (`Msg "disconnected")

module IO = struct
  let ( >>= ) m f = m >>= function
  | `Error e -> return (block_error e)
  | `Ok x -> f x
end

module Make(B: S.BLOCK) = struct
  let erase ?(pattern = "This block has been erased by mirage-block-volume/lib/eraseBlock.ml") t =
    let open Lwt in
    B.get_info t
    >>= fun info ->
    if info.B.sector_size > 4096
    then return (`Error (`Msg (Printf.sprintf "eraseBlock can only cope with sector size <= 4096: got %d" info.B.sector_size)))
    else begin
      let page = Io_page.get 1 in
      let sector = Cstruct.sub (Io_page.to_cstruct page) 0 info.B.sector_size in
      for i = 0 to Cstruct.len sector - 1 do
        Cstruct.set_char sector i (pattern.[i mod (String.length pattern)])
      done;
      let open IO in
      let rec loop n =
        if n = info.B.size_sectors
        then return (`Ok ())
        else
          B.write t n [ sector ]
          >>= fun () ->
          loop (Int64.succ n) in
      loop 0L
    end
end
