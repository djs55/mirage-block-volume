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

open Lwt
open S

let block_error = function
  | `Unknown x -> `Error x
  | `Unimplemented -> `Error "unimplemented"
  | `Is_read_only -> `Error "device is read-only"
  | `Disconnected -> `Error "disconnected"

let get_size device =
  let stats = Unix.LargeFile.lstat device in
  match stats.Unix.LargeFile.st_kind with
  | Unix.S_BLK ->
    begin match Block.blkgetsize device with
    | `Ok x -> return (`Ok x)
    | `Error e -> return (block_error e)
    end
  | Unix.S_REG -> return (`Ok stats.Unix.LargeFile.st_size)
  | _ -> return (`Error (Printf.sprintf "Unable to query the size of %s: it is neither a file nor a block device" device))

let with_file filename flags f =
  Lwt_unix.openfile filename flags 0o0644 >>= fun fd ->
  Lwt.catch
    (fun () -> f fd >>= fun x -> Lwt_unix.close fd >>= fun () -> return (`Ok x))
    (fun e -> Lwt_unix.close fd >>= fun () -> return (`Error (Printexc.to_string e)))

let get kind device offset length =
  let buf = Cstruct.create length in
  with_file device [ Lwt_unix.O_RDONLY ]
    (fun fd ->
      Lwt_unix.LargeFile.lseek fd offset Lwt_unix.SEEK_SET >>= fun _ ->
      Block.really_read fd buf >>= fun () ->
      return buf)

let put kind device offset buf =
  let flags = [ Lwt_unix.O_RDWR; Lwt_unix.O_DSYNC ] in
  with_file device flags
    (fun fd ->
      Lwt_unix.LargeFile.lseek fd offset Lwt_unix.SEEK_SET >>= fun _ ->
      Block.really_write fd buf)
