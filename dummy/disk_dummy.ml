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
open Lvm
open S

let dummy_base = ref "/tmp"

let string_of_kind = function
  | Label -> "pvh"
  | MDA_header -> "mdah"
  | MD1 -> "md1"
  | MD2 -> "md2"

let rec mkdir_p x =
  if Sys.file_exists x
  then ()
  else
    let parent = Filename.dirname x in
    if not(Sys.file_exists parent) then mkdir_p parent;
    Unix.mkdir x 0o0755

let dummy_fname dev ty =
  let dir = Filename.concat !dummy_base dev in
  mkdir_p dir;
  Filename.concat dir ty

let get_size device = return (`Ok Constants.tib)

let with_file filename flags f =
  Lwt_unix.openfile filename flags 0o0644 >>= fun fd ->
  Lwt.catch
    (fun () -> f fd >>= fun x -> Lwt_unix.close fd >>= fun () -> return (`Ok x))
    (fun e -> Lwt_unix.close fd >>= fun () -> return (`Error (Printexc.to_string e)))

let get kind device offset length =
  let name = string_of_kind kind in
  let filename = dummy_fname device name in
  let offset = 0L in
  let buf = Cstruct.create length in
  with_file filename [ Lwt_unix.O_RDONLY ]
    (fun fd ->
      Lwt_unix.LargeFile.lseek fd offset Lwt_unix.SEEK_SET >>= fun _ ->
      Block.really_read fd buf >>= fun () ->
      return buf)

let put kind device offset buf =
  let name = string_of_kind kind in
  let filename = dummy_fname device name in
  let flags = [ Lwt_unix.O_RDWR; Lwt_unix.O_DSYNC; Lwt_unix.O_CREAT ] in
  let offset = 0L in
  with_file filename flags
    (fun fd ->
      Lwt_unix.LargeFile.lseek fd offset Lwt_unix.SEEK_SET >>= fun _ ->
      Block.really_write fd buf)
