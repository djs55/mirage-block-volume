(*
 * Copyright (C) 2015 Citrix Systems Inc.
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

type t = [
  | `Lvm        (** normal LVM, synchronous and compatible with existing tools *)
  | `Journalled (** layer an operation journal on top of LVM *)
] with sexp

let lvm_magic     = " LVM2 x[5A%r0N*>"
let journal_magic = "JOURNALLED!1!1!!"

let length = String.length lvm_magic

let marshal t buf =
  Cstruct.blit_from_string (if t = `Lvm then lvm_magic else journal_magic) 0 buf 0 length;
  Cstruct.shift buf length

let unmarshal buf =
  if Cstruct.len buf < length
  then `Error "Buffer is too small for a magic string"
  else begin
    let m = Cstruct.(to_string (sub buf 0 length)) in
    let rest = Cstruct.shift buf length in
    if m = lvm_magic
    then `Ok (`Lvm, rest)
    else if m = journal_magic
    then `Ok (`Journalled, rest)
    else `Error (Printf.sprintf "Failed to parse magic string '%s'" (String.escaped m))
  end
