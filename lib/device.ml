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
open Unixext

let dummy_fname dev ty =
  let fname = Printf.sprintf "%s/%s/%s" (!Constants.dummy_base) dev ty in
  let basedir = Filename.dirname fname in
  Unixext.mkdir_rec basedir 0o755;
  fname

let get_mda_header device offset mda_header_size =
  let offset,fd = 
  if !Constants.dummy_mode 
    then (0L,Unix.openfile (dummy_fname device "mdah") [Unix.O_RDONLY] 0o000) 
    else (offset,Unix.openfile device [Unix.O_RDONLY] 0o000) in
  ignore(Unix.LargeFile.lseek fd offset Unix.SEEK_SET);
  let buf = really_read_string fd mda_header_size in
  Unix.close fd;
  buf

let put_mda_header device offset header =
  let fd =
    if !Constants.dummy_mode then begin
      Unix.openfile (dummy_fname device "mdah") [Unix.O_RDWR; Unix.O_DSYNC; Unix.O_CREAT] 0o644
    end else begin
      let fd = Unix.openfile device [Unix.O_RDWR; Unix.O_DSYNC] 0o000 in
      ignore(Unix.LargeFile.lseek fd offset Unix.SEEK_SET);
      fd
    end
    in
  let written = Unix.write fd header 0 (String.length header) in
  if written <> (String.length header) then failwith "Wrote short!";
  Unix.close fd
