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


(* LVM uses uuids that aren't really proper uuids. This module manipulates them *)

type t = string with rpc

let format = [6; 4; 4; 4; 4; 4; 6] 

let charlist = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!#"

let create () =
  let s = String.make (32+6) '-' in
  let rec make i j n f =
    if n=0 then match f with
      | n'::ns -> make i (j+1) n' ns
      | _ -> ()
    else (s.[j] <- charlist.[Random.int 62]; make (i+1) (j+1) (n-1) f)
  in
  make 0 0 (List.hd format) (List.tl format);
    s

let add_hyphens str =
  let str2 = String.make (32+6) '-' in
  let foldfn (i,j) n = String.blit str i str2 j n; (i+n,j+n+1) in
  ignore(List.fold_left foldfn (0,0) format);
  str2

let remove_hyphens str =
  let str2 = String.create 32 in
  let foldfn (i,j) n = String.blit str i str2 j n; (i+n+1, j+n) in
  ignore(List.fold_left foldfn (0,0) format);
  str2

let sizeof = 32

include Result

let unmarshal buf =
  let str = Cstruct.(to_string (sub buf 0 sizeof)) in
  let buf = Cstruct.shift buf sizeof in
  return (add_hyphens str, buf)

let marshal t buf =
  let str = remove_hyphens t in
  Cstruct.blit_from_string str 0 buf 0 sizeof;
  Cstruct.shift buf sizeof

let to_string x = x
let of_string x = `Ok x

