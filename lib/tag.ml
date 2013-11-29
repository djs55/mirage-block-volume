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

(* Note according to
   https://access.redhat.com/site/documentation/en-US/Red_Hat_Enterprise_Linux/6/html/Logical_Volume_Manager_Administration/lvm_tags.html

   after RH 6.1 tags can be 1024 characters and can contain  [/=!:#&]
*) 

type t = string with rpc

module CharSet = Set.Make(struct type t = char let compare = compare end)

(* The first character is drawn from a more limited alphabet *)
let first_char_set =
  let first_char_list = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_+." in
  let rec loop acc i =
    if i < (String.length first_char_list)
    then loop (CharSet.add first_char_list.[i] acc) (i + 1)
    else acc in
  loop CharSet.empty 0

(* Every other character can be a '-' *)
let other_char_set = CharSet.add '-' first_char_set

let of_string s = match s with
	| "" -> "empty_tag"
	| s ->
		let s = if String.length s > 128 then String.sub s 0 128 else String.copy s in
		(* Replace any invalid character with a '_' *)
		if not(CharSet.mem s.[0] first_char_set) then s.[0] <- '_';
		for i = 1 to String.length s - 1 do
			if not(CharSet.mem s.[i] other_char_set) then s.[i] <- '_'
		done;
		s

let to_string t = t
