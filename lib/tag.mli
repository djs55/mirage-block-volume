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

type t

val rpc_of_t : t -> Rpc.t
val t_of_rpc : Rpc.t -> t
(** Checks whether a string is a valid tag string.
    Tag character set: A-Za-z0-9_+.-
    Can't start with hyphen. Max length is 128.
    Empty tags are currently not allowed. *)
val is_valid : string -> bool
(** Creates a tag from a string. Fails on non-conforming strings. *)
val of_string : string -> t
(** Converts a tag to a string. *)
val string_of : t -> string
