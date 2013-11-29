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
(** A valid LVM volume tag *)

val rpc_of_t : t -> Rpc.t
val t_of_rpc : Rpc.t -> t

val of_string : string -> t
(** [of_string string] constructs a tag from a string. Any characters
    which are illegal will be replaced by '_' *)

val to_string : t -> string
(** [to_string t] returns one of the strings which correspond with the
    tag *)
