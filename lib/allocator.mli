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

type area = string * (int64 * int64)

type t = area list

val t_of_rpc: Rpc.t -> t
val rpc_of_t: t -> Rpc.t

(* Needed only for the test case: *)
val make_area: string -> int64 -> int64 -> area
val unpack_area: area -> string * (int64 * int64)
val to_string1: area -> string
val contained: area -> area -> bool
val make_area_by_end: string -> int64 -> int64 -> area
val safe_alloc: t -> int64 -> (area list * t) option
val normalize: t -> t
(* ---- *)

val to_string: t -> string

val create: string -> int64 -> t

val get_name: area -> string
val get_start: area -> int64
val get_size: area -> int64
val get_end: area -> int64

exception PVS_DONT_MATCH of string * string

exception NonSingular_Containing_Area

val alloc_specified_areas : t -> t -> t

val alloc : t -> int64 -> t * t

val free : t -> t -> t

