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

include S.RPC with type t := t
include S.PRINT with type t := t

(* Needed only for the test case: *)
val make_area: string -> int64 -> int64 -> area
val unpack_area: area -> string * (int64 * int64)
val to_string1: area -> string
val contained: area -> area -> bool
val make_area_by_end: string -> int64 -> int64 -> area
val normalize: t -> t
(* ---- *)

val create: string -> int64 -> t

val get_name: area -> string
val get_start: area -> int64
val get_size: area -> int64
val get_end: area -> int64

exception PVS_DONT_MATCH of string * string

exception NonSingular_Containing_Area

val alloc_specified_areas : t -> t -> t

(** [alloc free_space size] attempts to allocate a region of [size] from
    [free_space]. If successful it returns [allocated_space, free_space]
    where [allocated_space] has total length [size] and the [free_space]
    corresponds to the remaining free space post-allocation. If unsuccessful
    it means there is insufficient free space and the total amount of free
    space is returned. *)
val alloc : t -> int64 -> ((t * t), int64) Result.result

(** [free free_space to_free] returns the new [free_space] after
    freeing [to_free] *)
val free : t -> t -> t
