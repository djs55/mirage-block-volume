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
(** a contiguous fragment of physical space on a named volume *)

type t = area list
(** contiguous virtual space represented by an ordered list of 'areas' *)

include S.RPC with type t := t
include S.PRINT with type t := t

val create: string -> int64 -> t
(** [create name length] creates a single allocation from the entity
    with [name] covering region [0...length] *)

val get_name: area -> string
val get_start: area -> int64
val get_size: area -> int64
val get_end: area -> int64

(** [find free_space size] attempts to find space within [t] of total size
    [size]. If successful it returns a [t]. If it fails it returns the
    total amount of space currently free, which is insufficient to satisfy
    the request.
    The expected use is to 'allocate' space for a logical volume. *)
val find : t -> int64 -> (t, int64) Result.result

(** [merge t1 t2] returns a region [t] which contains all the physical
    space from both [t1] and [t2].
    The expected use is to return a previously-allocated [t] to a [t] which
    represents the free space. *)
val merge : t -> t -> t

(** [sub t1 t2] returns [t1] with all the space from [t2] removed.
    The expected use is to compute the remaining free space once space for
    a volume has been removed. *)
val sub : t -> t -> t
