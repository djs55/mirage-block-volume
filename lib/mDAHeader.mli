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


open Absty
open Logging

(** Start with the meta-metadata - this is how we actually locate the 
    metadata on disk. It's a bit backwards, because a PV is part of a 
    volume group, but it's the PV that contains the volume group info *)

open Device
open Lvmmarshal

val sizeof: int

type t

val rpc_of_t: t -> Rpc.t
val t_of_rpc: Rpc.t -> t

val unmarshal: string -> Label.disk_locn -> t

val marshal: t -> string

val to_string: t -> string

val write: t -> string -> unit

val read_md: string -> t -> int -> string

val write_md: string -> t -> string -> t
 
val create: unit -> t
