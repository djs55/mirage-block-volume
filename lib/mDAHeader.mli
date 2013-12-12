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

val mda_header_size: int

type mda_header

val rpc_of_mda_header: mda_header -> Rpc.t
val mda_header_of_rpc: Rpc.t -> mda_header

val unmarshal_mda_header: string -> Label.disk_locn -> mda_header

val to_ascii: mda_header -> string

val write_mda_header: mda_header -> string -> unit

val read_md: string -> mda_header -> int -> string

val write_md: string -> mda_header -> string -> mda_header
 
val create_blank: unit -> mda_header
