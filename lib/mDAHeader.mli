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
(** The MetaDataArea header, which allows us to locate the metadata.
    Note this seems a bit strange, because a PV is part of a VG, but
    it's the PV that contains the VG info *)

val sizeof: int
(** The MDA header is always of fixed size *)

val equals: t -> t -> bool
(** [equals a b] if [a] and [b] represent the same MDA header *)

val rpc_of_t: t -> Rpc.t
val t_of_rpc: Rpc.t -> t

val marshal: t -> string
(** [marshal t] writes [t] to a binary string *)

val unmarshal: string -> t
(** [unmarshal buf] returns an instance of [t] read from [buf] *)

val to_string: t -> string
(** [to_string t] returns a pretty-printable version of [t] *)

val write: t -> string -> unit
(** [write t device] writes [t] to the [device] *)

val read: string -> Label.disk_locn -> t
(** [read device location] reads [t] from the [device] *)

val create: unit -> t
(** [create ()] returns an instance of [t] *)

val read_md: string -> t -> int -> string

val write_md: string -> t -> string -> t
 
