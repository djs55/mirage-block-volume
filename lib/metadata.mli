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

module Header: sig
  type t
  (** The MetaDataArea header, which allows us to locate the metadata.
      Note this seems a bit strange, because a PV is part of a VG, but
      it's the PV that contains the VG info *)

  val sizeof: int
  (** The MDA header is always of fixed size *)

  include S.EQUALS with type t := t
  include S.SEXPABLE with type t := t
  include S.PRINT with type t := t
  include S.MARSHAL with type t := t
  include S.UNMARSHAL with type t := t
  include Monad.S2 with type ('a, 'b) t := ('a, 'b) Result.result

  module Make : functor(Block: S.BLOCK) -> sig

    val write: t -> Block.t -> unit S.io
    (** [write t device] writes [t] to the [device] *)

    val read: Block.t -> Label.Location.t -> t S.io
    (** [read device location] reads [t] from the [device] *)

    val read_all: Block.t -> Label.Location.t list -> t list S.io
    (** [read device locations] reads the [t]s found at [location]s,
        or an error if any single one can't be read. *)
  end

  val create: Magic.t -> t
  (** [create magic] returns an instance of [t] *)
end

val default_start: int64
(** Default byte offset to place the metadata area *)

val default_size: int64
(** Default length of the metadata area in bytes *)

module Make : functor(Block: S.BLOCK) -> sig
  val read: Block.t -> Header.t -> int -> Cstruct.t S.io

  val write: Block.t -> Header.t -> Cstruct.t -> Header.t S.io
end
