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


(** Physical Volume module *)

module Label_header : sig
  type t with sexp

  val create: Magic.t -> t

  val magic_of: t -> Magic.t option
  (** Return the magic of a particular label, or [None] if we don't
      recognise the magic. *)

  include S.PRINT with type t := t
  include S.EQUALS with type t := t
  include S.MARSHAL with type t := t
  include S.UNMARSHAL with type t := t
end

module Location : sig
  type t = {
    offset : int64;
    size : int64;
  }
end

module Pv_header : sig
  type t = {
    id : Uuid.t;
    device_size : int64;
    extents: Location.t list;
    metadata_areas: Location.t list;
  }

  val create: Uuid.t -> int64 -> int64 -> int64 -> t
  (** [create id size mda_start mda_size] creates a Pv_header
      for PVID [id], disk [size], and with metadata stored from
      [mda_start] to [mda_start + mda_size] *)

  include S.EQUALS with type t := t
  include S.PRINT with type t := t
  include S.MARSHAL with type t := t
  include S.UNMARSHAL with type t := t
end

type t = {
  label_header : Label_header.t;
  pv_header : Pv_header.t;
}

val create: ?magic:Magic.t -> Uuid.t -> int64 -> int64 -> int64 -> t

include S.EQUALS with type t := t
include S.PRINT with type t := t
include S.MARSHAL with type t := t
include S.UNMARSHAL with type t := t
include S.SEXPABLE with type t := t

val get_metadata_locations: t -> Location.t list

type error = [
  | `Msg of string
]

type 'a result = ('a, error) Result.result

val open_error: 'a result -> ('a, [> error]) Result.result

module Make : functor(Block: S.BLOCK) -> sig
  val read: Block.t -> t result Lwt.t

  val write: Block.t -> t -> unit result Lwt.t
end
