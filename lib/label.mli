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
  type t

  val create: unit -> t

  include S.MARSHAL with type t := t
end

type disk_locn = {
  dl_offset : int64;
  dl_size : int64;
}

module Pv_header : sig
  type t = {
    pvh_id : Lvm_uuid.t;
    pvh_device_size : int64;
    pvh_extents: disk_locn list;
    pvh_metadata_areas: disk_locn list;
  }

  val create: Lvm_uuid.t -> int64 -> int64 -> int64 -> t
  (** [create id size mda_start mda_size] creates a Pv_header
      for PVID [id], disk [size], and with metadata stored from
      [mda_start] to [mda_start + mda_size] *)

  include S.EQUALS with type t := t
  include S.PRINT with type t := t
  include S.MARSHAL with type t := t
end

type t = {
  device : string;
  label_header : Label_header.t;
  pv_header : Pv_header.t;
}

val create: string -> Lvm_uuid.t -> int64 -> int64 -> int64 -> t

include S.EQUALS with type t := t
include S.PRINT with type t := t
include S.MARSHAL with type t := t
include S.RPC with type t := t
include Monad_.S2 with type ('a, 'b) t := ('a, 'b) Result.result

val get_metadata_locations: t -> disk_locn list

val get_pv_id: t -> Lvm_uuid.t

val get_device: t -> string

val read: string -> t IO.io

val write: t -> unit IO.io

