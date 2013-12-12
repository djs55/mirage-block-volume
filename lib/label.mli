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

type label_header

type disk_locn = {
  dl_offset : int64;
  dl_size : int64;
}

type pv_header = {
  pvh_id : Lvm_uuid.t;
  pvh_device_size : int64;
  pvh_extents: disk_locn list;
  pvh_metadata_areas: disk_locn list;
}

type t = {
  device : string;
  label_header : label_header;
  pv_header : pv_header;
}

val t_of_rpc: Rpc.t -> t
val rpc_of_t: t -> Rpc.t

val write_label_and_pv_header: t -> unit

val get_metadata_locations: t -> disk_locn list

val get_pv_id: t -> Lvm_uuid.t

val get_device: t -> string

val find: string -> t

val create: string -> Lvm_uuid.t -> int64 -> 'a -> 'a -> int64 -> int64 -> t

val to_ascii: t -> string
