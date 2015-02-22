(*
 * Copyright (C) 2015 Citrix Systems Inc.
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
open Lvm

val name_of: Vg.metadata -> Lv.t -> string
(** [name_of vg lv] returns the conventional name used for a device mapper
    device corresponding to [lv]. Device mapper devices are arbitrary but this
    is the naming convention that LVM uses. *)

val vg_lv_of_name: string -> string * string
(** [vg_lv_of_name filename] returns the (vg name, lv name) of the device mapper
    device [filename] *)

type devices
(** The set of local physical devices containing the PVs *)

val read: string list -> devices Lwt.t
(** Read the LVM headers on a set of local physical devices *)

val to_targets: devices -> Vg.metadata -> Lv.t -> Devmapper.Target.t list
(** [to_targets devices vg lv] returns the device mapper targets needed to access
    the data stored within [lv], where [devices] are the local physical
    disks containing the PVs. *)
