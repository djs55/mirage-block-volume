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

(* We change both the metadata header magic string and the PV label_type
   to completely hide the volume group from regular LVM when in journalled
   mode. *)

type t = [
  | `Lvm        (** normal LVM, synchronous and compatible with existing tools *)
  | `Journalled (** layer an operation journal on top of LVM *)
] with sexp

include S.MARSHAL with type t := t
include S.UNMARSHAL with type t := t
