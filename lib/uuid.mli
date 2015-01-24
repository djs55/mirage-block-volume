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


(* LVM uses uuids that aren't really proper uuids. This module manipulates them *)

type t
(** An LVM 'uuid'. Note this isn't a valid uuid according to RFC4122 *)

include S.PRINT with type t := t
include S.SEXPABLE with type t := t
include S.MARSHAL with type t := t
include S.UNMARSHAL with type t := t
include Monad.S2 with type ('a, 'b) t := ('a, 'b) Result.result

val create: unit -> t
(** [create ()] generates a fresh uuid *)

val of_string: string -> (t, string) Result.result
(** [of_string s] returns [t] corresponding to [s] *)

