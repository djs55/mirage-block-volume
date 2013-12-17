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

module Status : sig
  type t = 
    | Read
    | Write
    | Visible

  include S.PRINT with type t := t

  val of_string: string -> (t, string) Result.result
end
	
type striped_segment = {
  st_stripe_size : int64;             (** In sectors *)
  st_stripes : (string * int64) list; (** pv name (LVM uuid) * start extent *)
}

type linear_segment = {
  l_pv_name : string; (* LVM uuid *)
  l_pv_start_extent : int64;
}

and segclass = 
  | Linear of linear_segment
  | Striped of striped_segment

type segment = 
    { s_start_extent : int64; 
      s_extent_count : int64;
      s_cls : segclass; }

type t = {
  name : string;           (** name given by the user *)
  id : Uuid.t;             (** arbitrary unique id *)
  tags : Tag.t list;       (** tags given by the user *)
  status : Status.t list;  (** status flags *)
  segments : segment list; (** an ordered list of blocks ('segments') *)
}
(** a logical volume within a volume group *)

include S.RPC with type t := t
include S.MARSHAL with type t := t

val sort_segments: segment list -> segment list

val of_metadata: string -> (string * Absty.absty) list -> (t, string) Result.result

val allocation_of_segment: segment -> (string * (int64 * int64))  list

val allocation_of_lv: t -> (string * (int64 * int64)) list

val size_in_extents: t -> int64

val reduce_size_to: t -> int64 -> (t, string) Result.result
(** [reduce_size_to lv new_size] reduces the size of [lv] to [new_size],
    or fails if the [new_size] is less than the current size. *)
