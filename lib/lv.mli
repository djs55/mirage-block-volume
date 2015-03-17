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
open Sexplib.Std
open Lvm_internal

module Status : sig
  type t = 
    | Read
    | Write
    | Visible
  with sexp

  include S.PRINT with type t := t

  val of_string: string -> (t, [ `Msg of string ]) Result.result
end

module Linear : sig
  type t = {
    name : Pv.Name.t;
    start_extent : int64;
  } with sexp
end

module Segment : sig
  type cls = 
    | Linear of Linear.t
  with sexp

  type t = {
    start_extent : int64; 
    extent_count : int64;
    cls : cls;
  } with sexp

  type ts = t list with sexp_of

  val sort: t list -> t list

  val to_allocation: t -> (Pv.Name.t * (int64 * int64)) list
  (** Compute the physical extents occupied by the storage *)

  val linear: int64 -> Pv.Allocator.t -> t list
  (** [create segment space] creates segments mapping from
      [segment] linearly covering all the [space] *)
end

type t = {
  name : string;             (** name given by the user *)
  id : Uuid.t;               (** arbitrary unique id *)
  tags : Name.Tag.t list;    (** tags given by the user *)
  status : Status.t list;    (** status flags *)
  (* TODO: this must be written in ascending order of start_extent.
     Should we convert this into a Map? *)
  segments : Segment.ts;     (** an ordered list of blocks ('segments') *)
} with sexp
(** a logical volume within a volume group *)

include S.SEXPABLE with type t := t
include S.MARSHAL with type t := t

val of_metadata: string -> (string * Absty.absty) list -> (t, [ `Msg of string ]) Result.result

val to_allocation: t -> (Pv.Name.t * (int64 * int64)) list

val size_in_extents: t -> int64

val find_extent: t -> int64 -> Segment.t option
(** [find_extent t x] returns the segment containing [x] *)

val reduce_size_to: t -> int64 -> (t, [> `Msg of string ]) Result.result
(** [reduce_size_to lv new_size] reduces the size of [lv] to [new_size] *)
