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

module Status : sig
  type t =
    | Read
    | Write
    | Resizeable
    | Clustered

  include S.PRINT with type t := t

  val of_string: string -> (t, string) Result.result
end

type metadata = {
  name : string;                (** name given by the user *)
  id : Uuid.t;                  (** arbitrary unique id *)
  seqno : int;                  (** sequence number of the next operation *)
  status : Status.t list;       (** status flags *)
  extent_size : int64;          (** the size of a block ("extent") in 512 byte sectors *)
  max_lv : int;
  max_pv : int;
  pvs : Pv.t list;              (** physical volumes *)
  lvs : Lv.t list;              (** logical volumes *)
  free_space : Pv.Allocator.t;  (** free space in physical volumes, which can be used for logical volumes *)
} with sexp
(** A volume group *)

val do_op: metadata -> Redo.Op.t -> (metadata * Redo.Op.t, string) Result.result
(** [do_op t op] performs [op], returning the modified volume group [t] *)

include S.MARSHAL with type t := metadata
include S.VOLUME
  with type t := metadata
  and type name := string
  and type tag := Tag.t
  and type size := int64
  and type op := Redo.Op.t

module Make(Log: S.LOG)(Block: S.BLOCK) : sig

  type vg
  (** A volume group spread over a set of block devices *)

  val metadata_of: vg -> metadata
  (** Extract a snapshot of the volume group metadata *)

  val format: string -> ?magic:Magic.t -> (Pv.Name.t * Block.t) list -> unit S.io
  (** [format name devices_and_names] initialises a new volume group
      with name [name], using physical volumes [devices] *)

  val connect: Block.t list -> vg S.io
  (** [connect devices] opens a volume group contained on [devices]
      for reading and writing *)

  val update: vg -> Redo.Op.t list -> unit S.io
  (** [update t updates] performs the operations [updates] and ensures
      the changes are persisted. *)

  val sync: vg -> unit S.io
  (** [sync t] flushes all pending writes associated with [t] to the
      main metadata area. This is only needed if you plan to switch off
      the redo-log. *)

  module Volume : sig
    include V1_LWT.BLOCK

    val connect: id -> [ `Ok of t | `Error of error ] Lwt.t

    val metadata_of: id -> Lv.t
    (** return the metadata associated with a volume *)
  end

  val find: vg -> string -> Volume.id option
  (** [find vg name] finds the volume with name [name] *)
end
