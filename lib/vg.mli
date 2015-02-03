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

type t = {
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
}
(** A volume group *)

include S.SEXPABLE with type t := t
include S.MARSHAL with type t := t
include S.VOLUME
  with type t := t
  and type name := string
  and type tag := Tag.t
  and type size := int64

module Make : functor(DISK: S.DISK) -> sig

  val format: string -> (string * string) list -> unit S.io
  (** [format name names_and_devices] initialises a new volume group
      with name [name], using physical volumes
      [names_and_devices = [ name1, device1; ...]] *)

  val read: string list -> t S.io
  (** [read devices] reads the volume group information from
      the set of physical volumes [devices] *)

  val write: t -> t S.io
  (** [write t] flushes the metadata of [t] to the physical volumes *)

end
