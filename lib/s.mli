(*
 * Copyright (C) 2013 Citrix Systems Inc.
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

module type PRINT = sig
  type t
  val to_string: t -> string
end

module type MARSHAL = sig
  type t
  val marshal: t -> Cstruct.t -> Cstruct.t
end

module type UNMARSHAL = sig
  type t
  val unmarshal: Cstruct.t -> (t * Cstruct.t, string) Result.result
end

module type EQUALS = sig
  type t
  val equals: t -> t -> bool
end

module type RPC = sig
  type t
  val t_of_rpc: Rpc.t -> t
  val rpc_of_t: t -> Rpc.t
end

module type VOLUME = sig

  type t
  (** a set of logical volumes and free space *)

  type tag
  (** an arbitrary tag added to a volume *)

  type name
  (** the name of a volume. This must be unique within a set *)

  type size
  (** the size of a volume in bytes *)

  val create: t -> name -> int64 -> (t, string) Result.result
  (** [create t name size] extends the volume group [t] with a new
      volume named [name] with size at least [size] bytes. The actual
      size of the volume may be rounded up. *)

  val rename: t -> name -> name -> (t, string) Result.result
  (** [rename t name new_name] returns a new volume group [t] where
      the volume previously named [name] has been renamed to [new_name] *)

  val resize: t -> name -> size -> (t, string) Result.result
  (** [resize t name new_size] returns a new volume group [t] where
      the volume with [name] has new size at least [new_size]. The
      size of the volume may be rounded up. *)

  val remove: t -> name -> (t, string) Result.result
  (** [remove t name] returns a new volume group [t] where the volume
      with [name] has been deallocated. *)

  val add_tag: t -> name -> tag -> (t, string) Result.result
  (** [add_tag t name tag] returns a new volume group [t] where the
      volume with [name] has a new tag [tag] *)

  val remove_tag: t -> name -> tag -> (t, string) Result.result
  (** [remove_tag t name tag] returns a new volume group [t] where the
      volume with [name] has no tag [tag] *)
end

