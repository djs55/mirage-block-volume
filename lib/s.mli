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

module type SEXPABLE = sig
  type t
  val t_of_sexp: Sexplib.Sexp.t -> t
  val sexp_of_t: t -> Sexplib.Sexp.t
end

(* The data being read from or written to disk has a 'kind' *)
type kind =
  | Label      (** PV label *)
  | MDA_header (** Metadata area header *)
  | MD1        (** Metadata fragment 1 *)
  | MD2        (** Metadata fragment 2 *)

type 'a io = ('a, string) Result.result Lwt.t

module type DISK = sig
  val get_size: string -> int64 io

  val get: kind -> string -> int64 -> int -> Cstruct.t io

  val put: kind -> string -> int64 -> Cstruct.t -> unit io
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

  type op
  (** The type of an atomic operation *)

  val create: t -> name -> int64 -> (t * op, string) Result.result
  (** [create t name size] extends the volume group [t] with a new
      volume named [name] with size at least [size] bytes. The actual
      size of the volume may be rounded up. *)

  val rename: t -> name -> name -> (t * op, string) Result.result
  (** [rename t name new_name] returns a new volume group [t] where
      the volume previously named [name] has been renamed to [new_name] *)

  val resize: t -> name -> size -> (t * op, string) Result.result
  (** [resize t name new_size] returns a new volume group [t] where
      the volume with [name] has new size at least [new_size]. The
      size of the volume may be rounded up. *)
 
  val remove: t -> name -> (t * op, string) Result.result
  (** [remove t name] returns a new volume group [t] where the volume
      with [name] has been deallocated. *)

  val add_tag: t -> name -> tag -> (t * op, string) Result.result
  (** [add_tag t name tag] returns a new volume group [t] where the
      volume with [name] has a new tag [tag] *)

  val remove_tag: t -> name -> tag -> (t * op, string) Result.result
  (** [remove_tag t name tag] returns a new volume group [t] where the
      volume with [name] has no tag [tag] *)
end

module type NAME = sig
  include Map.OrderedType
  include SEXPABLE with type t := t
  include PRINT with type t := t
end

module type ALLOCATOR = sig

  type name with sexp

  type area = name * (int64 * int64)
  (** a contiguous fragment of physical space on a volume ['a] *)

  type t = area list with sexp

  val to_string: t -> string

  val create: name -> int64 -> t
  (** [create name length] creates a single allocation from the entity
      with [name] covering region [0...length] *)

  val get_name: area -> name
  val get_start: area -> int64
  val get_size: area -> int64
  val get_end: area -> int64

  (** [find free_space size] attempts to find space within [t] of total size
      [size]. If successful it returns a [t]. If it fails it returns the
      total amount of space currently free, which is insufficient to satisfy
      the request.
      The expected use is to 'allocate' space for a logical volume. *)
  val find : t -> int64 -> (t, int64) Result.result

  (** [merge t1 t2] returns a region [t] which contains all the physical
      space from both [t1] and [t2].
      The expected use is to return a previously-allocated [t] to a [t] which
      represents the free space. *)
  val merge : t -> t -> t

  (** [sub t1 t2] returns [t1] with all the space from [t2] removed.
      The expected use is to compute the remaining free space once space for
      a volume has been removed. *)
  val sub : t -> t -> t

  (** [size t] returns the total size of [t] *)
  val size : t -> int64
end
