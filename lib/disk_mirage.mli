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

module type Block = V1.BLOCK
  with type 'a io = 'a Lwt.t
  and type page_aligned_buffer = Cstruct.t
  and type id = string

module type Memory = sig
  type t

  val get: int -> t
  val to_cstruct: t -> Cstruct.t
end

module Make : functor (B: Block) -> functor(M: Memory) -> S.DISK
