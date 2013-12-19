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

include Monad.S2 with type ('a, 'b) t = ('a, 'b) Result.result Lwt.t

module FromResult: sig
  type ('a, 'b) t = ('a, 'b) Result.result Lwt.t                              
  val ( >>= ) :
    [< `Error of 'a | `Ok of 'b ] ->
    ('b -> ([> `Error of 'a ] as 'c) Lwt.t) -> 'c Lwt.t
  val return : 'a -> [> `Ok of 'a ] Lwt.t
  val fail : 'b -> [> `Error of 'b ] Lwt.t
  val all: ('a, 'b) Result.result list Lwt.t -> ('a list, 'b) Result.result Lwt.t
end

type 'a io = ('a, string) Result.result Lwt.t

val get_size: string -> int64 io

(* The data being read or written has a 'kind' *)
type kind =
  | Label      (** PV label *)
  | MDA_header (** Metadata area header *)
  | MD1        (** Metadata fragment 1 *)
  | MD2        (** Metadata fragment 2 *)

val get: kind -> string -> int64 -> int -> Cstruct.t io

val put: kind -> string -> int64 -> Cstruct.t -> unit io
