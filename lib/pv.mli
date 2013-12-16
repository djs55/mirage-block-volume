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


(** Physical Volumes *)

module Status : sig
  type t = 
    | Allocatable

  include S.PRINT with type t := t

  val of_string: string -> (t, string) Result.result
end
	
type t = {
  name : string;
  id : Uuid.t;
  dev : string;
  real_device : string; (* Actual device we're reading/writing to/from *)
  status : Status.t list;
  dev_size : int64;
  pe_start : int64;
  pe_count : int64;
  label : Label.t;  (* The one label for this PV *)
  mda_headers : Metadata.Header.t list; 
}

include S.RPC with type t := t

val write_to_buffer: Buffer.t -> t -> unit

val of_metadata: string -> (string * Absty.absty) list -> (Label.t * Metadata.Header.t list) list -> t IO.io

val find_metadata: string -> (Cstruct.t * (Label.t * Metadata.Header.t list)) IO.io
(** Find the metadata area on a device and return the text of the metadata *)

val human_readable: t -> string

val create_new: string -> string -> t IO.io
(** [create_new real_device name] *)
