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

type status =
    | Read
    | Write
    | Resizeable
    | Clustered

type vg = {
  name : string;
  id : Uuid.t;
  seqno : int;
  status : status list;
  extent_size : int64;
  max_lv : int;
  max_pv : int;
  pvs : Pv.t list; (* Device to pv map *)
  lvs : Lv.t list;
  free_space : Allocator.t;
  (* XXX: hook in the redo log *)
  ops : Redo.sequenced_op list;
}

val vg_of_rpc: Rpc.t -> vg
val rpc_of_vg: vg -> Rpc.t

val status_to_string: status -> string

val status_of_string: string -> (status, string) Result.result

val marshal: vg -> Cstruct.t -> Cstruct.t
    
val create_lv: vg -> string -> int64 -> (vg, string) Result.result

val rename_lv: vg -> string -> string -> (vg, string) Result.result

val resize_lv: vg -> string -> int64 -> (vg, string) Result.result

val remove_lv: vg -> string -> (vg, string) Result.result

val add_tag_lv: vg -> string -> Tag.t -> (vg, string) Result.result

val remove_tag_lv: vg -> string -> Tag.t -> (vg, string) Result.result

val write_full: vg -> vg IO.io

val write: vg -> bool -> vg IO.io

val of_metadata: Absty.absty -> vg IO.io

val create_new: string -> (string * string) list -> unit IO.io

val load: string list -> vg IO.io
