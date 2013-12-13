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

type stat = 
    | Read
    | Write
    | Visible
	
and striped_segment = {
  st_stripe_size : int64;             (** In sectors *)
  st_stripes : (string * int64) list; (** pv name (LVM uuid) * start extent *)
}

and linear_segment = {
  l_pv_name : string; (* LVM uuid *)
  l_pv_start_extent : int64;
}

and segclass = 
  | Linear of linear_segment
  | Striped of striped_segment

and segment = 
    { s_start_extent : int64; 
      s_extent_count : int64;
      s_cls : segclass; }

and logical_volume = {
  name : string;
  id : Lvm_uuid.t;
  tags : Tag.t list;
  status : stat list;
  segments : segment list;
}

val logical_volume_of_rpc: Rpc.t -> logical_volume
val rpc_of_logical_volume: logical_volume -> Rpc.t

val status_to_string: stat -> string

val status_of_string: string -> stat

val sort_segments: segment list -> segment list

val write_to_buffer: Buffer.t -> logical_volume -> unit

val of_metadata: string -> (string * Absty.absty) list -> logical_volume

val allocation_of_segment: segment -> (string * (int64 * int64))  list

val allocation_of_lv: logical_volume -> (string * (int64 * int64)) list

val size_in_extents: logical_volume -> int64

val reduce_size_to: logical_volume -> int64 -> logical_volume
