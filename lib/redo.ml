(*
 * Copyright (C) 2009-2015 Citrix Systems Inc.
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

open Logging

module Op = struct 
  type lvrename_t = {
    lvmv_new_name : string;
  }

  and lvreduce_t = {
    lvrd_new_extent_count : int64;
  }

  and lvcrop_t = {
    lvc_segments: Lv.Segment.t list;
  }

  and lvexpand_t = {
    lvex_segments : Lv.Segment.t list;
  } with sexp

  (** First string corresponds to the name of the LV. *)
  type t =
    | LvCreate of Lv.t
    | LvReduce of string * lvreduce_t
    | LvCrop   of string * lvcrop_t
    | LvExpand of string * lvexpand_t
    | LvRename of string * lvrename_t
    | LvRemove of string
    | LvAddTag of string * Tag.t
    | LvRemoveTag of string * Tag.t
  with sexp

  let of_cstruct x =
    try
      Some (Cstruct.to_string x |> Sexplib.Sexp.of_string |> t_of_sexp)
    with _ -> 
      None

  let to_cstruct t =
    let s = sexp_of_t t |> Sexplib.Sexp.to_string in
    let c = Cstruct.create (String.length s) in
    Cstruct.blit_from_string s 0 c 0 (Cstruct.len c);
    c
end

(** Converts the redo operation to a human-readable string. *)
let redo_to_human_readable op =
  op |> Op.sexp_of_t |> Sexplib.Sexp.to_string 
