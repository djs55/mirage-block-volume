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
open Result

module CharSet = struct
  include Set.Make(struct type t = char let compare = compare end)

  let range c c' =
    let chars = Char.(Array.(to_list @@ make (code c' - code c + 1) c)) in
    of_list @@ List.mapi (fun i c -> Char.(chr @@ code c + i)) chars

  let of_string s =
    let rec exp acc i = if i < 0 then acc else exp (s.[i] :: acc) (i - 1) in
    of_list @@ exp [] (String.length s - 1)

  let alphanum = 
    List.fold_left union empty [range 'A' 'Z'; range 'a' 'z'; range '0' '9']

  let domain = range (Char.chr 0) (Char.chr 255)

  let dump = iter print_char

  let pp_string_of t =
    elements t |> List.map (Printf.sprintf "'%c'")
    |> String.concat "; " |> Printf.sprintf "{ %s }"
end

module type CONSTRAINTS = sig
  val allow_empty : bool
  val max_length : int
  val valid_chars : CharSet.t
  val reserved_strings : string list
  val reserved_substrings : string list
  val custom_validators : (string * (string -> bool)) list
end

module type Sanitised_string = sig
  type t
  include S.SEXPABLE with type t := t
  include S.PRINT with type t := t
  val of_string : string -> (t, string) Result.result
end


module Make(Constraints : CONSTRAINTS) = struct
  open Constraints
  type t = string with sexp

  let check_empty s =
    if not Constraints.allow_empty && String.length s = 0
    then fail (Printf.sprintf "Empty string not allowed")
    else return ()

  let check_max_length s =
    if String.length s <= Constraints.max_length
    then return ()
    else fail (Printf.sprintf "Exceeds maximum length: max = %d" max_length)

  let check_only_valid_chars s =
    let used_chars = CharSet.of_string s in
    if CharSet.subset used_chars Constraints.valid_chars
    then return ()
    else
      let illegal_chars = CharSet.diff used_chars Constraints.valid_chars in
      let msg = Printf.sprintf "Contains illegal characters: %s"
        (CharSet.pp_string_of illegal_chars) in
      fail msg

  let check_not_reserved_string s =
    if not (List.mem s Constraints.reserved_strings)
    then return ()
    else fail (Printf.sprintf "Reserved string: %s" s)

  let check_no_reserved_substrings s =
    (* TODO: use a more efficient substring algorithm *)
    let in_string s substr =
      if String.(length substr > length s)
      then false
      else begin
        let found = ref false in
        let i = ref 0 in
        while not !found && !i <= String.(length s - length substr) do
          if String.(sub s  !i (length substr)) = substr
          then found := true
          else i := !i + 1
        done;
        !found
      end
    in
    match List.filter (in_string s) Constraints.reserved_substrings with
    | [] -> return ()
    | subs -> fail @@ Printf.sprintf "Reserved substrings: [ %s ]" @@ String.concat "; " subs

  let of_string s =
    check_empty s >>= fun () ->
    check_max_length s >>= fun () ->
    check_only_valid_chars s >>= fun () ->
    check_not_reserved_string s >>= fun () ->
    check_no_reserved_substrings s >>= fun () ->
    let wrap (msg, f) = if f s then return () else fail msg in
    all @@ List.map wrap Constraints.custom_validators >>= fun _ ->
    return s

  let to_string t = t
end

module Unconstrained : CONSTRAINTS = struct
  let allow_empty = true
  let max_length = Sys.max_string_length
  let valid_chars = CharSet.domain
  let reserved_strings = []
  let reserved_substrings = []
  let custom_validators = []
end

(* From `man lvm`:
VALID NAMES
  The following characters are valid for VG and LV names: a-z A-Z 0-9 + _ . -

  VG and LV names cannot begin with a hyphen.  There  are  also  various
  reserved names that are used internally by lvm that can not be used as LV or
  VG names.  A VG cannot be called anything that exists in /dev/ at the time of
  creation,  nor can  it  be  called  '.'  or '..'.  A LV cannot be called '.'
  '..' 'snapshot' or 'pvmove'. The LV name may also  not  contain  the  strings
  '_mlog',  '_mimage', '_rimage', '_tdata', '_tmeta'.
...
  Characters allowed in tags are: A-Z a-z 0-9 _ + . - and as of version
  2.02.78 the following characters are also accepted: / = ! : # &
*)

module Vg_constraints : CONSTRAINTS = struct
  include Unconstrained
  let allow_empty = false
  let max_length = 127
  let valid_chars = CharSet.(union alphanum @@ of_string "_+.-")
  let reserved_strings = ["."; "..";]

  let does_not_start_with_hyphen s = try s.[0] <> '-' with _ -> true
  let custom_validators = [
    "Cannot start with hyphen", does_not_start_with_hyphen;
  ]
end

module Lv_constraints = struct
  include Vg_constraints
  let reserved_strings = ["."; ".."; "snapshot"; "pvmove"]
  let reserved_substrings = ["_mlog"; "_mimage"; "_rimage"; "_tdata"; "_tmeta"]
end

module Tag_constraints = struct
  include Unconstrained
  let allow_empty = false
  let valid_chars = CharSet.(union alphanum @@ of_string "_+.-/=!:#&")
end

module Vg_name = Make(Vg_constraints)
module Lv_name = Make(Lv_constraints)
module Tag = Make(Tag_constraints)
