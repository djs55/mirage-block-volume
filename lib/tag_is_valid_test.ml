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

open Tag

let test_tag_string (should_be_valid, s) =
  let is_valid = is_valid s in
  let is_valid_string = if is_valid then "  VALID" else "INVALID" in
  let result = if is_valid = should_be_valid then "  CORRECT" else "INCORRECT" in
	print_endline (result ^ " --- " ^ is_valid_string ^ " --- '" ^ s ^ "'")

let test_strings =
	[false, ""; true, "abc"; false, "----abc"; true, "abc-----"; false, "abc###";
	 true, String.make 128 'y'; false, String.make 129 'n'; true, "_0m_3+3-3.X"]

let _ = List.map test_tag_string test_strings
