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

open OUnit
open Tag

let test_tag_string (should_be_valid, s) =
  Printf.sprintf "Checking if tag '%s' is %svalid" s (if should_be_valid then "" else "in") >:: (fun () ->
    let is_valid = is_valid s in
    assert_equal ~printer:string_of_bool should_be_valid is_valid
  )

let test_strings =
	[false, ""; true, "abc"; false, "----abc"; true, "abc-----"; false, "abc###";
	 true, String.make 128 'y'; false, String.make 129 'n'; true, "_0m_3+3-3.X"]

let tag_suite = "tags" >::: (List.map test_tag_string test_strings)

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "MLVM test suite";

  run_test_tt ~verbose:!verbose tag_suite

