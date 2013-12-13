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
    let is_valid = Tag.(to_string (of_string s)) = s in
    assert_equal ~printer:string_of_bool should_be_valid is_valid
  )

let test_strings = [
  false, "";
  true, "abc";
  false, "----abc";
  true, "abc-----";
  false, "abc###";
  true, String.make 128 'y';
  false, String.make 129 'n';
  true, "_0m_3+3-3.X"
]

let tag_suite = "tags" >::: (List.map test_tag_string test_strings)

let mda = "\186\233\186\158 LVM2 x[5A%r0N*>\001\000\000\000\000\016\000\000\000\000\000\000\000\000\160\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let well_known_mdaheader () =
  let open Metadata.Header in
  let empty = create () in
  let data = String.make sizeof '\000', 0 in
  let _ = marshal empty data in
  assert_equal mda (fst data)

let unmarshal_marshal_mdaheader () =
  let open Metadata.Header in
  let empty = create () in
  let data = String.make sizeof '\000', 0 in
  let _ = marshal empty data in
  let empty', _ = unmarshal data in
  assert_equal ~printer:to_string ~cmp:equals empty empty';
  let _ = marshal empty' data in
  let empty'', _ = unmarshal data in
  assert_equal ~printer:to_string ~cmp:equals empty' empty''

let mda_suite = "Metadata" >::: [
  "well known MDA header" >:: well_known_mdaheader;
  "unmarshal(marshal(Metadata.Header.create()))" >:: unmarshal_marshal_mdaheader;
]

let label_header = "LABELONE\001\000\000\000\000\000\000\000\000\000\000\000 \000\000\000LVM2 001"

let well_known_label_header () =
  let open Label.Label_header in
  let buf = String.make 512 '\000' in
  let sector = marshal (create ()) (buf, 0) in
  let label_header' = String.sub (fst sector) 0 (snd sector) in
  assert_equal label_header label_header'

let label = "LABELONE\001\000\000\000\000\000\000\000<\131@\179 \000\000\000LVM2 001Obwn1MGs3G3TN8Rchuo73nKTT0uLuUxw\210\004\000\000\000\000\000\000,\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000d\000\000\000\000\000\000\000\200\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let well_known_label () =
  let open Label in
  let sector = String.make 512 '\000' in
  let expected = create "foo" (Lvm_uuid.of_string "Obwn1M-Gs3G-3TN8-Rchu-o73n-KTT0-uLuUxw") 1234L 100L 200L in
  let _, len = marshal expected (sector, 0) in
  let label' = String.sub sector 0 len in
  assert_equal label label'

let label_suite = "Label header" >::: [
  "well known label header" >:: well_known_label_header;
  "well known label" >:: well_known_label;
]

let pv_header = "Obwn1MGs3G3TN8Rchuo73nKTT0uLuUxw\210\004\000\000\000\000\000\000,\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000d\000\000\000\000\000\000\000\200\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let well_known_pv_header () =
  let open Label.Pv_header in
  let pvh = {
    pvh_id = Lvm_uuid.of_string "Obwn1M-Gs3G-3TN8-Rchu-o73n-KTT0-uLuUxw";
    pvh_device_size = 1234L;
    pvh_extents = [{Label.dl_offset = 300L; Label.dl_size = 0L}];
    pvh_metadata_areas = [{Label.dl_offset = 100L; Label.dl_size = 200L}]
  } in
  let sector = String.make 512 '\000' in
  let _, len = marshal pvh (sector, 0) in
  let pv_header' = String.sub sector 0 len in
  assert_equal pv_header pv_header'

let unmarshal_marshal_pv_header () =
  let open Label.Pv_header in
  let pvh = create (Lvm_uuid.create ()) 1234L 100L 50L in
  let sector = String.make 512 '\000' in
  let _ = marshal pvh (sector, 0) in
  let pvh', _ = unmarshal (sector, 0) in
  assert_equal ~printer:to_string ~cmp:equals pvh pvh'

let pv_header_suite = "PV header" >::: [
  "well known PV header" >:: well_known_pv_header;
  "unmarshal(marshal(Pv_header.create()))" >:: unmarshal_marshal_pv_header;
]


let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "MLVM test suite";

  run_test_tt ~verbose:!verbose tag_suite;
  run_test_tt ~verbose:!verbose mda_suite;
  run_test_tt ~verbose:!verbose label_suite;
  run_test_tt ~verbose:!verbose pv_header_suite

