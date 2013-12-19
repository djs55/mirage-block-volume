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
  let data = Cstruct.create sizeof in
  Utils.zero data;
  let _ = marshal empty data in
  assert_equal mda (Cstruct.to_string data)

let unmarshal_marshal_mdaheader () =
  let open Metadata.Header in
  let empty = create () in
  let data = Cstruct.create sizeof in
  Utils.zero data;
  let _ = marshal empty data in
  let empty', _ = Result.ok_or_failwith (unmarshal data) in
  assert_equal ~printer:to_string ~cmp:equals empty empty';
  let _ = marshal empty' data in
  let empty'', _ = Result.ok_or_failwith (unmarshal data) in
  assert_equal ~printer:to_string ~cmp:equals empty' empty''

let mda_suite = "Metadata" >::: [
  "well known MDA header" >:: well_known_mdaheader;
  "unmarshal(marshal(Metadata.Header.create()))" >:: unmarshal_marshal_mdaheader;
]

let label_header = "LABELONE\001\000\000\000\000\000\000\000\000\000\000\000 \000\000\000LVM2 001"

let well_known_label_header () =
  let open Label.Label_header in
  let buf = Cstruct.create 512 in
  let sector = marshal (create ()) buf in
  Utils.zero sector;
  let label_header' = Cstruct.(to_string (sub buf 0 (String.length label_header))) in
  assert_equal label_header label_header'

let label = "LABELONE\001\000\000\000\000\000\000\000<\131@\179 \000\000\000LVM2 001Obwn1MGs3G3TN8Rchuo73nKTT0uLuUxw\210\004\000\000\000\000\000\000,\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000d\000\000\000\000\000\000\000\200\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let well_known_label () =
  let open Label in
  let sector = Cstruct.create 512 in
  Utils.zero sector;
  let uuid = Result.ok_or_failwith (Uuid.of_string "Obwn1M-Gs3G-3TN8-Rchu-o73n-KTT0-uLuUxw") in
  let expected = create "foo" uuid 1234L 100L 200L in
  let _ = marshal expected sector in
  let label' = Cstruct.(to_string (sub sector 0 (String.length label))) in
  assert_equal label label'

let label_suite = "Label header" >::: [
  "well known label header" >:: well_known_label_header;
  "well known label" >:: well_known_label;
]

let pv_header = "Obwn1MGs3G3TN8Rchuo73nKTT0uLuUxw\210\004\000\000\000\000\000\000,\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000d\000\000\000\000\000\000\000\200\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let well_known_pv_header () =
  let open Label.Pv_header in
  let uuid = Result.ok_or_failwith (Uuid.of_string "Obwn1M-Gs3G-3TN8-Rchu-o73n-KTT0-uLuUxw") in
  let pvh = {
    id = uuid;
    device_size = 1234L;
    extents = [{Label.Location.offset = 300L; size = 0L}];
    metadata_areas = [{Label.Location.offset = 100L; size = 200L}]
  } in
  let sector = Cstruct.create 512 in
  let _ = marshal pvh sector in
  let pv_header' = Cstruct.(to_string (sub sector 0 (String.length pv_header))) in
  assert_equal pv_header pv_header'

let unmarshal_marshal_pv_header () =
  let open Label.Pv_header in
  let pvh = create (Uuid.create ()) 1234L 100L 50L in
  let sector = Cstruct.create 512 in
  let _ = marshal pvh sector in
  let pvh', _ = Result.ok_or_failwith (unmarshal sector) in
  assert_equal ~printer:to_string ~cmp:equals pvh pvh'

let pv_header_suite = "PV header" >::: [
  "well known PV header" >:: well_known_pv_header;
  "unmarshal(marshal(Pv_header.create()))" >:: unmarshal_marshal_pv_header;
]

let uuid_unmarshal_error () =
  let open Uuid in
  match unmarshal (Cstruct.create 0) with
  | `Error _ -> ()
  | `Ok _ -> failwith "uuid_unmarshal_error succeeded but should have failed"

let uuid_marshalled = "Obwn1MGs3G3TN8Rchuo73nKTT0uLuUxw"
let uuid_unmarshalled = "Obwn1M-Gs3G-3TN8-Rchu-o73n-KTT0-uLuUxw"

let well_known_uuid () =
  let open Uuid in
  let buf = Cstruct.create (String.length uuid_marshalled) in
  Cstruct.blit_from_string uuid_marshalled 0 buf 0 (String.length uuid_marshalled);
  let uuid, _ = Result.ok_or_failwith (unmarshal buf) in
  assert_equal uuid_unmarshalled (to_string uuid)

let uuid_of_string_error () =
  let open Uuid in
  match of_string "" with
  | `Error _ -> ()
  | `Ok _ -> failwith "uuid_of_string_error should have failed"

let uuid_of_string_ok () =
  let open Uuid in
  let _ = Result.ok_or_failwith (of_string uuid_unmarshalled) in
  ()

let uuid_suite = "Uuid" >::: [
  "unmarshal error" >:: uuid_unmarshal_error;
  "unmarshal well known uuid" >:: well_known_uuid;
  "of_string error" >:: uuid_of_string_error;
  "of_string ok" >:: uuid_of_string_ok;
]

let _ =
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "MLVM test suite";

  List.iter (fun suite -> ignore (run_test_tt ~verbose:!verbose suite)) [
    tag_suite;
    mda_suite;
    label_suite;
    pv_header_suite;
    uuid_suite;
  ]
