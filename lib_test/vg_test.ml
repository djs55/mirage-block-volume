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
open Lvm
open Vg
open Lwt

module Log = struct
  let debug fmt = Lwt_log.debug_f fmt
  let info  fmt = Lwt_log.info_f fmt
  let error fmt = Lwt_log.error_f fmt

  let trace _ = Lwt.return ()

  let _ =
    debug "This is the debug output"
    >>= fun () ->
    info "This is the info output"
    >>= fun () ->
    error "This is the error output"
end

let expect_some = function
  | None -> raise (Invalid_argument "None")
  | Some x -> x

let expect_none = function
  | Some _ -> raise (Invalid_argument "Some")
  | None -> ()

let ok_or_failwith x = match Vg.error_to_msg x with
  | `Ok x -> x
  | `Error (`Msg m) -> failwith m

let good_magic = " LVM2 x[5A%r0N*>"
let unmarshal_good_magic () =
  let open Magic in
  let buf = Cstruct.create (String.length good_magic) in
  Cstruct.blit_from_string good_magic 0 buf 0 (String.length good_magic);
  let _ = Result.get_ok (unmarshal buf) in
  ()

let unmarshal_bad_magic () =
  let open Magic in
  let buf = Cstruct.create 0 in
  let _ = Result.get_error (unmarshal buf) in
  let buf = Cstruct.create (String.length good_magic) in
  Cstruct.set_char buf 0 '\000';
  let _ = Result.get_error (unmarshal buf) in
  ()

let magic_suite = "Magic" >::: [
  "unmarshal good magic" >:: unmarshal_good_magic;
  "unmarshal bad magic" >:: unmarshal_bad_magic;
]

let mda = "\186\233\186\158 LVM2 x[5A%r0N*>\001\000\000\000\000\016\000\000\000\000\000\000\000\000\160\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let unmarshal_mdaheader () =
  let open Metadata.Header in
  let empty = create `Lvm in
  let data = Cstruct.create sizeof in
  Utils.zero data;
  let _ = marshal empty data in
  Cstruct.set_char data 0 '0'; (* corrupt *)
  let _ = Result.get_error (unmarshal data) in
  ()

let well_known_mdaheader () =
  let open Metadata.Header in
  let empty = create `Lvm in
  let data = Cstruct.create sizeof in
  Utils.zero data;
  let _ = marshal empty data in
  assert_equal mda (Cstruct.to_string data)

let unmarshal_marshal_mdaheader () =
  let open Metadata.Header in
  let empty = create `Lvm in
  let data = Cstruct.create sizeof in
  Utils.zero data;
  let _ = marshal empty data in
  let empty', _ = Result.get_ok (unmarshal data) in
  assert_equal ~printer:to_string ~cmp:equals empty empty';
  let _ = marshal empty' data in
  let empty'', _ = Result.get_ok (unmarshal data) in
  assert_equal ~printer:to_string ~cmp:equals empty' empty''

let mdaheader_prettyprint () =
  let open Metadata.Header in
  let a = create `Lvm in
  let b = create `Lvm in
  let c = create `Journalled in
  let a' = t_of_sexp (sexp_of_t a) in
  let b' = t_of_sexp (sexp_of_t b) in
  let c' = t_of_sexp (sexp_of_t c) in
  assert_equal ~printer:to_string a' b';
  assert_equal false (equals b' c')

let mda_suite = "Metadata" >::: [
  "well known MDA header" >:: well_known_mdaheader;
  "unmarshalling MDA headers" >:: unmarshal_mdaheader;
  "unmarshal(marshal(Metadata.Header.create()))" >:: unmarshal_marshal_mdaheader;
  "mdaheader prettyprint" >:: mdaheader_prettyprint;
]

let label_header = "LABELONE\001\000\000\000\000\000\000\000\000\000\000\000 \000\000\000LVM2 001"

let well_known_label_header () =
  let open Label.Label_header in
  let buf = Cstruct.create 512 in
  let sector = marshal (create `Lvm) buf in
  Utils.zero sector;
  let label_header' = Cstruct.(to_string (sub buf 0 (String.length label_header))) in
  assert_equal label_header label_header'

let label_header_equals () =
  let open Label.Label_header in
  let a = create `Lvm in
  let a' = create `Lvm in
  let b = create `Journalled in
  assert_equal ~cmp:equals a a';
  assert_equal false (equals a b);
  let txt = to_string a in
  let a'' = t_of_sexp (Sexplib.Sexp.of_string txt) in
  assert_equal ~cmp:equals a a''

let label = "LABELONE\001\000\000\000\000\000\000\000<\131@\179 \000\000\000LVM2 001Obwn1MGs3G3TN8Rchuo73nKTT0uLuUxw\210\004\000\000\000\000\000\000,\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000d\000\000\000\000\000\000\000\200\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let well_known_label () =
  let open Label in
  let sector = Cstruct.create 512 in
  Utils.zero sector;
  let uuid = Result.get_ok (Uuid.of_string "Obwn1M-Gs3G-3TN8-Rchu-o73n-KTT0-uLuUxw") in
  let expected = create uuid 1234L 100L 200L in
  let _ = marshal expected sector in
  let label' = Cstruct.(to_string (sub sector 0 (String.length label))) in
  assert_equal label label';
  let txt = to_string expected in
  let expected' = t_of_sexp (Sexplib.Sexp.of_string txt) in
  assert_equal ~cmp:equals expected expected'

let label_suite = "Label header" >::: [
  "well known label header" >:: well_known_label_header;
  "label header equality" >:: label_header_equals;
  "well known label" >:: well_known_label;
]

let pv_header = "Obwn1MGs3G3TN8Rchuo73nKTT0uLuUxw\210\004\000\000\000\000\000\000,\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000d\000\000\000\000\000\000\000\200\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let well_known_pv_header () =
  let open Label.Pv_header in
  let uuid = Result.get_ok (Uuid.of_string "Obwn1M-Gs3G-3TN8-Rchu-o73n-KTT0-uLuUxw") in
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
  let pvh', _ = Result.get_ok (unmarshal sector) in
  assert_equal ~printer:to_string ~cmp:equals pvh pvh'

let pv_header_suite = "PV header" >::: [
  "well known PV header" >:: well_known_pv_header;
  "unmarshal(marshal(Pv_header.create()))" >:: unmarshal_marshal_pv_header;
]

let uuid_unmarshal_error () =
  let open Uuid in
  let _ = Result.get_error (unmarshal (Cstruct.create 0)) in
  ()

let uuid_marshalled = "Obwn1MGs3G3TN8Rchuo73nKTT0uLuUxw"
let uuid_unmarshalled = "Obwn1M-Gs3G-3TN8-Rchu-o73n-KTT0-uLuUxw"

let well_known_uuid () =
  let open Uuid in
  let buf = Cstruct.create (String.length uuid_marshalled) in
  Cstruct.blit_from_string uuid_marshalled 0 buf 0 (String.length uuid_marshalled);
  let uuid, _ = Result.get_ok (unmarshal buf) in
  assert_equal uuid_unmarshalled (to_string uuid)

let uuid_of_string_error () =
  let open Uuid in
  let _ = Result.get_error (of_string "") in
  ()

let uuid_of_string_ok () =
  let open Uuid in
  let _ = Result.get_ok (of_string uuid_unmarshalled) in
  ()

let uuid_suite = "Uuid" >::: [
  "unmarshal error" >:: uuid_unmarshal_error;
  "unmarshal well known uuid" >:: well_known_uuid;
  "of_string error" >:: uuid_of_string_error;
  "of_string ok" >:: uuid_of_string_ok;
]

let test_lv_string (should_be_valid, s) =
  Printf.sprintf "Checking if LV name '%s' is %svalid" s (if should_be_valid then "" else "in") >:: (fun () ->
    let is_valid = match Name.Lv_name.of_string s with `Ok _ -> true | _ -> false in
    assert_equal ~printer:string_of_bool should_be_valid is_valid
  )

let test_strings = [
  false, "";
  true, "foo";
  false, ".";
  false, "foo_rimage";
  false, "snapshot";
  false, "\000";
  false, String.make 128 'X';
]

let lv_name_suite = "LV names" >::: (List.map test_lv_string test_strings)

let test_tag_string (should_be_valid, s) =
  Printf.sprintf "Checking if tag '%s' is %svalid" s (if should_be_valid then "" else "in") >:: (fun () ->
    let is_valid = match Name.Tag.of_string s with `Ok _ -> true | _ -> false in
    assert_equal ~printer:string_of_bool should_be_valid is_valid
  )

let test_strings = [
  false, "";
  true, "abc";
  true, "_0m_3+3-3.X"
]

let tag_suite = "tags" >::: (List.map test_tag_string test_strings)

module Time = struct
  type 'a io = 'a Lwt.t
  let sleep = Lwt_unix.sleep
end

module Vg_IO = Vg.Make(Log)(Block)(Time)(Clock)

let (>>*=) m f = match Lvm.Vg.error_to_msg m with
  | `Error (`Msg e) -> fail (Failure e)
  | `Ok x -> f x
let (>>|=) m f = m >>= fun x -> x >>*= f

let with_dummy fn =
  let filename = "/tmp/vg" in
  let f = Unix.openfile filename [Unix.O_CREAT; Unix.O_RDWR; Unix.O_TRUNC] 0o644 in
  (* approximately 10000 4MiB extents for volumes, 100MiB for metadata and
     overhead *)
  let _ = Unix.lseek f (1024*1024*4*10100 - 1) Unix.SEEK_SET in
  ignore(Unix.write f "\000" 0 1);
  Unix.close f;
  let result = fn filename in
  Unix.unlink filename;
  result
  (* NB we leak the file on error, but this is quite useful *)

let with_block filename f =
  let open Lwt in
  Block.connect (Printf.sprintf "buffered:%s" filename)
  >>= function
  | `Error x ->
    fail (Failure (Printf.sprintf "Unable to read %s" filename))
  | `Ok x ->
    f x (* no point catching errors here *)

let pv = Result.get_ok (Pv.Name.of_string "pv0")
let small = Int64.(mul (mul 1024L 1024L) 4L)
let small_extents = 1L

let lv_not_formatted () =
  let open Vg_IO in
  with_dummy (fun filename ->
      let t = 
        with_block filename
          (fun block ->
            Vg_IO.connect ~flush_interval:0. [] `RW >>= fun t ->
            let _ = Result.get_error t in
            Vg_IO.connect ~flush_interval:0. [ block ] `RW >>= fun t ->
            let _ = Result.get_error t in
            return ()
          )
      in
      Lwt_main.run t)

let pv_wipe () =
  let open Vg_IO in
  let module Pv_IO = Lvm.Pv.Make(Block) in
  with_dummy (fun filename ->
      let t = 
        with_block filename
          (fun block ->
            Vg_IO.format "vg" [ pv, block ] >>|= fun () ->
            Pv_IO.wipe block >>= fun t ->
            let _ = Result.get_ok t in
            Vg_IO.connect ~flush_interval:0. [ block ] `RW >>= fun t ->
            let _ = Result.get_error t in
            Pv_IO.unwipe block >>= fun t ->
            let _ = Result.get_ok t in
            Vg_IO.connect ~flush_interval:0. [ block ] `RW >>|= fun vg ->
            Lwt.return ()
          )
      in
      Lwt_main.run t)

let lv_name_clash () =
  let open Vg_IO in
  with_dummy (fun filename ->
      let t = 
        with_block filename
          (fun block ->
            Vg_IO.format "vg" [ pv, block ] >>|= fun () ->
            Vg_IO.connect ~flush_interval:0. [ block ] `RW >>|= fun vg ->
            Vg.create (Vg_IO.metadata_of vg) "name" small >>*= fun (md,_) ->
            let _ = Result.get_error (Vg.create md "name" small) in
            Lwt.return ()
          )
      in
      Lwt_main.run t)

let tag = "tag"

let lv_create magic () =
  let open Vg_IO in
  with_dummy (fun filename ->
      let t = 
        with_block filename
          (fun block ->
            Vg_IO.format ~magic "vg" [ pv, block ] >>|= fun () ->
            Vg_IO.connect ~flush_interval:0. [ block ] `RW >>|= fun vg ->
            let free_extents = Pv.Allocator.size (Vg_IO.metadata_of vg).Vg.free_space in
            let free_bytes = Int64.(mul 512L (mul (Vg_IO.metadata_of vg).Vg.extent_size free_extents)) in
            let _ =
              Vg.create (Vg_IO.metadata_of vg) "toobig" (Int64.succ free_bytes)
              |> Vg.error_to_msg |> Result.get_error in
            Vg.create (Vg_IO.metadata_of vg) ~tags:[tag] "name" ~status:Lv.Status.([Read; Write; Visible]) small >>*= fun (md, op) ->
            let _ = Vg.create md "name" small |> Vg.error_to_msg |> Result.get_error in
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let md' = Vg_IO.metadata_of vg in
            let md' = Vg.metadata_of_sexp (Vg.sexp_of_metadata md') in
            assert_equal (Vg.to_string md) (Vg.to_string md');
            let id = expect_some (Vg_IO.find vg "name") in
            let v_md = Vg_IO.Volume.metadata_of id in
            assert_equal ~printer:(fun x -> x) "name" v_md.Lv.name;
            assert_equal ~printer:Int64.to_string small_extents (Pv.Allocator.size (Lv.to_allocation v_md));
            (* Re-read the metadata and check it matches *)
            Vg_IO.connect ~flush_interval:0. [ block ] `RO >>|= fun vg' ->
            let id = expect_some (Vg_IO.find vg "name") in
            let v_md' = Vg_IO.Volume.metadata_of id in
            assert_equal ~printer:(fun x -> Sexplib.Sexp.to_string_hum (Lv.sexp_of_t x)) v_md v_md';
            (* While we're here, check we can't update a RO VG *)
            Vg_IO.update vg' [] >>= fun t ->
            let _ = t |> Vg.error_to_msg |> Result.get_error in
            Lwt.return ()
          )
      in
      Lwt_main.run t)

let lv_rename () =
  let open Vg_IO in
  with_dummy (fun filename ->
      let t = 
        with_block filename
          (fun block ->
            Vg_IO.format ~magic:`Journalled "vg" [ pv, block ] >>|= fun () ->
            Vg_IO.connect ~flush_interval:0. [ block ] `RW >>|= fun vg ->
            Vg.create (Vg_IO.metadata_of vg) ~tags:[tag] "name" ~status:Lv.Status.([Read; Write; Visible]) small >>*= fun (_,op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            Vg.rename (Vg_IO.metadata_of vg) "name" "name2" >>*= fun (_, op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let _ = expect_some (Vg_IO.find vg "name2") in
            expect_none (Vg_IO.find vg "name");
            Lwt.return ()
          )
      in
      Lwt_main.run t)
let bigger = Int64.mul small 2L
let bigger_extents = Int64.mul small_extents 2L

let lv_resize () =
  let open Vg_IO in
  with_dummy (fun filename ->
      let t = 
        with_block filename
          (fun block ->
            Vg_IO.format ~magic:`Journalled "vg" [ pv, block ] >>|= fun () ->
            Vg_IO.connect ~flush_interval:0. [ block ] `RW >>|= fun vg ->
            Vg.create (Vg_IO.metadata_of vg) "name" bigger >>*= fun (_,op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let _ =
              Vg.resize (Vg_IO.metadata_of vg) "doesntexist" 0L
              |> Vg.error_to_msg |> Result.get_error in
            let id = expect_some (Vg_IO.find vg "name") in
            let v_md = Vg_IO.Volume.metadata_of id in
            assert_equal ~printer:Int64.to_string bigger_extents (Pv.Allocator.size (Lv.to_allocation v_md));
            Vg.resize (Vg_IO.metadata_of vg) "name" small >>*= fun (_, op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let id = expect_some (Vg_IO.find vg "name") in
            let v_md = Vg_IO.Volume.metadata_of id in
            assert_equal ~printer:Int64.to_string small_extents (Pv.Allocator.size (Lv.to_allocation v_md));
            Vg.resize (Vg_IO.metadata_of vg) "name" bigger >>*= fun (_, op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let id = expect_some (Vg_IO.find vg "name") in
            let v_md = Vg_IO.Volume.metadata_of id in
            assert_equal ~printer:Int64.to_string bigger_extents (Pv.Allocator.size (Lv.to_allocation v_md));
            (* Use up all the space in the VG *)
            let vg_md = Vg_IO.metadata_of vg in
            let free_space = Int64.(mul (mul 512L vg_md.Vg.extent_size) (Pv.Allocator.size vg_md.Vg.free_space)) in
            let max_size = Int64.add bigger free_space in
            Vg.resize vg_md "name" max_size >>*= fun (_, op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            Lwt.catch (fun () ->
              Vg.resize (Vg_IO.metadata_of vg) "name" (Int64.succ max_size) >>*= fun _ ->
              Lwt.return ()
            ) (fun _ ->
              Lwt.return ()
            )
          )
      in
      Lwt_main.run t)

let lv_expand_transfer () =
  (* get some metadata to play around with *)
  let md =
    with_dummy (fun filename ->
      with_block filename (fun block ->
        Vg_IO.format ~magic:`Journalled "vg" [ pv, block ] >>|= fun () ->
        Vg_IO.connect ~flush_interval:5. [ block ] `RW >>|= fun vg ->
        Vg_IO.metadata_of vg |> return
      )
    ) |> Lwt_main.run in
  (* Make some LVs *)
  let open Lv.Segment in
  let lv0 =
    let segment = {
      start_extent=0L; extent_count=5L;
      cls=Lv.Linear.(Linear {name=pv; start_extent=10L})
    } in
    Lv.({name="lv0"; creation_host=""; creation_time=0L; id=(Uuid.create ()); tags=[]; status=[]; segments=[segment]}) in
  let lv1 =
    let segment = {
      start_extent=0L; extent_count=5L;
      cls=Lv.Linear.(Linear {name=pv; start_extent=15L})
    } in
    {lv0 with Lv.name="lv1"; id=(Uuid.create ()); segments=[segment]} in
  let open Redo.Op in
  (* Helper monad for applying operations to metadata *)
  let (>>!=) m f = match Vg.error_to_msg m with
  | `Ok (md', _) -> f md'
  | `Error (`Msg m) -> failwith m in
  (* Create the two LVs *)
  Vg.do_op md (LvCreate lv0) >>!= fun md ->
  Vg.do_op md (LvCreate lv1) >>!= fun md ->
  (* Check they exist (2 LVs + 1 LV for the redo log) *)
  assert_equal ~msg:"Couldn't create LVs" ~printer:string_of_int
    3 (LVs.cardinal md.Vg.lvs);
  (* Extend lv0 with a new segment *)
  let free_space = md.Vg.free_space in
  let segment = {
    start_extent=5L; extent_count=5L;
    cls=Lv.Linear.(Linear {name=pv; start_extent=20L})
  } in
  Vg.do_op md (LvExpand (lv0.Lv.id, {lvex_segments=[segment]})) >>!= fun md ->
  (* Check there is less free_space *)
  let open Pv.Allocator in
  let free_space' = md.Vg.free_space in
  assert_equal ~msg:"LvExpand lv0 did not reduce free_space" ~printer:Int64.to_string
    (Int64.sub (size free_space) 5L) (size free_space');
  (* Check the size of the lv0 allocation has increased by the right amount *)
  let alloc_of_segs ss = List.map to_allocation ss |> List.fold_left merge [] in
  let expected_lv0_alloc = alloc_of_segs (segment::lv0.Lv.segments) in
  let actual_lv0_alloc = alloc_of_segs (LVs.find lv0.Lv.id md.Vg.lvs).Lv.segments in
  assert_equal ~msg:"LvExpand lv0 did not increase lv0 size" ~printer:Int64.to_string
    (Int64.add (size (alloc_of_segs lv0.Lv.segments)) 5L) (size (actual_lv0_alloc));
  (* Check that the allocation is _exactly_ what we expect *)
  assert_equal ~msg:"LvExpand lv0 did not give expected allocation" ~printer:to_string
    expected_lv0_alloc actual_lv0_alloc;
  (* Transfer the segment from lv0 to lv1 *)
  Vg.do_op md (LvTransfer (lv0.Lv.id, lv1.Lv.id, [segment])) >>!= fun md ->
  (* Check that the free_space is unchanged *)
  assert_equal ~msg:"LvTransfer lv0->lv1 changed the free_space" ~printer:to_string
    free_space' md.Vg.free_space;
  (* Check that the segments are in lv1 *)
  let expected_lv1_alloc = alloc_of_segs (segment::lv1.Lv.segments) in
  let actual_lv1_alloc = alloc_of_segs (LVs.find lv1.Lv.id md.Vg.lvs).Lv.segments in
  assert_equal ~msg:"LvTransfer lv0->lv1 didn't add segs to lv1" ~printer:to_string
    expected_lv1_alloc actual_lv1_alloc;
  (* Check that the segments are not in lv0 *)
  assert_equal ~msg:"LvTransfer lv0->lv1 didn't remove segs from lv0"
    ~printer:(fun x -> Lv.sexp_of_t x |> Sexplib.Sexp.to_string_hum)
    lv0 (LVs.find lv0.Lv.id md.Vg.lvs);
  ()

let lv_remove () =
  let open Vg_IO in
  with_dummy (fun filename ->
      let t = 
        with_block filename
          (fun block ->
            Vg_IO.format ~magic:`Journalled "vg" [ pv, block ] >>|= fun () ->
            Vg_IO.connect ~flush_interval:0. [ block ] `RW >>|= fun vg ->
            Vg.create (Vg_IO.metadata_of vg) ~tags:[tag] "name" ~status:Lv.Status.([Read; Write; Visible]) small >>*= fun (_,op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            Vg.remove (Vg_IO.metadata_of vg) "name" >>*= fun (_, op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            expect_none (Vg_IO.find vg "name");
            Lwt.return ()
          )
      in
      Lwt_main.run t)

(* test the metadata area circular buffer logic *)
let lv_lots_of_ops () =
  let open Vg_IO in
  with_dummy (fun filename ->
      let t = 
        with_block filename
          (fun block ->
            Vg_IO.format ~magic:`Lvm "vg" [ pv, block ] >>|= fun () ->
            Vg_IO.connect ~flush_interval:0. [ block ] `RW >>|= fun vg ->
            let rec loop = function
            | 0 -> return ()
            | n ->
              let name = Printf.sprintf "lv%d" n in
              Vg.create (Vg_IO.metadata_of vg) ~tags:[tag] name ~status:Lv.Status.([Read; Write; Visible]) small >>*= fun (_,op) ->
              Vg_IO.update vg [ op ] >>|= fun () ->
              Vg_IO.sync vg >>|= fun () ->
              let (_: Vg_IO.Volume.id) = expect_some (Vg_IO.find vg name) in
              Vg.remove (Vg_IO.metadata_of vg) name >>*= fun (_, op) ->
              Vg_IO.update vg [ op ] >>|= fun () ->
              Vg_IO.sync vg >>|= fun () ->
              loop (n - 1) in
            loop 1000 (* hopefully this fills up the 10M metadata area *)
          )
      in
      Lwt_main.run t)

(*BISECT-IGNORE-BEGIN*)
(* test lots of out-of-sync operations *)
let lv_lots_of_out_of_sync () =
  let open Vg_IO in
  with_dummy (fun filename ->
      let t =
        with_block filename
          (fun block ->
            Vg_IO.format ~magic:`Journalled "vg" [ pv, block ] >>|= fun () ->
            Vg_IO.connect ~flush_interval:5. [ block ] `RW >>|= fun vg ->
            let name n = Printf.sprintf "lv%d" n in
            let rec loop f = function
            | 0 -> return ()
            | n ->
              f n
              >>= fun () ->
              loop f (n - 1) in
            let n = 1000 in
            loop (fun n ->
              let name = name n in
              Vg.create (Vg_IO.metadata_of vg) ~tags:[tag] name ~status:Lv.Status.([Read; Write; Visible]) small >>*= fun (_,op) ->
              Vg_IO.update vg [ op ] >>|= fun () ->
              let (_: Vg_IO.Volume.id) = expect_some (Vg_IO.find vg name) in
              return ()
            ) n
            >>= fun () ->
            (* One more for the redo log *)
            assert_equal ~printer:string_of_int (n + 1) (Vg.LVs.cardinal (Vg_IO.metadata_of vg).Vg.lvs);
            loop (fun n ->
              let name = name n in
              Vg.remove (Vg_IO.metadata_of vg) name >>*= fun (_, op) ->
              Vg_IO.update vg [ op ] >>|= fun () ->
              expect_none (Vg_IO.find vg name);
              return ()
            ) n
            >>= fun () ->
            (* One more for the redo log *)
            assert_equal ~printer:string_of_int 1 (Vg.LVs.cardinal (Vg_IO.metadata_of vg).Vg.lvs);
            Vg_IO.sync vg >>|= fun () ->
            return ()
          )
      in
      Lwt_main.run t)
(*BISECT-IGNORE-END*)

let lv_op_idempotence () =
  let ok_or_failwith x = match Vg.error_to_msg x with
  | `Ok x -> x
  | `Error (`Msg m) -> failwith m
  in
  let test_op md op =
    let (md', _) = Vg.do_op md op |> ok_or_failwith in
    let (md'', _) = Vg.do_op md' op |> ok_or_failwith in
    let printer m = Sexplib.Sexp.to_string_hum @@ Vg.sexp_of_metadata m in
    let msg x =
      Printf.sprintf "Expected %s op to be %s but %s observed"
      (Sexplib.Sexp.to_string_hum @@ Redo.Op.sexp_of_t op)
      (match x with `Potent -> "effectual" | `Impotent -> "ineffectual")
      (match x with `Potent -> "no change" | `Impotent -> "change") in
    assert_equal ~msg:(msg `Potent) ~printer ~cmp:(<>) md md';
    assert_equal ~msg:(msg `Impotent) ~printer ~cmp:(=) md' md'';
    md''
  in
  (* get some metadata to play around with *)
  with_dummy (fun filename ->
    with_block filename (fun block ->
      Vg_IO.format ~magic:`Journalled "vg" [ pv, block ] >>|= fun () ->
      Vg_IO.connect ~flush_interval:5. [ block ] `RW >>|= fun vg ->
      Vg_IO.metadata_of vg |> return
    )
  ) |> Lwt_main.run |> fun init_md ->
  let open Lv.Segment in
  let segment = {
    start_extent=0L; extent_count=2L;
    cls=Lv.Linear.(Linear {name=pv; start_extent=8L})
  } in
  let segment' = {
    start_extent=9L; extent_count=2L;
    cls=Lv.Linear.(Linear {name=pv; start_extent=8L})
  } in
  let lv = Lv.({name="lv0"; creation_host=""; creation_time=0L; id=(Uuid.create ()); tags=[]; status=[]; segments=[segment]}) in
  let lv' = Lv.({name="lv1"; creation_host=""; creation_time=0L; id=(Uuid.create ()); tags=[]; status=[]; segments=[segment']}) in
  let open Redo.Op in
  let ops_to_test = [
    LvCreate lv;
    LvCreate lv';
    LvReduce(lv.Lv.id, {lvrd_new_extent_count=1L});
    LvExpand(lv.Lv.id, {lvex_segments=[segment]});
    LvTransfer(lv.Lv.id, lv'.Lv.id, [segment]);
    LvAddTag(lv.Lv.id, Name.Tag.of_string "tag" |> Result.get_ok);
    LvRemoveTag(lv.Lv.id, Name.Tag.of_string "tag" |> Result.get_ok);
    LvSetStatus(lv.Lv.id, Lv.Status.([Read; Write; Visible]));
    LvRename(lv.Lv.id, {lvmv_new_name="lv1"});
    LvRemove(lv.Lv.id);
  ] in
  let _ = function (* Just to make sure this test catches all the ops *)
  | LvCreate _ | LvRemove _ | LvRename _ | LvReduce _ | LvExpand _
  | LvAddTag _ | LvRemoveTag _ | LvSetStatus _ | LvTransfer _ -> () in
  List.fold_left test_op init_md ops_to_test |> ignore

let lv_tags () =
  let open Vg_IO in
  with_dummy (fun filename ->
      let t = 
        with_block filename
          (fun block ->
            Vg_IO.format ~magic:`Journalled "vg" [ pv, block ] >>|= fun () ->
            Vg_IO.connect ~flush_interval:0. [ block ] `RW >>|= fun vg ->
            Vg.create (Vg_IO.metadata_of vg) "name" ~status:Lv.Status.([Read; Write; Visible]) small >>*= fun (_,op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let id = expect_some (Vg_IO.find vg "name") in
            let v_md = Vg_IO.Volume.metadata_of id in
            assert_equal [] v_md.Lv.tags;
            (* first one that doesn't exist *)
            let _ = Result.get_error (Vg.add_tag (Vg_IO.metadata_of vg) "doesntexist" tag) in
            Vg.add_tag (Vg_IO.metadata_of vg) "name" tag >>*= fun (_, op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let id = expect_some (Vg_IO.find vg "name") in
            let v_md = Vg_IO.Volume.metadata_of id in
            assert_equal [tag] (List.map Name.Tag.to_string v_md.Lv.tags);
            (* add it again for no change *)
            Vg.add_tag (Vg_IO.metadata_of vg) "name" tag >>*= fun (_, op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let id = expect_some (Vg_IO.find vg "name") in
            let v_md = Vg_IO.Volume.metadata_of id in
            assert_equal [tag] (List.map Name.Tag.to_string v_md.Lv.tags);
            Vg.remove_tag (Vg_IO.metadata_of vg) "name" tag >>*= fun (_, op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let id = expect_some (Vg_IO.find vg "name") in
            let v_md = Vg_IO.Volume.metadata_of id in
            assert_equal [] (List.map Name.Tag.to_string v_md.Lv.tags);
            Lwt.return ()
          )
      in
      Lwt_main.run t)

let lv_status () =
  let open Vg_IO in
  with_dummy (fun filename ->
      let t = 
        with_block filename
          (fun block ->
            Vg_IO.format ~magic:`Journalled "vg" [ pv, block ] >>|= fun () ->
            Vg_IO.connect ~flush_interval:0. [ block ] `RW >>|= fun vg ->
            Vg.create (Vg_IO.metadata_of vg) "name" ~status:Lv.Status.([Read; Write; Visible]) small >>*= fun (_,op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let id = expect_some (Vg_IO.find vg "name") in
            let v_md = Vg_IO.Volume.metadata_of id in
            assert_equal Lv.Status.([Read; Write; Visible]) v_md.Lv.status;
            (* first one that doesn't exist *)
            Vg.set_status (Vg_IO.metadata_of vg) "name" Lv.Status.([Read]) >>*= fun (_, op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let id = expect_some (Vg_IO.find vg "name") in
            let v_md = Vg_IO.Volume.metadata_of id in
            assert_equal Lv.Status.([Read]) v_md.Lv.status;
            Lwt.return ()
          )
      in
      Lwt_main.run t)
    

let vg_suite = "Vg" >::: [
    "LV not formatted" >:: lv_not_formatted;
    "Wipe and unwipe a PV" >:: pv_wipe;
    "LV name clash" >:: lv_name_clash;
    "LV create without redo" >:: lv_create `Lvm;
    "LV create with redo" >:: lv_create `Journalled;
    "LV rename" >:: lv_rename;
    "LV resize" >:: lv_resize;
    "LV remove" >:: lv_remove;
    "LV tags" >:: lv_tags;
    "LV status" >:: lv_status;
    "LV lots of ops" >:: lv_lots_of_ops;
    "LV op idempotence" >:: lv_op_idempotence;
    "LV expand and transfer" >:: lv_expand_transfer;
    (* XXX: this test fails on travis-- problem in the journal code?
    "LV lots of out-of-sync" >:: lv_lots_of_out_of_sync;
    *)
  ]

open Pv.Allocator

type 'a l =
  | Cons: 'a * ('a l Lazy.t) -> 'a l
  | Nil: 'a l

let rec of_list = function
  | [] -> Nil
  | x :: xs -> Cons(x, lazy(of_list xs))

let rec map f = function
  | Nil -> Nil
  | Cons (hd, tl) -> Cons(f hd, lazy(map f (Lazy.force tl)))

let rec iter f = function
  | Nil -> ()
  | Cons (x, xs) -> f x; iter f (Lazy.force xs)

let rec append xs ys = match xs with
  | Nil -> Lazy.force ys
  | Cons (x, xs) -> Cons(x, lazy(append (Lazy.force xs) ys))

let rec allpairs xs = function
  | Nil -> Nil
  | Cons (y, ys) ->
    append (map (fun x -> (x, y)) xs) (lazy(allpairs xs (Lazy.force ys)))

let pv0 = Result.get_ok (Pv.Name.of_string "pv0")

let vectors: Lvm.Pv.Allocator.t l =
  [ 0L; 1L; 2L; 4L; 10L ]
  |> of_list
  |> map (create pv0)
  |> fun x -> map (fun (a, b) -> merge a b) (allpairs x x)
  |> fun x -> map (fun (a, b) -> sub a b)   (allpairs x x)

let cmp x y = Pv.Allocator.compare x y = 0

let forall f = iter f (allpairs vectors vectors)

let equal = assert_equal ~printer:to_string ~cmp

let allocator_suite = "Allocator" >::: [
  "merge (sub x y) y == merge x y" >:: (fun () -> forall (fun (x, y) -> equal (merge x y) (merge (sub x y) y)));
  "merge (sub (sub x y) y) y == merge x y" >:: (fun () -> forall (fun (x, y) -> equal (merge x y) (merge (sub (sub x y) y) y)));
  "merge (merge (sub x y) y) y == merge x y" >:: (fun () -> forall (fun (x, y) -> equal (merge x y) (merge (merge (sub x y) y) y)));
]

let _ =
  List.iter (fun suite -> ignore (run_test_tt suite)) [
    magic_suite;
    mda_suite;
    label_suite;
    pv_header_suite;
    uuid_suite;
    lv_name_suite;
    tag_suite;
    vg_suite;
    allocator_suite;
  ]
