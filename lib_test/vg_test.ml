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
  let debug fmt = Printf.ksprintf (fun s -> print_endline s) fmt
  let info  fmt = Printf.ksprintf (fun s -> print_endline s) fmt
  let error fmt = Printf.ksprintf (fun s -> print_endline s) fmt

  let _ =
    debug "This is the debug output";
    info "This is the info output";
    error "This is the error output"
end

let ok_or_fail = function
  | None -> raise (Invalid_argument "None")
  | Some x -> x

let mda = "\186\233\186\158 LVM2 x[5A%r0N*>\001\000\000\000\000\016\000\000\000\000\000\000\000\000\160\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

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
  let empty', _ = Result.ok_or_failwith (unmarshal data) in
  assert_equal ~printer:to_string ~cmp:equals empty empty';
  let _ = marshal empty' data in
  let empty'', _ = Result.ok_or_failwith (unmarshal data) in
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
  assert_equal false (equals a b)

let label = "LABELONE\001\000\000\000\000\000\000\000<\131@\179 \000\000\000LVM2 001Obwn1MGs3G3TN8Rchuo73nKTT0uLuUxw\210\004\000\000\000\000\000\000,\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000d\000\000\000\000\000\000\000\200\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let well_known_label () =
  let open Label in
  let sector = Cstruct.create 512 in
  Utils.zero sector;
  let uuid = Result.ok_or_failwith (Uuid.of_string "Obwn1M-Gs3G-3TN8-Rchu-o73n-KTT0-uLuUxw") in
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

module Vg_IO = Vg.Make(Log)(Block)

let (>>|=) m f = m >>= function
  | `Error e -> fail (Failure e)
  | `Ok x -> f x
let (>>*=) m f = match m with
  | `Error e -> fail (Failure e)
  | `Ok x -> f x

let with_dummy fn =
  let filename = "/tmp/vg" in
  let f = Unix.openfile filename [Unix.O_CREAT; Unix.O_RDWR; Unix.O_TRUNC] 0o644 in
  let _ = Unix.lseek f (1024*1024*100 - 1) Unix.SEEK_SET in
  ignore(Unix.write f "\000" 0 1);
  Unix.close f;
  try 
    let result = fn filename in
    Unix.unlink filename;
    result
  with e ->
    Unix.unlink filename;
    raise e

let expect_failure f x =
  match f x with 
  | `Ok _ -> `Error "Expected failure"
  | `Error _ -> `Ok ()

let with_block filename f =
  let open Lwt in
  Block.connect (Printf.sprintf "buffered:%s" filename)
  >>= function
  | `Error (`Unknown s) ->
    fail (Failure (Printf.sprintf "Unable to read %s (Unknown: %s)" filename s))
  | `Error x ->
    fail (Failure (Printf.sprintf "Unable to read %s (other)" filename))
  | `Ok x ->
    Lwt.catch (fun () -> f x) (fun e -> Block.disconnect x >>= fun () -> fail e)

let pv = match Pv.Name.of_string "pv0" with `Ok x -> x | `Error _ -> assert false
let small = Int64.(mul (mul 1024L 1024L) 4L)
let small_extents = 1L

let lv_name_clash () =
  let open Vg_IO in
  with_dummy (fun filename ->
      let t = 
        with_block filename
          (fun block ->
            Vg_IO.format "vg" [ pv, block ] >>|= fun () ->
            Vg_IO.connect [ block ] `RW >>|= fun vg ->
            Vg.create (Vg_IO.metadata_of vg) "name" small >>*= fun (md,_) ->
            expect_failure (Vg.create md "name") small >>*=
            Lwt.return
          )
      in
      Lwt_main.run t)

let tag = Tag.of_string "tag"

let lv_create magic () =
  let open Vg_IO in
  with_dummy (fun filename ->
      let t = 
        with_block filename
          (fun block ->
            Vg_IO.format ~magic "vg" [ pv, block ] >>|= fun () ->
            Vg_IO.connect [ block ] `RW >>|= fun vg ->
            Vg.create (Vg_IO.metadata_of vg) ~tags:[tag] "name" ~status:Lv.Status.([Read; Write; Visible]) small >>*= fun (md, op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let md' = Vg_IO.metadata_of vg in
            assert_equal (Vg.to_string md) (Vg.to_string md');
            let id = ok_or_fail (Vg_IO.find vg "name") in
            let v_md = Vg_IO.Volume.metadata_of id in
            assert_equal ~printer:(fun x -> x) "name" v_md.Lv.name;
            assert_equal ~printer:Int64.to_string small_extents (Pv.Allocator.size (Lv.to_allocation v_md));
            (* Re-read the metadata and check it matches *)
            Vg_IO.connect [ block ] `RO >>|= fun vg' ->
            let id = ok_or_fail (Vg_IO.find vg "name") in
            let v_md' = Vg_IO.Volume.metadata_of id in
            assert_equal ~printer:(fun x -> Sexplib.Sexp.to_string_hum (Lv.sexp_of_t x)) v_md v_md';
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
            Vg_IO.connect [ block ] `RW >>|= fun vg ->
            Vg.create (Vg_IO.metadata_of vg) ~tags:[tag] "name" ~status:Lv.Status.([Read; Write; Visible]) small >>*= fun (_,op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            Vg.rename (Vg_IO.metadata_of vg) "name" "name2" >>*= fun (_, op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            ( match Vg_IO.find vg "name2" with None -> failwith "rename name2" | Some x -> () );
            ( match Vg_IO.find vg "name" with Some _ -> failwith "rename name2, name still exists" | None -> () );
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
            Vg_IO.connect [ block ] `RW >>|= fun vg ->
            Vg.create (Vg_IO.metadata_of vg) "name" bigger >>*= fun (_,op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let id = ok_or_fail (Vg_IO.find vg "name") in
            let v_md = Vg_IO.Volume.metadata_of id in
            assert_equal ~printer:Int64.to_string bigger_extents (Pv.Allocator.size (Lv.to_allocation v_md));
            Vg.resize (Vg_IO.metadata_of vg) "name" small >>*= fun (_, op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let id = ok_or_fail (Vg_IO.find vg "name") in
            let v_md = Vg_IO.Volume.metadata_of id in
            assert_equal ~printer:Int64.to_string small_extents (Pv.Allocator.size (Lv.to_allocation v_md));
            Vg.resize (Vg_IO.metadata_of vg) "name" bigger >>*= fun (_, op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let id = ok_or_fail (Vg_IO.find vg "name") in
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

let lv_crop () =
  let open Vg_IO in
  with_dummy (fun filename ->
      let t = 
        with_block filename
          (fun block ->
            Vg_IO.format ~magic:`Journalled "vg" [ pv, block ] >>|= fun () ->
            Vg_IO.connect [ block ] `RW >>|= fun vg ->
            Vg.create (Vg_IO.metadata_of vg) "name" bigger >>*= fun (_,op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let id = ok_or_fail (Vg_IO.find vg "name") in
            let v_md = Vg_IO.Volume.metadata_of id in
            assert_equal ~printer:Int64.to_string bigger_extents (Pv.Allocator.size (Lv.to_allocation v_md));
            let space = Lv.to_allocation v_md in
            let name, (start, length) = List.hd space in
            (* remove the first segment *)
            let op = Redo.Op.(LvCrop("name", { lvc_segments = Lv.Segment.linear 0L [ name, (start, 1L) ] })) in
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let id = ok_or_fail (Vg_IO.find vg "name") in
            let v_md = Vg_IO.Volume.metadata_of id in
            assert_equal ~printer:Int64.to_string small_extents (Pv.Allocator.size (Lv.to_allocation v_md));
            Lwt.return ()
          )
      in
      Lwt_main.run t)

let lv_remove () =
  let open Vg_IO in
  with_dummy (fun filename ->
      let t = 
        with_block filename
          (fun block ->
            Vg_IO.format ~magic:`Journalled "vg" [ pv, block ] >>|= fun () ->
            Vg_IO.connect [ block ] `RW >>|= fun vg ->
            Vg.create (Vg_IO.metadata_of vg) ~tags:[tag] "name" ~status:Lv.Status.([Read; Write; Visible]) small >>*= fun (_,op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            Vg.remove (Vg_IO.metadata_of vg) "name" >>*= fun (_, op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            ( match Vg_IO.find vg "name" with None -> () | Some _ -> failwith "remove didn't work" );
            Lwt.return ()
          )
      in
      Lwt_main.run t)

let lv_tags () =
  let open Vg_IO in
  with_dummy (fun filename ->
      let t = 
        with_block filename
          (fun block ->
            Vg_IO.format ~magic:`Journalled "vg" [ pv, block ] >>|= fun () ->
            Vg_IO.connect [ block ] `RW >>|= fun vg ->
            Vg.create (Vg_IO.metadata_of vg) "name" ~status:Lv.Status.([Read; Write; Visible]) small >>*= fun (_,op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let id = ok_or_fail (Vg_IO.find vg "name") in
            let v_md = Vg_IO.Volume.metadata_of id in
            let printer xs = String.concat "," (List.map Tag.to_string xs) in
            assert_equal ~printer [] v_md.Lv.tags;
            Vg.add_tag (Vg_IO.metadata_of vg) "name" tag >>*= fun (_, op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let id = ok_or_fail (Vg_IO.find vg "name") in
            let v_md = Vg_IO.Volume.metadata_of id in
            assert_equal ~printer [tag] v_md.Lv.tags;
            (* add it again for no change *)
            Vg.add_tag (Vg_IO.metadata_of vg) "name" tag >>*= fun (_, op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let id = ok_or_fail (Vg_IO.find vg "name") in
            let v_md = Vg_IO.Volume.metadata_of id in
            assert_equal ~printer [tag] v_md.Lv.tags;
            Vg.remove_tag (Vg_IO.metadata_of vg) "name" tag >>*= fun (_, op) ->
            Vg_IO.update vg [ op ] >>|= fun () ->
            Vg_IO.sync vg >>|= fun () ->
            let id = ok_or_fail (Vg_IO.find vg "name") in
            let v_md = Vg_IO.Volume.metadata_of id in
            assert_equal ~printer [] v_md.Lv.tags;
            Lwt.return ()
          )
      in
      Lwt_main.run t)

let vg_suite = "Vg" >::: [
    "LV name clash" >:: lv_name_clash;
    "LV create without redo" >:: lv_create `Lvm;
    "LV create with redo" >:: lv_create `Journalled;
    "LV rename" >:: lv_rename;
    "LV resize" >:: lv_resize;
    "LV crop" >:: lv_crop;
    "LV remove" >:: lv_remove;
    "LV tags" >:: lv_tags;
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

let pv0 = match Pv.Name.of_string "pv0" with
  | `Error x -> assert false
  | `Ok x -> x

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
  let verbose = ref false in
  Arg.parse [
    "-verbose", Arg.Unit (fun _ -> verbose := true), "Run in verbose mode";
  ] (fun x -> Printf.fprintf stderr "Ignoring argument: %s" x)
    "VG test suite";

  List.iter (fun suite -> ignore (run_test_tt ~verbose:!verbose suite)) [
    mda_suite;
    label_suite;
    pv_header_suite;
    uuid_suite;
    vg_suite;
    allocator_suite;
  ]
