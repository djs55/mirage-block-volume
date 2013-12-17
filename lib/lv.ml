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
open Logging
open Result

module Status = struct
  type t = 
    | Read
    | Write
    | Visible
  with rpc
	
  let to_string = function
    | Read -> "READ"
    | Write -> "WRITE"
    | Visible -> "VISIBLE"

  let of_string = function
    | "READ" -> return Read
    | "WRITE" -> return Write
    | "VISIBLE" -> return Visible
    | x -> fail (Printf.sprintf "Bad LV status string: %s" x)
end

module Stripe = struct
  type t = {
    st_stripe_size : int64; (* In sectors *)
    st_stripes : (string * int64) list; (* pv name * start extent *)
  } with rpc
end

module Linear = struct
  type t = {
    l_pv_name : string;
    l_pv_start_extent : int64;
  } with rpc
end

module Segment = struct
  type cls = 
  | Linear of Linear.t 
  | Striped of Stripe.t

  and t = 
    { s_start_extent : int64; 
      s_extent_count : int64;
      s_cls : cls; }
  with rpc

  let sort s =
    List.sort (fun s1 s2 -> compare s1.s_start_extent s2.s_start_extent) s

  let of_metadata name config =
    let open Stripe in
    let open Linear in
    expect_mapped_int "start_extent" config >>= fun s_start_extent ->
    expect_mapped_int "extent_count" config >>= fun s_extent_count ->
    expect_mapped_string "type" config >>= fun ty ->
    ( if ty = "striped" then return ty
      else fail (Printf.sprintf "Cannot handle LV segment type '%s'" ty) ) >>= fun ty ->
    expect_mapped_array "stripes" config >>= fun stripes ->
    let rec handle_stripes acc = function
      | [] -> return (List.rev acc)
      | [ _ ] -> fail "Unexpected attribute found when parsing stripes"
      | name::offset::rest ->
        expect_string "name" name >>= fun name ->
        expect_int "offset" offset >>= fun offset ->
        handle_stripes ((name, offset) :: acc) rest in
    ( match stripes with
      | [ name; offset ] ->
        expect_string "name" name >>= fun l_pv_name ->
        expect_int "offset" offset >>= fun l_pv_start_extent ->
        return (Linear { l_pv_name; l_pv_start_extent })
      | _ ->
        expect_mapped_int "stripe_size" config >>= fun st_stripe_size ->
        handle_stripes [] stripes >>= fun st_stripes ->
        return (Striped { st_stripe_size; st_stripes }) ) >>= fun s_cls ->
    return { s_start_extent; s_extent_count; s_cls }

  let to_allocation s = match s.s_cls with
    | Linear l ->
	[(l.Linear.l_pv_name, (l.Linear.l_pv_start_extent, s.s_extent_count))]
    | Striped st ->
(* LVM appears to always round up the number of extents allocated such
   that it's divisible by the number of stripes, so we always fully allocate
   each extent in each PV. Let's be tolerant to broken metadata when this
   isn't the case by rounding up rather than down, so partially allocated
   extents are included in the allocation *)
	let extent_count = s.s_extent_count in
	let nstripes = Int64.of_int (List.length st.Stripe.st_stripes) in
	List.map (fun (name,start) ->
		    let allocated_extents = 
		      Int64.div 
			(Int64.sub 
			   (Int64.add 
			      extent_count nstripes) 1L) nstripes
		    in
		    (name,(start,allocated_extents)))
	  (st.Stripe.st_stripes)

end

type t = {
  name : string;
  id : Uuid.t;
  tags : Tag.t list;
  status : Status.t list;
  segments : Segment.t list;
} with rpc

open Result

let iteri f list = ignore (List.fold_left (fun i x -> f i x; i+1) 0 list)

let comp f g x = f (g x)
let (++) f g x = comp f g x

let marshal lv b =
  let ofs = ref 0 in
  let bprintf fmt = Printf.kprintf (fun s ->
    let len = String.length s in
    Cstruct.blit_from_string s 0 b !ofs len;
    ofs := !ofs + len
  ) fmt in
  bprintf "\n%s {\nid = \"%s\"\nstatus = [%s]\n" lv.name (Uuid.to_string lv.id)
    (String.concat ", " (List.map (o quote Status.to_string) lv.status));
  if List.length lv.tags > 0 then 
    bprintf "tags = [%s]\n" (String.concat ", " (List.map (quote ++ Tag.to_string) lv.tags));
  bprintf "segment_count = %d\n\n" (List.length lv.segments);
  let open Segment in
  let open Stripe in
  let open Linear in
  iteri
    (fun i s -> 
      bprintf "segment%d {\nstart_extent = %Ld\nextent_count = %Ld\n\n"
	(i+1) s.s_start_extent s.s_extent_count;
       match s.s_cls with
	 | Linear l ->
	     bprintf "type = \"striped\"\nstripe_count = 1\t#linear\n\n";
	     bprintf "stripes = [\n\"%s\", %Ld\n]\n}\n" l.l_pv_name l.l_pv_start_extent
	 | Striped st ->
	     let stripes = List.length st.st_stripes in
	     bprintf "type = \"striped\"\nstripe_count = %d\nstripe_size = %Ld\n\nstripes = [\n"
	       stripes st.st_stripe_size;
	     List.iter (fun (pv,offset) -> bprintf "%s, %Ld\n" (quote pv) offset) st.st_stripes;
	     bprintf "]\n}\n") lv.segments;
  bprintf "}\n";
  Cstruct.shift b !ofs

(** Builds a logical_volume structure out of a name and metadata. *)
let of_metadata name config =
  expect_mapped_string "id" config >>= fun id ->
  Uuid.of_string id >>= fun id ->
  map_expected_mapped_array "status"
    (fun a ->
      expect_string "status" a >>= fun x ->
      Status.of_string x
    ) config >>= fun status ->
  (if List.mem_assoc "tags" config
   then map_expected_mapped_array "tags" (expect_string "tags") config
   else return []) >>= fun tags ->
  let tags = List.map Tag.of_string tags in
  let segments = filter_structs config in
  Result.all (List.map (fun (a, _) ->
    expect_mapped_struct a segments >>= fun x ->
    Segment.of_metadata a x) segments) >>= fun segments ->
  let segments = Segment.sort segments in
  return { name; id; status; tags; segments }

let to_allocation lv =
  List.flatten 
    (List.map Segment.to_allocation lv.segments)

let size_in_extents lv =
  List.fold_left (Int64.add) 0L
    (List.map (fun seg -> seg.Segment.s_extent_count) lv.segments)
	    
let reduce_size_to lv new_seg_count =
  let cur_size = size_in_extents lv in
  debug "Beginning reduce_size_to:";
  ( if cur_size < new_seg_count
    then fail (Printf.sprintf "LV: cannot reduce size: current size (%Ld) is less than requested size (%Ld)" cur_size new_seg_count)
    else return () ) >>= fun () ->
  let rec doit segs left acc =
    match segs with 
      | s::ss ->
	  debug "Lv.reduce_size_to: s.s_start_extent=%Ld s.s_extent_count=%Ld left=%Ld" 
			  s.Segment.s_start_extent s.Segment.s_extent_count left;
	  if left > s.Segment.s_extent_count then
	    doit ss (Int64.sub left s.Segment.s_extent_count) (s::acc)
	  else
	    {s with Segment.s_extent_count = left}::acc
      | _ -> acc
  in
  return {lv with segments = Segment.sort (doit lv.segments new_seg_count [])}

let increase_allocation lv new_segs =
  {lv with segments = Segment.sort (lv.segments @ new_segs)}
