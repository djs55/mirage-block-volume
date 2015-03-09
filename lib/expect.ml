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
open Lvm_internal
open Absty

(* Useful helper fns *)
let quote s = Printf.sprintf "\"%s\"" s 
let o = fun f g x -> f (g x) 

open Result

let expect_string name field =
  match field with 
    | AStr s -> return s
    | AInt n -> return (Printf.sprintf "%Ld" n)
    | _ -> fail (Printf.sprintf "Expecting string for identifier '%s'" name)

let expect_int name field =
  match field with 
    | AInt n -> return n
    | _ -> fail (Printf.sprintf "Expecting string for identifier '%s'" name)

let expect_struct name field =
  match field with
    | AStruct fields -> return fields
    | _ -> fail (Printf.sprintf "Expecting struct for identifier '%s'" name)

let expect_array name field =
  match field with
    | AArr a -> return a
    | _ -> fail (Printf.sprintf "Expecting array for identifier '%s'" name)
     
let expect_mapped_field transform name alist =
  if List.mem_assoc name alist
  then transform name (List.assoc name alist)
  else fail (Printf.sprintf "Failed to find %s in list [ %s ] " name (String.concat ", " (List.map fst alist)))
	
let expect_mapped_string = expect_mapped_field expect_string 
let expect_mapped_int = expect_mapped_field expect_int
let expect_mapped_struct = expect_mapped_field expect_struct
let expect_mapped_array = expect_mapped_field expect_array

let map_expected_mapped_array name fn alist =
  expect_mapped_array name alist >>= fun a ->
  all (List.map fn a)

let filter_structs alist = 
  List.filter (fun (a,b) -> match b with | AStruct _ -> true | _ -> false) alist
