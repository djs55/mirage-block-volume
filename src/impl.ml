(*
 * Copyright (C) 2011-2013 Citrix Inc
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

open Common
open Cmdliner
open Lwt

let require name arg = match arg with
  | None -> failwith (Printf.sprintf "Please supply a %s argument" name)
  | Some x -> x

let (>>|=) m f = m >>= function
  | `Error e -> fail (Failure e)
  | `Ok x -> f x

let apply common =
  Constants.dummy_mode := common.Common.dummy;
  if common.Common.debug
  then Logging.destination := (fun s -> Printf.fprintf stderr "%s\n" s)

let read common filename =
  apply common;
  try
    let filename = require "filename" filename in
    let t =
      Vg.read [ filename ] >>|= fun vg ->
      return vg in
    let vg = Lwt_main.run t in
    Printf.printf "%s\n" (Jsonrpc.to_string (Vg.rpc_of_t vg));
    `Ok ()
  with
    | Failure x ->
      `Error(true, x)

let format common filename vgname pvname =
  apply common;
  try
    let filename = require "filename" filename in
    let t =
      Vg.format vgname [ filename, pvname ] >>|= fun () ->
      return () in
    Lwt_main.run t;
    `Ok ()
  with
    | Failure x ->
      `Error(true, x)

