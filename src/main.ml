(*
 * Copyright (C) 2013 Citrix Inc
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

let project_url = "http://github.com/jonludlam/mlvm"

open Common
open Cmdliner

(* Help sections common to all commands *)

let _common_options = "COMMON OPTIONS"
let help = [ 
 `S _common_options; 
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
 `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" project_url);
]

(* Options common to all commands *)
let common_options_t = 
  let docs = _common_options in 
  let debug = 
    let doc = "Give only debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc) in
  let verb =
    let doc = "Give verbose output." in
    let verbose = true, Arg.info ["v"; "verbose"] ~docs ~doc in 
    Arg.(last & vflag_all [false] [verbose]) in
  let dummy =
    let doc = "Use 'dummy' mode for testing." in
    Arg.(value & flag & info ["dummy"] ~docs ~doc) in
  Term.(pure Common.make $ debug $ verb $ dummy)

let filename =
  let doc = Printf.sprintf "Path to the device to read." in
  Arg.(value & pos 0 (some file) None & info [] ~doc)

let read_cmd =
  let doc = "read and print the volume metadata" in
  let man = [
    `S "DESCRIPTION";
    `P "Search for volume metadata on the specified device and print it.";
  ] @ help in
  Term.(ret(pure Impl.read $ common_options_t $ filename)),
  Term.info "read" ~sdocs:_common_options ~doc ~man

let format_cmd =
  let doc = "format the device with a fresh volume group" in
  let man = [
    `S "DESCRIPTION";
    `P "Erase any volume metadata the device and re-initialise it with a fresh volume group. This operation is dangerous and irreversible."
  ] @ help in
  let vgname =
    let doc = "Name for the volume group" in
    Arg.(value & opt string "volumegroup" & info ["volume-group-name"] ~doc) in
  let pvname =
    let doc = "Name for the physical volume" in
    Arg.(value & opt string "physicalvolume" & info ["physical-volume-name"] ~doc) in
  Term.(ret(pure Impl.format $ common_options_t $ filename $ vgname $ pvname)),
  Term.info "format" ~sdocs:_common_options ~doc ~man

let default_cmd = 
  let doc = "manipulate MLVM volumes" in
  let man = help in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info (Sys.argv.(0)) ~version:"1.0.0" ~sdocs:_common_options ~doc ~man
       
let cmds = [read_cmd; format_cmd]

let _ =
  match Term.eval_choice default_cmd cmds with 
  | `Error _ -> exit 1
  | _ -> exit 0
