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
  Term.(pure Common.make $ debug $ verb)

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
  let journalled =
    let doc = "Use the async journalled format" in
    Arg.(value & flag & info [ "journalled" ] ~doc) in
  Term.(ret(pure Impl.format $ common_options_t $ filename $ vgname $ pvname $ journalled)),
  Term.info "format" ~sdocs:_common_options ~doc ~man

let lvname =
  let doc = "logical volume name" in
  Arg.(value & pos 1 string "name" & info [] ~doc)

let lvsize =
  let doc = "logical volume size" in
  Arg.(value & opt string "0" & info [ "size" ] ~doc)

let create_cmd =
  let doc = "create a logical volume" in
  let man = [
    `S "DESCRIPTION";
    `P "Create a logical volume with a given name and size.";
  ] @ help in
  Term.(ret(pure Impl.create $ common_options_t $ filename $ lvname $ lvsize)),
  Term.info "create" ~sdocs:_common_options ~doc ~man

let map_cmd =
  let doc = "display the segment map for a logical volume" in
  let man = [
    `S "DESCRIPTION";
    `P "Display the mapping of logical blocks to physical blocks for a named volume."
  ] @ help in
  Term.(ret(pure Impl.map $ common_options_t $ filename $ lvname)),
  Term.info "map" ~sdocs:_common_options ~doc ~man

let rename_cmd =
  let doc = "rename a logical volume" in
  let man = [
    `S "DESCRIPTION";
    `P "Rename a logical volume which already exists.";
  ] @ help in
  let newname =
    let doc = "new logical volume name" in
    Arg.(value & pos 2 string "newname" & info [] ~doc) in
  Term.(ret(pure Impl.rename $ common_options_t $ filename $ lvname $ newname)),
  Term.info "rename" ~sdocs:_common_options ~doc ~man

let resize_cmd =
  let doc = "resize a logical volume" in
  let man = [
    `S "DESCRIPTION";
    `P "Resize a logical volume. Note if the new size is smaller than the old size then data will be lost.";
  ] @ help in
  let newsize =
    let doc = "new size" in
    Arg.(value & pos 2 string "" & info [] ~doc) in
  Term.(ret(pure Impl.resize $ common_options_t $ filename $ lvname $ newsize)),
  Term.info "resize" ~sdocs:_common_options ~doc ~man

let remove_cmd =
  let doc = "remove a logical volume" in
  let man = [
    `S "DESCRIPTION";
    `P "Remove a logical volume. All data within the volume will be lost but will not be erased.";
  ] @ help in
  Term.(ret(pure Impl.remove $ common_options_t $ filename $ lvname)),
  Term.info "remove" ~sdocs:_common_options ~doc ~man

let tag =
  let doc = "tag" in
  Arg.(value & pos 2 string "tag" & info [] ~doc)

let add_tag_cmd =
  let doc = "add a tag to a logical volume" in
  let man = [
    `S "DESCRIPTION";
    `P "Add a tag to a logical volume.";
  ] @ help in
  Term.(ret(pure Impl.add_tag $ common_options_t $ filename $ lvname $ tag)),
  Term.info "add-tag" ~sdocs:_common_options ~doc ~man

let remove_tag_cmd =
  let doc = "remove a tag from a logical volume" in
  let man = [
    `S "DESCRPIPTION";
    `P "Remove a tag from a logical volume.";
  ] @ help in
  Term.(ret(pure Impl.remove_tag $ common_options_t $ filename $ lvname $ tag)),
  Term.info "remove-tag" ~sdocs:_common_options ~doc ~man

let default_cmd = 
  let doc = "manipulate MLVM volumes" in
  let man = help in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ common_options_t)),
  Term.info (Sys.argv.(0)) ~version:"1.0.0" ~sdocs:_common_options ~doc ~man
       
let cmds = [read_cmd; format_cmd; create_cmd; rename_cmd; resize_cmd;
            add_tag_cmd; remove_tag_cmd; remove_cmd; map_cmd]

let _ =
  match Term.eval_choice default_cmd cmds with 
  | `Error _ -> exit 1
  | _ -> exit 0
