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


let label_id = "LABELONE"
let sector_size = 512
let sector_sizeL = 512L
let label_size = sector_size
let label_scan_sectors = 4
let label_scan_size = label_scan_sectors * sector_size

let label_type = "LVM2 001"

let extent_size = Int64.mul 4096L 1024L
let extent_size_minus_one = Int64.sub extent_size 1L
let extent_size_in_sectors = Int64.div extent_size sector_sizeL

let fmtt_magic = " LVM2 x[5A%r0N*>"

let pe_align = 65536L

let redo_log_lv_name = "mlvm_redo_log"

let mib = Int64.mul 1024L 1024L
let tib = Int64.mul mib mib

let max_metadata_size = Int64.mul mib 16L

(* Ahem, mutable constants? *)
let mapper_name = ref "mapper"
let full_provision = ref false
