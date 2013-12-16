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

let (--) = Int64.sub
let (++) = Int64.add
let (//) = Int64.div
let ( *** ) = Int64.mul

let int64_round_up value blocksize =
  blocksize *** ((value ++ blocksize -- 1L) // blocksize)

let zero buf =
  for i = 0 to Cstruct.len buf - 1 do
    Cstruct.set_uint8 buf i 0
  done
