(*
 * Copyright (C) 2009-2015 Citrix Systems Inc.
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

module type Sanitised_string = sig
  type t
  include S.SEXPABLE with type t := t
  include S.PRINT with type t := t
  val of_string : string -> (t, string) Result.result
end

module Vg_name : Sanitised_string
module Lv_name : Sanitised_string
module Tag : Sanitised_string
