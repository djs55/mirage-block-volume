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

open Lwt

type ('a, 'b) t = ('a, 'b) Result.result Lwt.t

open S

let ( >>= ) m f = m >>= function
  | `Error x -> return (`Error x)
  | `Ok x -> f x

let return x = return (`Ok x)

module FromResult = struct
  type ('a, 'b) t = ('a, 'b) Result.result Lwt.t

  let ( >>= ) m f = match m with
    | `Error x -> Lwt.return (`Error x)
    | `Ok x -> f x

  let return x = Lwt.return (`Ok x)

  let fail x = Lwt.return (`Error x)

  let all xs =
    let open Lwt in
    let rec loop acc x = x >>= function
    | [] -> return (`Ok (List.rev acc))
    | `Ok x :: xs -> loop (x :: acc) (return xs)
    | `Error x :: _ -> return (`Error x) in
    loop [] xs

end
