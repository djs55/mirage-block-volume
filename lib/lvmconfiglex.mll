{
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

open Lvmconfigparser
}

let digit = ['0'-'9']
let name = ['a'-'z' 'A'-'Z' '0'-'9' '.' '_' '+'] ['a'-'z' 'A'-'Z' '0'-'9' '.' '_' '+' '-']*
 
rule lvmtok = parse
  | '#' [^ '\n']* '\n' 
      { lvmtok lexbuf } (* Ignore comments *)
  | ['\t' '\n' ' '] 
      { lvmtok lexbuf } (* Ignore whitespace *)
  | digit+ as inum 
      { Integer (Int64.of_string inum) }
  | '\"' [^ '\"']* '\"' as str 
      { String (String.sub str 1 (String.length str - 2)) }
  | '{' { BeginStruct } 
  | '}' { EndStruct } 
  | '[' { BeginArray }
  | ']' { EndArray }
  | '=' { Equals }
  | ',' { Comma }
  | name as str 
      { Ident str }
  | _ as c
      { Printf.printf "Unrecognized character: '%c'\n" c;
	lvmtok lexbuf
      }
  | eof { Eof } 
