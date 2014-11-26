(*
 * CYojson - interface to Yojson
 * Copyright (C) 2014 Simon Cruanes
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)


type 'a or_error = [ `Ok of 'a | `Error of string ]
type t =
  [ `Assoc of (string * Yojson.Basic.json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of Yojson.Basic.json list
  | `Null
  | `String of string ]

let source =
  let module D = CConv.Decode in
  let rec src = {D.emit=fun dec (x:t) -> match x with
    | `Bool b -> dec.D.accept_bool src b
    | `Int i -> dec.D.accept_int src i
    | `Float f -> dec.D.accept_float src f
    | `String s -> dec.D.accept_string src s
    | `Null -> dec.D.accept_unit src ()
    | `List l -> dec.D.accept_list src l
    | `Assoc l -> dec.D.accept_record src l
  } in
  src

let target =
  let module E = CConv.Encode in
  { E.unit= `Null;
    bool = (fun b -> `Bool b);
    float = (fun f -> `Float f);
    int = (fun i -> `Int i);
    string = (fun s -> `String s);
    list = (fun l -> `List l);
    record = (fun l -> `Assoc l);
    tuple = (fun l -> `List l);
    sum = (fun name l -> match l with
      | [] -> `String name
      | _::_ -> `List (`String name :: l));
  }

let json_to_string s = Yojson.Basic.to_string ~std:true s

let encode src x = CConv.encode src target x

let decode dec x = CConv.decode source dec x

let decode_exn dec x = CConv.decode_exn source dec x

let to_string src x =
  json_to_string (encode src x)

let of_string dec s =
  try
    let x = Yojson.Basic.from_string s in
    decode dec x
  with Failure _ ->
    `Error "invalid JSON string"

let of_string_exn dec s =
  let x = Yojson.Basic.from_string s in
  decode_exn dec x

