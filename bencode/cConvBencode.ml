(*
 * CBencode - interface to Bencode
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
type t = Bencode.t

let source =
  let module D = CConv.Decode in
  let rec src = {D.emit=fun dec b -> match b with
    | Bencode.String s -> dec.D.accept_string src s
    | Bencode.Integer i -> dec.D.accept_int src i
    | Bencode.List l -> dec.D.accept_list src l
    | Bencode.Dict l -> dec.D.accept_record src l
  } in
  src

let output =
  let module E = CConv.Encode in
  { E.unit = Bencode.Integer 0;
    bool = (fun b -> Bencode.Integer (if b then 1 else 0));
    float = (fun f -> Bencode.String (string_of_float f));
    int = (fun i -> Bencode.Integer i);
    string = (fun s -> Bencode.String s);
    option = (function None -> Bencode.List[] | Some x -> Bencode.List [x]);
    list = (fun l -> Bencode.List l);
    record = (fun l -> Bencode.Dict l);
    tuple = (fun l -> Bencode.List l);
    sum = (fun name l -> match l with
      | [] -> Bencode.String name
      | _::_ -> Bencode.List (Bencode.String name :: l));
  }

let bencode_to_string = Bencode.encode_to_string

let encode src x = CConv.encode src output x

let decode dec x = CConv.decode source dec x

let decode_exn dec x = CConv.decode_exn source dec x

let to_string src x =
  bencode_to_string (encode src x)

let of_string dec s =
  try
    let x = Bencode.decode (`String s) in
    decode dec x
  with Failure _ ->
    `Error "invalid B-encode string"

let of_string_exn dec s =
  let x = Bencode.decode (`String s) in
  decode_exn dec x
