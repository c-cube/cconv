(*
 * BatSexp - interface to Sexplib
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

(** {1 Interface to Sexplib} *)

type 'a or_error = [ `Ok of 'a | `Error of string ]

type t = Sexplib.Sexp.t =
  | Atom of string
  | List of t list

val output : t CConv.Encode.output
val source : t CConv.Decode.source

val encode : 'src CConv.Encode.encoder -> 'src -> t
val decode_exn : 'into CConv.Decode.decoder -> t -> 'into
val decode : 'into CConv.Decode.decoder -> t -> 'into or_error

val to_string : 'a CConv.Encode.encoder -> 'a -> string
val of_string : 'a CConv.Decode.decoder -> string -> 'a or_error
val of_string_exn : 'a CConv.Decode.decoder -> string -> 'a

val sexp_to_string : t -> string
