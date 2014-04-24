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

(** {1 Interface to Yojson} *)

type t = Yojson.Basic.json

val source : t CConv.UniversalSource.t
val sink : t CConv.UniversalSink.t

val into : 'a CConv.Source.t -> 'a -> t
val from : 'a CConv.Sink.t -> t -> 'a
val from_opt : 'a CConv.Sink.t -> t -> 'a option

val to_string : 'a CConv.Source.t -> 'a -> string
val of_string : 'a CConv.Sink.t -> string -> 'a option

val json_to_string : t -> string
