
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Bidirectional Conversion} *)

exception ConversionFailure of string

val report_error : ('a, Buffer.t, unit, 'b) format4 -> 'a
(** Helper to report conversion errors.
    @raise ConversionFailure if the conversion failed (!) *)

(** {2 Encode}

Helps encoding values into a serialization format (building
values of some type 'a, such as a JSON tree) *)

module Encode : sig
  type 'a target = {
    unit : 'a;
    bool : bool -> 'a;
    float : float -> 'a;
    int : int -> 'a;
    string : string -> 'a;
    list : 'a list -> 'a;
    record : (string * 'a) list -> 'a;
    tuple : 'a list -> 'a;
    sum : string -> 'a list -> 'a;
  }

  val string_target : string target
  (** Print values. Caution, inefficient! Should be used for debugging only *)

  type -'src encoder = {
    emit : 'into. 'into target -> 'src -> 'into
  } (** A way to encode values of type ['src] into any serialization format *)

  val unit : unit encoder
  val bool : bool encoder
  val float : float encoder
  val int : int encoder
  val string : string encoder
  val list : 'a encoder -> 'a list encoder

  val map : ('a -> 'b) -> 'b encoder -> 'a encoder
  val array : 'a encoder -> 'a array encoder

  (** {6 Heterogeneous List} *)
  type hlist =
    | HNil : hlist
    | HCons : 'a encoder * 'a * hlist -> hlist

  val hnil : hlist
  val hcons : 'a encoder -> 'a -> hlist -> hlist
  val (@::) : ('a encoder * 'a) -> hlist -> hlist

  (** {6 Composite Types} *)

  type record_fields =
    | RecordEnd : record_fields (** Empty record *)
    | RecordField :
      string (** name *)
      * 'a encoder  (** how to encode value *)
      * 'a  (** value *)
      * record_fields  (** Rest *)
      -> record_fields

  val record_end : record_fields
  val field : string -> 'a encoder -> 'a ->
              record_fields -> record_fields

  val (@->) : (string * 'a encoder * 'a) ->
              record_fields -> record_fields
  (** Infix constructor for record fields. Example:
  {[ type point = {x:int; y:int; c:string};;
   let enc_point = record
     (fun {x;y;c} -> ("x", int, x) @-> ("y", int, y) @-> ("c", string, c) @-> record_end)
    ;;
  ]} *)

  val record : ('r -> record_fields) -> 'r encoder
  val record_fix : ('r encoder -> 'r -> record_fields) -> 'r encoder

  val tuple : ('a ->  hlist) -> 'a encoder
  (** General encoding of tuples *)

  val pair : 'a encoder ->
             'b encoder ->
             ('a * 'b) encoder
  val triple : 'a encoder ->
               'b encoder ->
               'c encoder ->
               ('a * 'b * 'c) encoder
  val quad : 'a encoder ->
             'b encoder ->
             'c encoder ->
             'd encoder ->
             ('a * 'b * 'c * 'd) encoder

  val sum : ('a -> string *  hlist) -> 'a encoder
  val sum0 : ('a -> string) -> 'a encoder (** Constant sums *)

  val sum_fix : ('a encoder -> 'a -> string * hlist) -> 'a encoder

  val option : 'a encoder -> 'a option encoder
end

(** {2 Decode}

A 'a decodee describes a way to traverse a value of some type representing
a serialization format such as JSON or B-encode *)
module Decode : sig
  type 'src source = {
    emit : 'a. ('src,'a) inner_decoder -> 'src -> 'a;
  } (** Decode a value of type 'src *)

  and ('src, 'into) inner_decoder = {
    accept_unit : 'src source ->  unit -> 'into;
    accept_bool : 'src source -> bool -> 'into;
    accept_float : 'src source -> float -> 'into;
    accept_int : 'src source -> int -> 'into;
    accept_string : 'src source -> string -> 'into;
    accept_list : 'src source -> 'src list -> 'into;
    accept_record : 'src source -> (string * 'src) list -> 'into;
    accept_tuple : 'src source -> 'src list -> 'into;
    accept_sum : 'src source -> string -> 'src list -> 'into;
  } (** Decode a value of type 'src into a type 'into. *)

  type 'into decoder = {
    dec : 'src. ('src, 'into) inner_decoder;
  }

  val apply : 'src source -> 'into decoder -> 'src -> 'into
  (** Apply a decoder to a source *)

  (** {6 Decoder Combinators} *)

  val int : int decoder
  val float : float decoder
  val bool : bool decoder
  val unit : unit decoder
  val string : string decoder

  val list : 'a decoder -> 'a list decoder
  val array : 'a decoder -> 'a array decoder

  val map : ('a -> 'b) -> 'a decoder -> 'b decoder
  (** Map the decoded value *)

  val arg0 : 'src list -> unit
  (** Only accepts an empty list/tuple *)

  val arg1 : 'a decoder -> 'src source -> 'src list -> 'a
  (** Only accepts a 1-element list/tuple *)

  val arg2 : 'a decoder -> 'b decoder ->
             'src source -> 'src list -> 'a * 'b
  (** Only accepts a 2-elements list/tuple *)

  val arg3 : 'a decoder ->
             'b decoder ->
             'c decoder ->
             'src source ->
             'src list -> 'a * 'b * 'c
  (** Only accepts a 3-elements list/tuple *)

  val option : 'a decoder -> 'a option decoder
  (** Helper for options *)

  val pair : 'a decoder -> 'b decoder -> ('a * 'b) decoder

  val triple : 'a decoder ->
               'b decoder ->
               'c decoder ->
               ('a * 'b * 'c) decoder

  val record_get : string -> 'into decoder ->
                  'src source -> (string * 'src) list ->
                  'into
  (** [record_get name dec l] is a helper for decoding records. It is
      given a list of fields [l], and searches [name] through it.
      If [name] is found with a value [v], [dec.accept v] is called.
      Otherwise an error is raised *)

  val record_get_opt : string -> 'into decoder ->
                      'src source -> (string * 'src) list ->
                      'into option

  type 'into record_decoder = {
    record_accept : 'src. 'src source -> (string * 'src) list -> 'into;
  }

  val record : 'into record_decoder -> 'into decoder
  (** Decoder for records. It will adapt itself to association tuples
      and lists. *)

  val record_fix : ('into decoder -> 'into record_decoder) ->
                   'into decoder

  type 'into sum_decoder = {
    sum_accept : 'src. 'src source -> string -> 'src list -> 'into;
  }

  val sum : 'into sum_decoder -> 'into decoder
  (** Decoder for sums. It will adapt itself to strings, lists
      and tuples *)

  val sum_fix : ('into decoder -> 'into sum_decoder) ->
                 'into decoder

  type 'into tuple_decoder = {
    tuple_accept : 'src. 'src source -> 'src list -> 'into;
  }

  val tuple : 'into tuple_decoder -> 'into decoder
  (** Tuple decoder *)

  (** Examples:
  {[
  type mytuple = int * string * float list ;;

  let decode_mytuple = tuple {
    tuple_accept=fun src l -> arg3 int int (list string) src l);
  };;

  (* OR, because triples are really easy: *)
  let decode_mytuple = triple int int (list string);;

  type point = { x:int;  y:int; color:string };;

  let decode_point = record ~expected:"point" {
    record_accept=(fun src l ->
      let x = record_get "x" int src l in
      let y = record_get "y" int src l in
      let color = record_get "color" string src l in
      {x;y;color}
    );
  };;
  ]}
  *)
end

type 'a or_error = [ `Ok of 'a | `Error of string ]

val encode : 'src Encode.encoder -> 'into Encode.target -> 'src -> 'into
(** Encode a value into the serialization format ['into] *)

val to_string : 'src Encode.encoder -> 'src -> string
(** Use {!Encode.string_target} to print the value *)

val decode_exn : 'src Decode.source -> 'into Decode.decoder -> 'src -> 'into
(** Decode a serialized value *)

val decode : 'src Decode.source -> 'into Decode.decoder -> 'src -> 'into or_error

