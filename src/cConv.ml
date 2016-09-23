
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

exception IntermediateFailure of (string list * string)

type 'a sequence = ('a -> unit) -> unit

(* error-raising function *)
let report_error msg =
  let b = Buffer.create 15 in
  Printf.bprintf b "conversion error: ";
  Printf.kbprintf
    (fun b -> raise (IntermediateFailure ([], (Buffer.contents b))))
    b msg

let formatted_error path msg =
  let path' = (String.concat " / " path) in
  raise (ConversionFailure (Printf.sprintf "%s at %s" msg path'))

(* function to look up the given name in an association list *)
let _get_field l name =
  try List.assoc name l
  with Not_found ->
    report_error "record field %s not found in source" name

module Encode = struct
  type 'a output = {
    unit : 'a;
    bool : bool -> 'a;
    float : float -> 'a;
    char : char -> 'a;
    int : int -> 'a;
    nativeint : nativeint -> 'a;
    int32 : int32 -> 'a;
    int64 : int64 -> 'a;
    string : string -> 'a;
    list : 'a list -> 'a;
    option : 'a option -> 'a;
    record : (string * 'a) list -> 'a;
    tuple : 'a list -> 'a;
    sum : string -> 'a list -> 'a;
  }

  let string_target = {
    unit="()";
    bool=string_of_bool;
    char=String.make 1;
    int=string_of_int;
    nativeint=Nativeint.to_string;
    int32=Int32.to_string;
    int64=Int64.to_string;
    float=string_of_float;
    string=(fun s -> "\"" ^ s ^ "\"");
    list=(fun l -> Printf.sprintf "[%s]" (String.concat "; " l));
    record=(fun l ->
      let l = List.map (fun (name,s) -> name ^ "=" ^ s) l in
      Printf.sprintf "{%s}" (String.concat "; " l)
    );
    option=(function None -> "None"  | Some x -> "Some " ^ x);
    sum=(fun name l -> match l with
      | [] -> name
      | [x] -> Printf.sprintf "%s (%s)" name x
      | _ -> Printf.sprintf "%s (%s)" name (String.concat ", " l)
    );
    tuple=(fun l -> Printf.sprintf "(%s)" (String.concat ", " l));
  }

  type -'src encoder = {
    emit : 'into. 'into output -> 'src -> 'into
  }

  let unit = {emit=fun into () -> into.unit}
  let char = {emit=fun into x -> into.char x}
  let int = {emit=fun into i -> into.int i}
  let nativeint = {emit=fun into i -> into.nativeint i}
  let int32 = {emit=fun into i -> into.int32 i}
  let int64 = {emit=fun into i -> into.int64 i}
  let bool = {emit=fun into b -> into.bool b}
  let float = {emit=fun into f -> into.float f}
  let string = {emit=fun into s -> into.string s}
  let list encode_x =
    {emit=fun into l -> into.list (List.map (encode_x.emit into) l)}

  let option encode_x =
    {emit=fun into x -> match x with
      | None -> into.option None
      | Some x -> into.option (Some (encode_x.emit into x))
    }

  let map f encode_fx = {emit=fun into x -> encode_fx.emit into (f x)}

  let array encode_x = {emit=fun into a ->
    into.list (Array.to_list (Array.map (encode_x.emit into) a))
  }

  let seq_to_list seq =
    let r = ref [] in
    seq (fun x -> r := x :: !r);
    List.rev !r

  let sequence encode_x =
    map seq_to_list (list encode_x)

  (** {6 Composite Types} *)

  let apply into enc x = enc.emit into x

  type 'r record_encoder = {
    record_emit : 'into. 'into output -> 'r -> (string * 'into) list;
  }

  let record f = {emit=fun into r ->
    into.record (f.record_emit into r)
  }

  let record_fix f =
    let rec f' = {emit=fun into r ->
      let fields = (Lazy.force emit).record_emit into r in
      into.record fields
    } and emit = lazy (f f') in
    f'

  type 't tuple_encoder = {
    tuple_emit : 'into. 'into output -> 't -> 'into list;
  }

  let tuple f = {emit=fun into x ->
    into.tuple (f.tuple_emit into x)
  }

  let pair enc_x enc_y = {emit=fun into (x,y) ->
    into.tuple [enc_x.emit into x; enc_y.emit into y]
  }

  let triple enc_x enc_y enc_z = {emit=fun into (x,y,z) ->
    into.tuple [enc_x.emit into x; enc_y.emit into y; enc_z.emit into z]
  }

  let quad enc_x enc_y enc_z enc_w = {emit=fun into (x,y,z,w) ->
    into.tuple [enc_x.emit into x; enc_y.emit into y;
                enc_z.emit into z; enc_w.emit into w]
  }

  type 's sum_encoder = {
    sum_emit : 'into. 'into output -> 's -> string * 'into list
  }

  let sum f = {emit=fun into x ->
    let name, args = f.sum_emit into x in
    into.sum name args
  }

  let sum0 f = {emit=fun into x ->
    let name = f x in
    into.sum name []
  }

  let sum_fix f =
    let rec f' = {emit=fun into x ->
      let name, args = (Lazy.force emit).sum_emit into x in
      into.sum name args
    }
  and emit = lazy (f f') in
    f'
end

module Decode = struct
  type 'src source = {
    emit : 'a. ('src,'a) inner_decoder -> 'src -> 'a;
  } (** Decode a value of type 'src *)

  and ('src, 'into) inner_decoder = {
    accept_unit : 'src source ->  unit -> 'into;
    accept_bool : 'src source -> bool -> 'into;
    accept_float : 'src source -> float -> 'into;
    accept_int : 'src source -> int -> 'into;
    accept_int32 : 'src source -> int32 -> 'into;
    accept_int64 : 'src source -> int64 -> 'into;
    accept_nativeint : 'src source -> nativeint -> 'into;
    accept_char : 'src source -> char -> 'into;
    accept_string : 'src source -> string -> 'into;
    accept_list : 'src source -> 'src list -> 'into;
    accept_option : 'src source -> 'src option -> 'into;
    accept_record : 'src source -> (string * 'src) list -> 'into;
    accept_tuple : 'src source -> 'src list -> 'into;
    accept_sum : 'src source -> string -> 'src list -> 'into;
  } (** Decode a value of type 'src into a type 'into.
        The user must provide all functions but [accept] *)

  type 'into decoder = {
    dec : 'src. ('src, 'into) inner_decoder;
  }

  let apply_inner src dec x = src.emit dec x
  let apply src dec x = src.emit dec.dec x

  let fail_ obtained = report_error "unexpected %s" obtained

  let failing =
    { accept_unit=(fun _ _ -> fail_ "unit")
    ; accept_int=(fun _ _ -> fail_ "int")
    ; accept_nativeint=(fun _ _ -> fail_ "nativeint")
    ; accept_int32=(fun _ _ -> fail_ "int32")
    ; accept_int64=(fun _ _ -> fail_ "int64")
    ; accept_char=(fun _ _ -> fail_ "char")
    ; accept_float=(fun _ _ -> fail_ "float")
    ; accept_bool=(fun _ _ -> fail_ "bool")
    ; accept_string=(fun _ _ -> fail_ "string")
    ; accept_list=(fun _ _ -> fail_ "list")
    ; accept_option=(fun _ _ -> fail_ "option")
    ; accept_sum=(fun _ _ _ -> fail_ "sum")
    ; accept_record=(fun _ _ -> fail_ "record")
    ; accept_tuple=(fun _ _ -> fail_ "tuple")
  }

  let char = {dec={
    failing with
    accept_char=(fun _ c -> c);
    accept_int=(fun _ x -> Char.chr x);
    accept_string=(fun _ s ->
      if String.length s = 1 then String.get s 0 else fail_ "string"
    );
  }}

  let int = {dec={
    failing with
    accept_int=(fun _ x -> x);
    accept_nativeint=(fun _ x -> Nativeint.to_int x);
    accept_int32=(fun _ x -> Int32.to_int x);
    accept_int64=(fun _ x -> Int64.to_int x);
    accept_float=(fun _ x-> int_of_float x);
    accept_string=(fun _ s ->
      try int_of_string s with Failure _ -> fail_ "string"
    );
  }}

  let nativeint = {dec={
    failing with
    accept_int=(fun _ x -> Nativeint.of_int x);
    accept_nativeint=(fun _ x -> x);
    accept_int32=(fun _ x -> Nativeint.of_int32 x);
    accept_int64=(fun _ x -> Int64.to_nativeint x);
    accept_float=(fun _ x-> Nativeint.of_float x);
    accept_string=(fun _ s ->
      try Nativeint.of_string s with Failure _ -> fail_ "string"
    );
  }}

  let int32 = {dec={
    failing with
    accept_int=(fun _ x -> Int32.of_int x);
    accept_nativeint=(fun _ x -> Nativeint.to_int32 x);
    accept_int32=(fun _ x -> x);
    accept_int64=(fun _ x -> Int64.to_int32 x);
    accept_float=(fun _ x-> Int32.of_float x);
    accept_string=(fun _ s ->
      try Int32.of_string s with Failure _ -> fail_ "string"
    );
  }}

  let int64 = {dec={
    failing with
    accept_int=(fun _ x -> Int64.of_int x);
    accept_nativeint=(fun _ x -> Int64.of_nativeint x);
    accept_int32=(fun _ x -> Int64.of_int32 x);
    accept_int64=(fun _ x -> x);
    accept_float=(fun _ x-> Int64.of_float x);
    accept_string=(fun _ s ->
      try Int64.of_string s with Failure _ -> fail_ "string"
    );
  }}

  let bool = {dec={
    failing with
    accept_bool=(fun _ x ->x);
    accept_int=(fun _ i -> if i=0 then false else true);
    accept_string=(fun _ s -> match s with
      | "true" | "True" -> true
      | "false" | "False" -> false
      | s -> fail_ s
    );
  }}

  let unit = {dec={
    failing with
    accept_unit=(fun _ _ ->());
    accept_int=(fun _ i -> if i=0 then () else fail_ "expected unit");
    accept_int32=(fun _ i -> if i=0l then () else fail_ "expected unit");
    accept_int64=(fun _ i -> if i=0L then () else fail_ "expected unit");
    accept_string=(fun _ s -> match s with
      | "()" -> ()
      | s -> fail_ s
    );
  }}

  let float = {dec={
    failing with
    accept_float=(fun _ x->x);
    accept_int=(fun _ x -> float_of_int x);
    accept_string=(fun _ s ->
      try float_of_string s with Failure _ -> fail_ s
    );
  }}

  let string = {dec={
    failing with
    accept_float=(fun _ x -> string_of_float x);
    accept_int=(fun _ -> string_of_int);
    accept_unit=(fun _ _ -> "()");
    accept_bool=(fun _ x ->string_of_bool x);
    accept_string=(fun _ x -> x);
    accept_sum=(fun _src name args ->
      if args=[] then name else fail_ "sum"
    );
  }}

  let list dec_x =
    let emitter src l =
      let wrapper i v =
        try src.emit dec_x.dec v
        with IntermediateFailure (path, msg) ->
          raise (IntermediateFailure ((string_of_int i)::path, msg))
      in
      List.mapi wrapper l in
    {dec={
    failing with
    accept_list=emitter;
    accept_tuple=emitter;
    accept_option=(fun src o -> match o with
      | None -> []
      | Some x -> [src.emit dec_x.dec x]
    );
  }}

  let option dec_x = {dec={
    failing with
    accept_option=(fun src o -> match o with
      | None -> None
      | Some x -> Some (src.emit dec_x.dec x)
    );
    accept_list=(fun src l -> match l with
      | [] -> None
      | [x] -> Some (src.emit dec_x.dec x)
      | _ -> report_error "expected option, got list"
    );
    accept_unit=(fun _src () -> None);
  }}

  let map f d = {dec=
    { accept_unit=(fun src x -> f (d.dec.accept_unit src x))
    ; accept_bool=(fun src x -> f (d.dec.accept_bool src x))
    ; accept_float=(fun src x -> f (d.dec.accept_float src x))
    ; accept_char=(fun src x -> f (d.dec.accept_char src x))
    ; accept_int=(fun src x -> f (d.dec.accept_int src x))
    ; accept_int32=(fun src x -> f (d.dec.accept_int32 src x))
    ; accept_int64=(fun src x -> f (d.dec.accept_int64 src x))
    ; accept_nativeint=(fun src x -> f (d.dec.accept_nativeint src x))
    ; accept_string=(fun src x -> f (d.dec.accept_string src x))
    ; accept_list=(fun src l -> f (d.dec.accept_list src l))
    ; accept_option=(fun src x -> f (d.dec.accept_option src x))
    ; accept_record=(fun src l -> f (d.dec.accept_record src l))
    ; accept_tuple=(fun src l -> f (d.dec.accept_tuple src l))
    ; accept_sum=(fun src name l -> f (d.dec.accept_sum src name l))
  }}

  let array dec_x = map Array.of_list (list dec_x)

  let seq_of_list l yield = List.iter yield l

  let sequence dec_x = map seq_of_list (list dec_x)

  let fail_accept_ expected =
    report_error "expected %s" expected

  let arg0 = function
    | [] -> ()
    | _ -> fail_accept_ "empty list"

  let arg1 dec src = function
    | [x] -> src.emit dec.dec x
    | _ -> fail_accept_ "one-element list"

  let arg2 dec_x dec_y src = function
    | [x;y] -> src.emit dec_x.dec x, src.emit dec_y.dec y
    | _ -> fail_accept_ "2 elements"

  let arg3 dec_x dec_y dec_z src = function
    | [x;y;z] -> src.emit dec_x.dec x, src.emit dec_y.dec y, src.emit dec_z.dec z
    | _ -> fail_accept_ "3 elements"

  let pair dec_x dec_y = {dec={
    failing with
    accept_list=(fun src l -> arg2 dec_x dec_y src l);
    accept_tuple=(fun src l -> arg2 dec_x dec_y src l);
  }}

  let triple dec_x dec_y dec_z = {dec={
    failing with
    accept_list=(fun src l -> arg3 dec_x dec_y dec_z src l);
    accept_tuple=(fun src l -> arg3 dec_x dec_y dec_z src l);
  }}

  let record_get name dec src l =
    let rec getter = function
    | [] -> report_error "could not find record field %s" name
    | (name', x) :: _ when name=name' -> src.emit dec.dec x
    | _ :: tail -> getter tail in
    try getter l
    with IntermediateFailure (path, msg) ->
      raise (IntermediateFailure (name::path, msg))

  let rec record_get_opt name dec src l = match l with
    | [] -> None
    | (name', x) :: _ when name=name' -> Some (src.emit dec.dec x)
    | _ :: tail -> record_get_opt name dec src tail

  type 'into record_decoder = {
    record_accept : 'src. 'src source -> (string * 'src) list -> 'into;
  }

  (* put an item of an association list into [r] *)
  let get_assoc_pair () = {
    failing with
    accept_list=(fun src l -> match l with
      | [s; x] -> src.emit string.dec s, x
      | _ -> fail_accept_ "expected pair string/value"
    );
  }

  let get_assoc_list src l =
    let getter = get_assoc_pair () in
    List.map (fun p -> src.emit getter p) l

  let record f = {dec={
    failing with
    accept_record=(fun src l -> f.record_accept src l);
    accept_list=(fun src l ->
      let assoc = get_assoc_list src l in
      f.record_accept src assoc
    );
    accept_tuple=(fun src l ->
      let assoc = get_assoc_list src l in
      f.record_accept src assoc
    );
  }}

  let record_fix make_f =
    let rec self = lazy (record {
      record_accept=fun src l -> (Lazy.force f).record_accept src l
    })
    and f = lazy (make_f (Lazy.force self)) in
    Lazy.force self

  type 'into sum_decoder = {
    sum_accept : 'src. 'src source -> string -> 'src list -> 'into;
  }

  let sum f = {dec={
    failing with
    accept_sum=(fun src name args -> f.sum_accept src name args);
    accept_string=(fun src s -> f.sum_accept src s []);
    accept_list=(fun src l -> match l with
      | name::args ->
          let name = apply_inner src string.dec name in
          f.sum_accept src name args
      | [] -> fail_ "empty list when expecting sum"
    );
    accept_tuple=(fun src l -> match l with
      | name::args ->
          let name = apply_inner src string.dec name in
          f.sum_accept src name args
      | [] -> fail_ "empty tuple when expecting sum"
    );
  }}

  let sum_fix make_f =
    let rec self = lazy (sum {
      sum_accept=fun src name args -> (Lazy.force f).sum_accept src name args
    })
    and f = lazy (make_f (Lazy.force self)) in
    Lazy.force self

  type 'into tuple_decoder = {
    tuple_accept : 'src. 'src source -> 'src list -> 'into;
  }

  let tuple f = {dec={
    failing with
    accept_tuple=(fun src l -> f.tuple_accept src l);
    accept_list=(fun src l -> f.tuple_accept src l);
  }}
end

let encode enc target x = enc.Encode.emit target x

let to_string enc x = enc.Encode.emit Encode.string_target x

let decode_exn src dec x =
  try src.Decode.emit dec.Decode.dec x
  with IntermediateFailure (path, msg) ->
    formatted_error path msg

type 'a or_error = [ `Ok of 'a | `Error of string ]

let decode src dec x =
  try `Ok (src.Decode.emit dec.Decode.dec x)
  with ConversionFailure msg -> `Error msg
