
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

(* error-raising function *)
let report_error msg =
  let b = Buffer.create 15 in
  Printf.bprintf b "conversion error: ";
  Printf.kbprintf
    (fun b -> raise (ConversionFailure (Buffer.contents b)))
    b msg

(* function to look up the given name in an association list *)
let _get_field l name =
  try List.assoc name l
  with Not_found ->
    report_error "record field %s not found in source" name

module Encode = struct
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

  let string_target = {
    unit="()";
    bool=string_of_bool;
    int=string_of_int;
    float=string_of_float;
    string=(fun s -> "\"" ^ s ^ "\"");
    list=(fun l -> Printf.sprintf "[%s]" (String.concat "; " l));
    record=(fun l ->
      let l = List.map (fun (name,s) -> name ^ "=" ^ s) l in
      Printf.sprintf "{%s}" (String.concat "; " l)
    );
    sum=(fun name l -> match l with
      | [] -> name
      | [x] -> Printf.sprintf "%s (%s)" name x
      | _ -> Printf.sprintf "%s (%s)" name (String.concat ", " l)
    );
    tuple=(fun l -> Printf.sprintf "(%s)" (String.concat ", " l));
  }

  type -'src encoder = {
    emit : 'into. 'into target -> 'src -> 'into
  }

  let unit = {emit=fun into () -> into.unit}
  let int = {emit=fun into i -> into.int i}
  let bool = {emit=fun into b -> into.bool b}
  let float = {emit=fun into f -> into.float f}
  let string = {emit=fun into s -> into.string s}
  let list encode_x =
    {emit=fun into l -> into.list (List.map (encode_x.emit into) l)}

  let map f encode_fx = {emit=fun into x -> encode_fx.emit into (f x)}

  let array encode_x = {emit=fun into a ->
    into.list (Array.to_list (Array.map (encode_x.emit into) a))
  }

  (** {6 Heterogeneous List} *)
  type hlist =
    | HNil : hlist
    | HCons : 'a encoder * 'a * hlist -> hlist

  let hnil = HNil
  let hcons e x tail = HCons (e,x,tail)
  let (@::) (e, x) tail = HCons (e,x,tail)

  (** {6 Composite Types} *)

  type record_fields =
    | RecordEnd : record_fields (** Empty record *)
    | RecordField :
      string (** name *)
      * 'a encoder  (** how to encode the value *)
      * 'a  (** value *)
      * record_fields  (** Rest *)
      -> record_fields

  let rec apply_fields : type a. a target -> record_fields -> (string * a) list
    = fun into f -> match f with
    | RecordEnd -> []
    | RecordField (name, enc_x, x, tail) ->
        (name, enc_x.emit into x) :: apply_fields into tail

  let record_end = RecordEnd
  let field name enc_x x tail = RecordField (name,enc_x,x,tail)
  let (@->) (name,enc_x,x) tail = RecordField (name,enc_x,x,tail)

  let record f = {emit=fun into r ->
    let fields = f r in
    into.record (apply_fields into fields)
  }

  let record_fix f =
    let rec f' = {emit=fun into r ->
      let fields = f f' r in
      into.record (apply_fields into fields)
    } in
    f'

  let rec apply_hlist : type a. a target -> hlist -> a list
    = fun into l -> match l with
    | HNil -> []
    | HCons (enc_x, x, tail) ->
        enc_x.emit into x :: apply_hlist into tail

  let tuple f = {emit=fun into x ->
    let hlist = f x in
    into.tuple (apply_hlist into hlist)
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

  let sum f = {emit=fun into x ->
    let name, args = f x in
    into.sum name (apply_hlist into args)
  }

  let sum0 f = {emit=fun into x ->
    let name = f x in
    into.sum name []
  }

  let sum_fix f =
    let rec f' = {emit=fun into x ->
      let name, args = f f' x in
      into.sum name (apply_hlist into args)
    } in
    f'

  let option enc_x = {emit=fun into o -> match o with
    | None -> into.sum "none" []
    | Some x -> into.sum "some" [enc_x.emit into x]
  }
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
    accept_string : 'src source -> string -> 'into;
    accept_list : 'src source -> 'src list -> 'into;
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

  let fail_ obtained = report_error "unexpected  %s" obtained

  let failing =
    { accept_unit=(fun _ _ -> fail_ "unit")
    ; accept_int=(fun _ _ -> fail_ "int")
    ; accept_float=(fun _ _ -> fail_ "float")
    ; accept_bool=(fun _ _ -> fail_ "bool")
    ; accept_string=(fun _ _ -> fail_ "string")
    ; accept_list=(fun _ _ -> fail_ "list")
    ; accept_sum=(fun _ _ _ -> fail_ "sum")
    ; accept_record=(fun _ _ -> fail_ "record")
    ; accept_tuple=(fun _ _ -> fail_ "tuple")
  }

  let int = {dec={
    failing with
    accept_int=(fun _ x -> x);
    accept_float=(fun _ x-> int_of_float x);
    accept_string=(fun _ s ->
      try int_of_string s with Failure _ -> fail_ "string"
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
    accept_sum=(fun src name args ->
      if args=[] then name else fail_ "sum"
    );
  }}

  let list dec_x = {dec={
    failing with
    accept_list=(fun src l -> List.map (src.emit dec_x.dec) l);
    accept_tuple=(fun src l -> List.map (src.emit dec_x.dec) l);
  }}

  let map f d = {dec=
    { accept_unit=(fun src x -> f (d.dec.accept_unit src x))
    ; accept_bool=(fun src x -> f (d.dec.accept_bool src x))
    ; accept_float=(fun src x -> f (d.dec.accept_float src x))
    ; accept_int=(fun src x -> f (d.dec.accept_int src x))
    ; accept_string=(fun src x -> f (d.dec.accept_string src x))
    ; accept_list=(fun src l -> f (d.dec.accept_list src l))
    ; accept_record=(fun src l -> f (d.dec.accept_record src l))
    ; accept_tuple=(fun src l -> f (d.dec.accept_tuple src l))
    ; accept_sum=(fun src name l -> f (d.dec.accept_sum src name l))
  }}

  let array dec_x = map Array.of_list (list dec_x)

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

  let rec record_get name dec src l = match l with
    | [] -> report_error "could not find record field %s" name
    | (name', x) :: _ when name=name' -> src.emit dec.dec x
    | _ :: tail -> record_get name dec src tail

  let rec record_get_opt name dec src l = match l with
    | [] -> None
    | (name', x) :: _ when name=name' -> Some (src.emit dec.dec x)
    | _ :: tail -> record_get_opt name dec src tail

  type 'into record_decoder = {
    record_accept : 'src. 'src source -> (string * 'src) list -> 'into;
  }

  type assoc_pair =
    | AssocPair : 'src source * string * 'src -> assoc_pair

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

  let option dec = sum {
    sum_accept=(fun src name args -> match name, args with
      | ("none" | "None"), [] -> None
      | ("some" | "Some"), [x] -> Some (src.emit dec.dec x)
      | _ -> fail_accept_ "option"
    )
  }

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

let decode_exn src dec x = src.Decode.emit dec.Decode.dec x

type 'a or_error = [ `Ok of 'a | `Error of string ]

let decode src dec x =
  try `Ok (src.Decode.emit dec.Decode.dec x)
  with ConversionFailure msg -> `Error msg
