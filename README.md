CConv
=====

Combinators for Type Conversion in OCaml.

Documentation can be found [here](http://cedeela.fr/~simon/software/cconv/CConv.html),
and some toy examples in the `example` directory.

## Build and Install

There are no dependencies. This should work with OCaml>=4.00.1.

    $ make

Alternatively:

    $ opam pin add cconv -k git \
        https://github.com/c-cube/cconv.git

Optional bindings to serialization libraries are available for `sexplib`,
`yojson` and `bencode`. They will be installed via opam if the corresponding
library is present. See [this section](#backends) to learn how to write
your own serialization backends.

## License

This code is free, under the BSD license. See the `LICENSE` file.

## Usage

From `example/all.ml`:

```ocaml
#require "cconv";;

module Point = struct
  type t = {
    x : int;
    y : int;
    color : string;
    prev : t option; (* previous position, say *)
  }


  let encode = CConv.Encode.(record_fix
    (fun self ->
      let o_self = option self in
      {record_emit=fun into {x;y;color;prev} ->
        [ "x", int.emit into x
        ; "y", int.emit into y
        ; "color", string.emit into color
        ; "prev", o_self.emit into prev
        ]
      })) ;;

  let decode = CConv.Decode.(record_fix
    (fun self -> { record_accept=fun src l ->
      let x = record_get "x" int src l in
      let y = record_get "y" int src l in
      let color = record_get "color" string src l in
      let prev = record_get "prev" (option self) src l in
      {x;y;color;prev}
    })
  )

  let p = { x=1; y=2; color="red"; prev=None; }
  let p2 = {x=1; y=3; color="yellow"; prev=Some p; }
end ;;
module Point : sig ... end

(* convert into json *)
#require "cconv.yojson";;

CConvYojson.encode Point.encode Point.p;;
- : CConvYojson.t = `Assoc [
    ("x", `Int 1);
    ("y", `Int 2); ("color", `String "red");
    ("prev", `String "none")]

let json = CConvYojson.encode Point.encode Point.p2;;
val json : CConvYojson.t = `Assoc
    [("x", `Int 1); ("y", `Int 3); ("color", `String "yellow");
     ("prev", `List [`String "some"; `Assoc [("x", `Int 1);
     ("y", `Int 2); ("color", `String "red"); ("prev", `String "none")]])
     ]

let p2' = CConvYojson.decode Point.decode json;;
val p2' : Point.t CConvYojson.or_error =
    `Ok {Point.x = 1; y = 3; color = "yellow";
    prev = Some {Point.x = 1; y = 2; color = "red"; prev = None}}

match p2' with `Ok x -> x = Point.p2 | `Error e -> failwith e;;
- : bool = true

module Lambda = struct
  type t =
    | Var of string
    | App of t * t
    | Lambda of string * t

  let encode = CConv.Encode.(sum_fix
    (fun self -> {sum_emit=fun into t -> match t with
      | Var s -> "var", [string.emit into s]
      | App (t1,t2) -> "app", [self.emit into t1; self.emit into t2]
      | Lambda (v,t') -> "lambda", [string.emit into v; self.emit into t']
    })
  )

  let decode = CConv.Decode.(sum_fix
    (fun self -> {
      sum_accept=fun src name args -> match name, args with
        | "var", [x] ->
            let x = apply src string x in
            Var x
        | "app", [x;y] ->
            let x = apply src self x in
            let y = apply src self y in
            App(x,y)
        | "lambda", [x;y] ->
            let x = apply src string x in
            let y = apply src self y in
            Lambda(x,y)
        | _ -> CConv.report_error "expected lambda-term"
    })
  )

  let t1 = Lambda ("x", App (Lambda ("y", App (Var "y", Var "x")), Var "x"))
end;;
module Lambda : sig ... end

(*convert into bencode *)
#require "cconv.bencode";;

let b = CConvBencode.encode Lambda.encode Lambda.t1;;
val b : Bencode.t = ...

let t2 = CConvBencode.decode Lambda.decode b;;
val t2 : Lambda.t CConvBencode.or_error =
    `Ok (Lambda.Lambda ("x", Lambda.App
        (Lambda.Lambda ("y", Lambda.App (Lambda.Var "y",
        Lambda.Var "x")), Lambda.Var "x")))

```

## Backends

It is quite easy to write a backend for your favorite format, assuming it
can somehow represent atoms (strings, integers...), lists, records, sums, and is
recursive. The simplest example is `sexplib` (as found in `sexp/cConvSexp.ml`):


```ocaml
type t = Sexplib.Sexp.t

(* how to decode from Sexp *)
let source =
  let module D = CConv.Decode in
  let rec src = {D.emit=fun dec s -> match s with
    | Atom s -> dec.D.accept_string src s
    | List l -> dec.D.accept_list src l
  } in
  src

(* how to encode into Sexp *)
let target =
  let module E = CConv.Encode in
  { E.unit = List [];
    bool = (fun b -> Atom (string_of_bool b));
    float = (fun f -> Atom (string_of_float f));
    int = (fun i -> Atom (string_of_int i));
    string = (fun s -> Atom (String.escaped s));
    option = (function None -> List[] | Some x -> List [x]);
    list = (fun l -> List l);
    record = (fun l -> List (List.map (fun (a,b) -> List [Atom a; b]) l));
    tuple = (fun l -> List l);
    sum = (fun name l -> match l with
      | [] -> Atom name
      | _::_ -> List (Atom name :: l));
  }

```

## ppx_deriving_cconv

A [ppx_deriving](https://github.com/whitequark/ppx_deriving) plugin.
The point is to obtain many serializers/deserializers in one stroke.

### Usage

First, `#require "cconv.ppx";;` or use the library `cconv.ppx` (depends
on `ppx_deriving`). It provides three annotations:

- `[@@deriving cconv]` derives an encoder and a decoder for the type
- `[@@deriving encode]` derives only an encoder (print values)
- `[@@deriving decode]` derives only a decoder (parse values)

Example:

```ocaml
#require "cconv.ppx";;

type t = {
    x : int;
    y : int;
    color : string;
    prev : t option; (* previous position, say *)
} [@@deriving cconv] ;;
type t = ...
val encode : t CConv.Encode.encoder = ...
val decode : t CConv.Decode.decoder = ...

type term =
    | Var of string
    | App of term * term
    | Lambda of string * term
[@@deriving cconv] ;;
type term = ...
val encode_term : term CConv.Encode.encoder = ...
val decode_term : term CConv.Decode.decoder = ...

(* encoders/decoders can be used with several backend *)
#require "cconv.yojson";;

CConvYojson.encode encode_term;;
- : term -> Yojson.Basic.json = <fun>

CConvYojson.decode decode_term;;
- : Yojson.Basic.json -> [`Ok of term | `Error of string] = <fun>

#require "cconv.bencode";;
CConvBencode.encode encode_term;;
- : term -> Bencode.t = <fun>

CConvBencode.decode decode_term;;
- : Bencode.t -> [`Ok of term | `Error of string] = <fun>

#require "cconv.sexp";;
CConvSexp.encode encode;;
- : t -> Sexplib.Sexp.t = <fun>

CConvSexp.decode decode;;
- : Sexplib.Sexp.t -> [`Ok of t | `Error of string] = <fun>

(* on the fly converter *)

let json = CConvYojson.encode [%encode: int list] [1;2;3];;
val json : CConvYojson.t = `List [`Int 1; `Int 2; `Int 3]

let l = CConvYojson.decode [%decode: float list] json;;
val l : float list CConvYojson.or_error = `Ok [1.; 2.; 3.]

```

### Options

Attributes can modify the behavior of `ppx_deriving_cconv`. They are as follows:

- `[@encoder e]` to specify an encoder for a record field of variant argument
- `[@decoder d]` to specify a decoder for a record field or variant argument
- `[@cconv.ignore]` to ignore the field for encoding (decoding will still require it)
