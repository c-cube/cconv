CConv
=====

Combinators for Type Conversion in OCaml.

Documentation can be found [here](https://c-cube.github.io/cconv),
and some toy examples in the `example` directory.

## Build and Install

There are no dependencies. This should work with OCaml>=4.03.0.

```
$ make
```

Alternatively:

```
$ opam pin https://github.com/c-cube/cconv.git
```

Optional bindings to serialization libraries are available for `sexplib`,
`yojson` and `bencode`. They will be installed via opam if the corresponding
library is present. See [this section](#backends) to learn how to write
your own serialization backends.

## License

This code is free, under the BSD license. See the `LICENSE` file.

## Usage

From `example/all.ml`:

```ocaml
# #require "cconv";;
```

```ocaml
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
      }))

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
end
```

```ocaml
# (* convert into json *)
  # require "cconv.yojson";;
# CConvYojson.encode Point.encode Point.p;;
- : CConvYojson.t =
`Assoc
  [("x", `Int 1); ("y", `Int 2); ("color", `String "red");
   ("prev", `List [])]

# let json = CConvYojson.encode Point.encode Point.p2;;
val json : CConvYojson.t =
  `Assoc
    [("x", `Int 1); ("y", `Int 3); ("color", `String "yellow");
     ("prev",
      `List
        [`Assoc
           [("x", `Int 1); ("y", `Int 2); ("color", `String "red");
            ("prev", `List [])]])]

# let p2' = CConvYojson.decode Point.decode json;;
val p2' : Point.t CConvYojson.or_error =
  `Ok
    {Point.x = 1; y = 3; color = "yellow";
     prev = Some {Point.x = 1; y = 2; color = "red"; prev = None}}

# match p2' with `Ok x -> x = Point.p2 | `Error e -> failwith e;;
- : bool = true
```

```ocaml
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
end
```

```ocaml
# (*convert into bencode *)
  #require "cconv.bencode";;

# let b = CConvBencode.encode Lambda.encode Lambda.t1;;
val b : Bencode.t =
...

# let t2 = CConvBencode.decode Lambda.decode b;;
val t2 : Lambda.t CConvBencode.or_error =
  `Ok
    (Lambda.Lambda ("x",
      Lambda.App
       (Lambda.Lambda ("y", Lambda.App (Lambda.Var "y", Lambda.Var "x")),
       Lambda.Var "x")))
```

## Backends

It is quite easy to write a backend for your favorite format, assuming it
can somehow represent atoms (strings, integers...), lists, records, sums, and is
recursive. The simplest example is `sexplib` (as found in `sexp/cConvSexp.ml`):

```ocaml
# #require "sexplib";;
```

```ocaml
type t = Sexplib.Sexp.t

(* how to decode from Sexp *)
let source =
  let open Sexplib.Sexp in
  let module D = CConv.Decode in
  let rec src = {D.emit=fun dec s -> match s with
    | Atom s -> dec.D.accept_string src s
    | List l -> dec.D.accept_list src l
  } in
  src

(* how to encode into Sexp *)
let target =
  let open Sexplib.Sexp in
  let module E = CConv.Encode in
  { E.unit = List [];
    bool = (fun b -> Atom (string_of_bool b));
    float = (fun f -> Atom (string_of_float f));
    int = (fun i -> Atom (string_of_int i));
    string = (fun s -> Atom (String.escaped s));
    option = (function None -> List[] | Some x -> List [x]);
    list = (fun l -> List l);
    char = (fun x -> Atom (String.make 1 x));
    nativeint = (fun i -> Atom (Nativeint.to_string i));
    int32 = (fun i -> Atom (Int32.to_string i));
    int64 = (fun i -> Atom (Int64.to_string i));
    record = (fun l -> List (List.map (fun (a,b) -> List [Atom a; b]) l));
    tuple = (fun l -> List l);
    sum = (fun name l -> match l with
      | [] -> Atom name
      | _::_ -> List (Atom name :: l));
  }

```

## ppx_deriving_cconv

A [ppx_deriving](https://github.com/ocaml-ppx/ppx_deriving) plugin.
The point is to obtain many serializers/deserializers in one stroke.

### Usage

First, `#require "cconv.ppx";;` or use the library `cconv.ppx` (depends
on `ppx_deriving`). It provides three annotations:

- `[@@deriving cconv]` derives an encoder and a decoder for the type
- `[@@deriving encode]` derives only an encoder (print values)
- `[@@deriving decode]` derives only a decoder (parse values)

Example:

```ocaml
# #require "cconv-ppx";;

# type t = {
    x : int;
    y : int;
    color : string;
    prev : t option; (* previous position, say *)
  } [@@deriving cconv] ;;
type t = { x : int; y : int; color : string; prev : t option; }
val encode : t CConv.Encode.encoder = {CConv.Encode.emit = <fun>}
val decode : t CConv.Decode.decoder =
...

# type term =
    | Var of string
    | App of term * term
    | Lambda of string * term
  [@@deriving cconv];;
type term = Var of string | App of term * term | Lambda of string * term
val encode_term : term CConv.Encode.encoder = {CConv.Encode.emit = <fun>}
val decode_term : term CConv.Decode.decoder =
...
```

Encoders/decoders can be used with several backend:

```ocaml
# #require "cconv.yojson";;

# CConvYojson.encode encode_term;;
- : term -> CConvYojson.t = <fun>

# CConvYojson.decode decode_term;;
- : CConvYojson.t -> term CConvYojson.or_error = <fun>

# #require "cconv.bencode";;
# CConvBencode.encode encode_term;;
- : term -> Bencode.t = <fun>

# CConvBencode.decode decode_term;;
- : Bencode.t -> term CConvBencode.or_error = <fun>

# #require "sexplib";;
# #require "cconv.sexp";;
# CConvSexp.encode encode;;
- : t -> CConvSexp.t = <fun>

# CConvSexp.decode decode;;
- : CConvSexp.t -> t CConvSexp.or_error = <fun>

# let json = CConvYojson.encode [%encode: int list] [1;2;3];;
val json : CConvYojson.t = `List [`Int 1; `Int 2; `Int 3]

# let l = CConvYojson.decode [%decode: float list] json;;
val l : float list CConvYojson.or_error = `Ok [1.; 2.; 3.]
```

### Options

Attributes can modify the behavior of `ppx_deriving_cconv`. They are as follows:

- `[@encoder e]` to specify an encoder for a record field of variant argument. type attribute
- `[@decoder d]` to specify a decoder for a record field or variant argument. type attribute

```ocaml
type boxed_int = {
  bint : int;
} [@@deriving cconv]

let box_int bint = {bint}
let unbox_int {bint} = bint

type t = {
  i : (int
         [@encoder CConv.Encode.(map box_int encode_boxed_int)]
         [@decoder CConv.Decode.(map unbox_int decode_boxed_int)]);
  j : int;
} [@@deriving cconv]
```

```ocaml
# CConvYojson.of_string_exn decode "{\"i\": {\"bint\": 100}, \"j\": 10}";;
- : t = {i = 100; j = 10}
```

- `[@cconv.ignore]` to ignore the field for encoding (decoding will still require it)
- `[@default expr]` to specify default value for a record field
```ocaml
type pagination = {
  pages   : int;
  current : int [@default 0];
} [@@deriving cconv]
```

- `[@key name]` to specify a name of record field in encoder / decoder
```ocaml
type geo = {
  lat : float [@key "Latitude"];
  lon : float [@key "Longitude"];
} [@@deriving cconv]
```
