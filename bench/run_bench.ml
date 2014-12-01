
(* benchmark encoding *)

module Point = struct
  type t = {
    x : int;
    y : int;
    color : string;
    prev : t option; (* previous position, say *)
  } [@@deriving yojson]

  let encode = CConv.Encode.(record_fix
    (fun self {x;y;color;prev} ->
      ("x", int, x) @->
      ("y", int, y) @->
      ("color", string, color) @->
      ("prev", option self, prev) @->
      record_end
    )
  ) ;;

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
  let p' = {x=1; y=3; color="yellow"; prev=Some p; }

  (* manual *)
  let rec to_json p = `Assoc
    [ "x", `Int p.x
    ; "y", `Int p.y
    ; "color", `String p.color
    ; "prev", match p.prev with
      | None -> `Null
      | Some p' -> to_json p'
    ]
end

module Lambda = struct
  type t =
    | Var of string
    | App of t * t
    | Lambda of string * t
    [@@deriving yojson]

  let encode = CConv.Encode.(sum_fix
    (fun self t -> match t with
      | Var s -> "var", hcons string s hnil
      | App (t1,t2) -> "app", hcons self t1 @@ hcons self t2 @@ hnil
      | Lambda (v,t') -> "lambda", hcons string v @@ hcons self t' @@ hnil
    )
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

  let rec to_json = function
    | Var s -> `List [`String "var"; `String s]
    | App (t1,t2) -> `List [`String "app"; to_json t1; to_json t2]
    | Lambda(x,t) -> `List [`String "lambda"; `String x; to_json t]
end

(* encode x with encoder *)
let bench_encoding encoder x =
  for _ = 1 to 10 do
    ignore (encoder x)
  done

let bench_encoding_point () =
  print_endline "\nbenchmark points";
  Benchmark.throughputN 4
    [ "manual", bench_encoding Point.to_json, Point.p'
    ; "cconv", bench_encoding (CConvYojson.encode Point.encode), Point.p'
    ; "deriving_yojson", bench_encoding Point.to_yojson, Point.p'
    ]

let bench_encoding_term () =
  print_endline "\nbenchmark terms";
  Benchmark.throughputN 4
    [ "manual", bench_encoding Lambda.to_json, Lambda.t1
    ; "cconv", bench_encoding (CConvYojson.encode Lambda.encode), Lambda.t1
    ; "deriving_yojson", bench_encoding Lambda.to_yojson, Lambda.t1
    ]

let () =
  Benchmark.tabulate (bench_encoding_point ());
  Benchmark.tabulate (bench_encoding_term ());
  ()
