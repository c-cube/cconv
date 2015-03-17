
open OUnit

module type S = sig
  type t [@@deriving cconv]
  val show : t -> string
  val name : string
  val examples : t list
end

module type TEST = sig
  val suite : OUnit.test (* automatically registered *)
end

let suites = ref []
let add_suite x = suites := x :: !suites

module Make(X : S) : TEST = struct
  let bij_json ex () =
    let j = CConvYojson.encode X.encode ex in
    match CConvYojson.decode X.decode j with
    | `Ok ex' ->
      assert_equal ~printer:X.show ex' ex
    | `Error msg -> assert_failure msg

  let bij_bencode ex () =
    let j = CConvBencode.encode X.encode ex in
    match CConvBencode.decode X.decode j with
    | `Ok ex' ->
      assert_equal ~printer:X.show ex' ex
    | `Error msg -> assert_failure msg

  let suite_of_example ex =
    [ "bij_json" >:: bij_json ex
    ; "bij_bencode" >:: bij_bencode ex
    ]

  let suite =
    X.name >::: (List.map suite_of_example X.examples |> List.flatten)

  let () = add_suite suite
end

module M1 = Make(struct
  type t = {
    x : int;
    y : int;
    color : string;
    prev : t option; (* previous position, say *)
  } [@@deriving show, cconv]

  let name = "point"
  let p = { x=1; y=2; color="red"; prev=None; }
  let p' = {x=1; y=3; color="yellow"; prev=Some p; }
  let examples = [p; p']
end)

module M2 = Make(struct
  type t =
    | Var of string
    | App of t * t
    | Lambda of string * t
  [@@deriving show, cconv]

  let name = "lambda-term"
  let t1 = Lambda ("x", App (Lambda ("y", App (Var "y", Var "x")), Var "x"))
  let examples = [t1]
end)

module M3 = struct
  module T = struct
    type boxed_int = {
      bint : int;
    } [@@deriving cconv, show]

    let box_int bint = {bint}
    let unbox_int {bint} = bint

    type t = {
      i : int
        [@encoder CConv.Encode.(map box_int encode_boxed_int)]
        [@decoder CConv.Decode.(map unbox_int decode_boxed_int)];
      j : int;
    }
    [@@deriving show, cconv]

    let name = "record_encoder"
    let t1 = { i=1; j=42 }
    let t2 = { i=10; j=0 }
    let t3 = { i=0; j=11 }
    let examples = [t1; t2; t3]
  end

  include Make(T)

  (* sort json record *)
  let sort_json = function
    | `Assoc l -> `Assoc (List.sort compare l)
    | x -> x

  let test_encode_yojson () =
    let json = CConvYojson.encode T.encode T.t1 |> sort_json in
    OUnit.assert_equal ~printer:(Yojson.Basic.pretty_to_string ~std:true)
      (`Assoc ["i", `Assoc ["bint", `Int 1]; "j", `Int 42]) json

  let suite2 = "" >:::
    [ "@encoder" >:: test_encode_yojson
    ]

  let () = add_suite suite2
end

module M4 = Make(struct
  type t = int * string [@@deriving cconv, show]
  let name = "pair"
  let examples = [1, "foo";  2, "bar"; max_int, ""]
end)

module M5 = Make(struct
  type t = bool * (int * float * int32 * unit) * string list array
             [@@deriving cconv]
  let show _ = "<too complicated>" (* TODO: wait for deriving show to work on unit *)
  let name = "bifi32usla"
  let examples = [
    true, (1, 3.14, 42l, ()), [| ["a"]; ["hello"; "world"] |]
  ]
end)

type record_ignore = {
  x : int;
  y : int [@ignore];
} [@@deriving show, cconv]

let test_record_ignore () =
  let r = { x=1; y=2} in
  let json = CConvYojson.encode encode_record_ignore r in
  OUnit.assert_equal ~printer:(Yojson.Basic.pretty_to_string ~std:true)
    (`Assoc ["x", `Int 1]) json;
  ()

let () = add_suite ("record_ignore" >:: test_record_ignore)

let _ =
  let suite = "cconv" >::: List.rev !suites in
  OUnit.run_test_tt_main suite

