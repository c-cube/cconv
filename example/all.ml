
(** {1 Examples} *)

open CConv

module Json = struct
  type t = [
    | `Int of int
    | `Float of float
    | `Bool of bool
    | `Null
    | `String of string
    | `List of t list
    | `Assoc of (string * t) list
  ]

  let source =
    let module U = UniversalSource in
    let rec visit : type b. b Sink.t -> t -> b =
    fun sink x -> match x with
      | `Int i -> U.int_ sink i
      | `Float f -> U.float_ sink f
      | `Bool b -> U.bool_ sink b
      | `Null -> U.unit_ sink
      | `String s ->
          begin match Sink.expected sink with
          | Sink.ExpectSum -> U.sum ~src sink s []
          | _ -> U.string_ sink s
          end
      | `List ((`String name :: l) as l') ->
          begin match Sink.expected sink with
          | Sink.ExpectSum -> U.sum ~src sink name l
          | _ -> U.list_ ~src sink l'
          end
      | `List l -> U.list_ ~src sink l
      | `Assoc l -> U.record ~src sink l
    and src = { U.visit=visit; } in
    src

  let sink : t UniversalSink.t =
    let open UniversalSink in
    { unit_ = `Null;
      bool_ = (fun b -> `Bool b);
      float_ = (fun f -> `Float f);
      int_ = (fun i -> `Int i);
      string_ = (fun s -> `String s);
      list_ = (fun l -> `List l);
      record = (fun l -> `Assoc l);
      tuple = (fun l -> `List l);
      sum = (fun name l -> match l with
        | [] -> `String name
        | _::_ -> `List (`String name :: l));
    }
end

module Sexp = struct
  type t =
    | Atom of string
    | List of t list

  let source =
    let module U = UniversalSource in
    let rec visit : type b. b Sink.t -> t -> b =
    fun sink x -> match x, Sink.expected sink with
      | Atom s, Sink.ExpectSum -> U.sum ~src sink s []
      | List (Atom name :: l), Sink.ExpectSum -> U.sum ~src sink name l
      | List l, Sink.ExpectRecord ->
          let l' = List.map (function
            | List [Atom name; x] -> name, x
            | _ -> report_error "get List, but expected Record") l
          in U.record ~src sink l'
      | Atom s, _ -> U.string_ sink s
      | List [], Sink.ExpectUnit -> U.unit_ sink
      | List l, _ -> U.list_ ~src sink l
    and src = { U.visit=visit; } in
    src

  let sink =
    let open UniversalSink in
    { unit_ = List [];
      bool_ = (fun b -> Atom (string_of_bool b));
      float_ = (fun f -> Atom (string_of_float f));
      int_ = (fun i -> Atom (string_of_int i));
      string_ = (fun s -> Atom (String.escaped s));
      list_ = (fun l -> List l);
      record = (fun l -> List (List.map (fun (a,b) -> List [Atom a; b]) l));
      tuple = (fun l -> List l);
      sum = (fun name l -> match l with
        | [] -> Atom name
        | _::_ -> List (Atom name :: l));
    }

  let rec fmt out = function
    | Atom s -> Format.pp_print_string out s
    | List l ->
        Format.pp_print_char out '(';
        List.iteri (fun i s ->
          if i > 0 then Format.pp_print_char out ' ';
          fmt out s) l;
        Format.pp_print_char out ')'
end

module Bencode = struct
  type t =
    | Int of int
    | String of string
    | List of t list
    | Assoc of (string * t) list

  let source =
    let module U = UniversalSource in
    let rec visit : type b. b Sink.t -> t -> b =
    fun sink x -> match x, Sink.expected sink with
      | String s, Sink.ExpectSum -> U.sum ~src sink s []
      | List (String name :: l), Sink.ExpectSum -> U.sum ~src sink name l
      | Assoc l, _ -> U.record ~src sink l
      | String s, _ -> U.string_ sink s
      | List [], Sink.ExpectUnit -> U.unit_ sink
      | List l, _ -> U.list_ ~src sink l
      | Int 0, Sink.ExpectUnit -> U.unit_ sink
      | Int i, _ -> U.int_ sink i
    and src = { U.visit=visit; } in
    src

  let sink =
    let open UniversalSink in
    { unit_ = Int 0;
      bool_ = (fun b -> Int (if b then 1 else 0));
      float_ = (fun f -> String (string_of_float f));
      int_ = (fun i -> Int i);
      string_ = (fun s -> String s);
      list_ = (fun l -> List l);
      record = (fun l -> Assoc l);
      tuple = (fun l -> List l);
      sum = (fun name l -> match l with
        | [] -> String name
        | _::_ -> List (String name :: l));
    }
end

(* tests *)

module Point = struct
  type t = {
    x : int;
    y : int;
    color : string;
    prev : t option; (* previous position, say *)
  }

  let sink =
    Sink.(record_fix
      (fun self ->
        field "x" int_ @@ fun x ->
        field "y" int_ @@ fun y ->
        field "color" string_ @@ fun color ->
        field "prev" (opt self) @@ fun prev ->
        yield_record {x;y;color;prev}
      ))

  let source =
    Source.(record_fix
      (fun self ->
        field "x" (fun p -> p.x) int_ @@
        field "y" (fun p -> p.y) int_ @@
        field "color" (fun p -> p.color) string_ @@
        field "prev" (fun p -> p.prev) (opt self) @@
        record_stop
      ))

  let p = {x=1; y=42; color="yellow";
           prev = Some {x=1; y=41; color="red"; prev=None};}

  let p2 = into source Json.sink p

  let p3 = from Json.source sink p2

  let p4 = into source Json.sink p3

  let p2_sexp = into source Sexp.sink p

  let p3_sexp = from Sexp.source sink p2_sexp

  let p4_sexp = into source Sexp.sink p3_sexp
end

module Lambda = struct
  type t =
    | Var of string
    | App of t * t
    | Lambda of string * t

  let source = Source.(sum_fix
    (fun self t -> match t with
        | Var s -> "var", hcons string_ s @@ hnil
        | App (t1, t2) -> "app", hcons self t1 @@ hcons self t2 @@ hnil
        | Lambda (s, t) -> "lam", hcons string_ s @@ hcons self t @@ hnil
      ))

  let sink = Sink.(sum_fix
    (fun self str -> match str with
      | "var" -> string_ |+| fun s -> yield (Var s)
      | "app" -> self |+| fun t1 -> self |+| fun t2 -> yield (App (t1, t2))
      | "lam" -> string_ |+| fun s -> self |+| fun t -> yield (Lambda (s, t))
      | _ -> report_error "expected lambda term"
    ))

  let t1 = Lambda ("x", App (Lambda ("y", App (Var "y", Var "x")), Var "x"))

  let t1_json = into source Json.sink t1
  let t1_bencode = into source Bencode.sink t1
  let t1_sexp = into source Sexp.sink t1
end
