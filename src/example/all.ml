
(** {1 Examples} *)

(* tests *)

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
      }
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
end

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
