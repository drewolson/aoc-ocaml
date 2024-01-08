module A = Z3.Arithmetic
module B = Z3.Boolean
module I = Z3.Arithmetic.Integer
module P = Util.Parser
open P.Syntax

type stone =
  { x : Q.t
  ; y : Q.t
  ; z : Q.t
  ; dx : Q.t
  ; dy : Q.t
  ; dz : Q.t
  }

type line =
  { a : Q.t
  ; b : Q.t
  ; c : Q.t
  }

let stone_p =
  let+ x = P.signed_integer <* P.char ',' <* P.spaces >>| Q.of_int
  and+ y = P.signed_integer <* P.char ',' <* P.spaces >>| Q.of_int
  and+ z = P.signed_integer <* P.spaces <* P.char '@' <* P.spaces >>| Q.of_int
  and+ dx = P.signed_integer <* P.char ',' <* P.spaces >>| Q.of_int
  and+ dy = P.signed_integer <* P.char ',' <* P.spaces >>| Q.of_int
  and+ dz = P.signed_integer >>| Q.of_int in
  { x; y; z; dx; dy; dz }
;;

let stones_p = P.sep_by1 P.end_of_line stone_p

let in_test_area target (h1, h2) =
  let open Q in
  let to_line { x; y; dx; dy } =
    let x' = x + dx in
    let y' = y + dy in
    { a = y - y'; b = x' - x; c = ~-((x * y') - (x' * y)) }
  in
  let in_range (a, b) v = a <= v && v <= b in
  let sign f = ~$(compare f ~$0) in
  let in_future h1 h2 x y =
    sign (x - h1.x) = sign h1.dx
    && sign (y - h1.y) = sign h1.dy
    && sign (x - h2.x) = sign h2.dx
    && sign (y - h2.y) = sign h2.dy
  in
  let l1 = to_line h1 in
  let l2 = to_line h2 in
  let d = (l1.a * l2.b) - (l1.b * l2.a) in
  let dx = (l1.c * l2.b) - (l1.b * l2.c) in
  let dy = (l1.a * l2.c) - (l1.c * l2.a) in
  if d = ~$0
  then false
  else (
    let x = dx / d in
    let y = dy / d in
    in_range target x && in_range target y && in_future h1 h2 x y)
;;

let solve_system stones =
  let check_sat_exn s =
    match Z3.Solver.check s [] with
    | UNKNOWN -> failwith "UNKNOWN"
    | UNSATISFIABLE -> failwith "UNSATISFIABLE"
    | SATISFIABLE -> ()
  in
  let get_model_exn s =
    check_sat_exn s;
    s |> Z3.Solver.get_model |> Option.value_exn
  in
  let ctx = Z3.mk_context [ "model", "true"; "proof", "false" ] in
  let zero = I.mk_numeral_i ctx 0 in
  let x = I.mk_const_s ctx "x" in
  let y = I.mk_const_s ctx "y" in
  let z = I.mk_const_s ctx "z" in
  let dx = I.mk_const_s ctx "dx" in
  let dy = I.mk_const_s ctx "dy" in
  let dz = I.mk_const_s ctx "dz" in
  let s = Z3.Solver.mk_simple_solver ctx in
  stones
  |> Util.List.take ~n:3
  |> List.iteri ~f:(fun i h ->
    let t = I.mk_const_s ctx (Printf.sprintf "t%i" i) in
    Z3.Solver.add
      s
      [ A.mk_gt ctx t zero
      ; B.mk_eq
          ctx
          (A.mk_add ctx [ x; A.mk_mul ctx [ dx; t ] ])
          (A.mk_add
             ctx
             [ I.mk_numeral_i ctx (Q.to_int h.x)
             ; A.mk_mul ctx [ I.mk_numeral_i ctx (Q.to_int h.dx); t ]
             ])
      ; B.mk_eq
          ctx
          (A.mk_add ctx [ y; A.mk_mul ctx [ dy; t ] ])
          (A.mk_add
             ctx
             [ I.mk_numeral_i ctx (Q.to_int h.y)
             ; A.mk_mul ctx [ I.mk_numeral_i ctx (Q.to_int h.dy); t ]
             ])
      ; B.mk_eq
          ctx
          (A.mk_add ctx [ z; A.mk_mul ctx [ dz; t ] ])
          (A.mk_add
             ctx
             [ I.mk_numeral_i ctx (Q.to_int h.z)
             ; A.mk_mul ctx [ I.mk_numeral_i ctx (Q.to_int h.dz); t ]
             ])
      ]);
  let m = get_model_exn s in
  let result = A.mk_add ctx [ x; y; z ] in
  Z3.Model.eval m result false |> Option.value_exn |> Z3.Expr.to_string |> Int.of_string
;;

let part1 target input =
  input
  |> P.parse_exn stones_p
  |> Util.List.pairs
  |> List.filter ~f:(in_test_area target)
  |> List.length
;;

let part2 input = input |> P.parse_exn stones_p |> solve_system
