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
  let%map x = P.signed_integer <* P.char ',' <* P.spaces >>| Q.of_int
  and y = P.signed_integer <* P.char ',' <* P.spaces >>| Q.of_int
  and z = P.signed_integer <* P.spaces <* P.char '@' <* P.spaces >>| Q.of_int
  and dx = P.signed_integer <* P.char ',' <* P.spaces >>| Q.of_int
  and dy = P.signed_integer <* P.char ',' <* P.spaces >>| Q.of_int
  and dz = P.signed_integer >>| Q.of_int in
  { x; y; z; dx; dy; dz }
;;

let stones_p = P.sep_by1 P.end_of_line stone_p

let to_line { x; y; dx; dy } =
  let open Q in
  let x1 = x in
  let y1 = y in
  let x2 = x1 + dx in
  let y2 = y1 + dy in
  { a = y1 - y2; b = x2 - x1; c = ~-((x1 * y2) - (x2 * y1)) }
;;

let in_test_area target (h1, h2) =
  let in_range (a, b) v = Q.compare a v <= 0 && Q.compare v b <= 0 in
  let sign f = Q.compare f Q.zero in
  let in_future h1 h2 x y =
    sign Q.(x - h1.x) = sign h1.dx
    && sign Q.(y - h1.y) = sign h1.dy
    && sign Q.(x - h2.x) = sign h2.dx
    && sign Q.(y - h2.y) = sign h2.dy
  in
  let open Q in
  let l1 = to_line h1 in
  let l2 = to_line h2 in
  let d = (l1.a * l2.b) - (l1.b * l2.a) in
  let dx = (l1.c * l2.b) - (l1.b * l2.c) in
  let dy = (l1.a * l2.c) - (l1.c * l2.a) in
  if Q.equal d Q.zero
  then false
  else (
    let x = dx / d in
    let y = dy / d in
    in_range target x && in_range target y && in_future h1 h2 x y)
;;

let make_matrix stones ~f =
  let m = List.map stones ~f in
  List.take m 4
  |> List.map ~f:(fun d ->
    List.zip_exn d (List.last_exn m) |> List.map ~f:(fun (a, b) -> Q.(a - b)))
  |> List.map ~f:Array.of_list
  |> Array.of_list
;;

let elim m =
  let l = Array.length m in
  List.range 0 l
  |> List.iter ~f:(fun i ->
    let t = m.(i).(i) in
    m.(i) <- Array.map m.(i) ~f:(fun x -> Q.(x / t));
    List.range (i + 1) l
    |> List.iter ~f:(fun j ->
      let t = m.(j).(i) in
      m.(j) <- Array.mapi m.(j) ~f:(fun k x -> Q.(x - (t * m.(i).(k))))));
  List.range 0 l
  |> List.rev
  |> List.iter ~f:(fun i ->
    List.range 0 i
    |> List.iter ~f:(fun j ->
      let t = m.(j).(i) in
      m.(j) <- Array.mapi m.(j) ~f:(fun k x -> Q.(x - (t * m.(i).(k))))));
  Array.map m ~f:(fun r -> Array.last r)
;;

let part1 target input =
  input
  |> P.parse_exn stones_p
  |> Util.List.pairs
  |> List.filter ~f:(in_test_area target)
  |> List.length
;;

(* all credit to https://github.com/tckmn/polyaoc-2023/blob/97689dc6b5ff38c557cd885b10be425e14928958/24/rb/24.rb#L22 *)
let part2 input =
  let open Q in
  let stones = P.parse_exn stones_p input in
  let a =
    make_matrix stones ~f:(fun h ->
      [ ~-(h.dy); h.dx; h.y; ~-(h.x); (h.y * h.dx) - (h.x * h.dy) ])
  in
  let b =
    make_matrix stones ~f:(fun h ->
      [ ~-(h.dy); h.dz; h.y; ~-(h.z); (h.y * h.dz) - (h.z * h.dy) ])
  in
  let r1 = elim a in
  let r2 = elim b in
  r1.(0) + r1.(1) + r2.(0) |> Q.to_int
;;
