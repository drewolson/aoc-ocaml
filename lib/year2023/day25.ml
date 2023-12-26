module P = Util.Parser
open P.Syntax

module Node = struct
  type t = string [@@deriving sexp, equal, compare]
end

module Edge = struct
  type t = Node.t * Node.t [@@deriving sexp, equal, compare]
end

module EdgeSet = Set.Make (Edge)
module NodeSet = Set.Make (Node)

let token_p =
  P.take_while (function
    | 'a' .. 'z' -> true
    | _ -> false)
;;

let line_p =
  let%map node = token_p <* P.string ": "
  and nodes = P.sep_by1 (P.char ' ') token_p in
  node, nodes
;;

let lines_p = P.sep_by1 P.end_of_line line_p
let make_edge a b = if String.compare a b < 0 then a, b else b, a

let make_graph lines =
  let aux (ns, es) (n, conns) =
    let nodes = n :: conns in
    let edges = conns |> List.map ~f:(fun c -> make_edge n c) in
    let ns' = List.fold nodes ~init:ns ~f:(fun acc n -> Set.add acc n) in
    let es' = List.fold edges ~init:es ~f:(fun acc e -> Set.add acc e) in
    ns', es'
  in
  let nodes, edges = List.fold lines ~init:(NodeSet.empty, EdgeSet.empty) ~f:aux in
  nodes, Set.to_list edges
;;

let solve graph =
  let rec aux (nodes, edges) =
    let ln = Set.length nodes in
    let le = List.length edges in
    if ln = 2
    then
      if le = 3
      then
        Some (Set.fold nodes ~init:1 ~f:(fun sum node -> sum * (String.length node / 3)))
      else None
    else (
      let n = Random.int le in
      let a, b = List.nth_exn edges n in
      let ab = a ^ b in
      let edges' =
        List.filter_map edges ~f:(function
          | e when Edge.equal e (a, b) -> None
          | x, y when Node.equal x a || Node.equal x b -> Some (make_edge ab y)
          | x, y when Node.equal y a || Node.equal y b -> Some (make_edge x ab)
          | e -> Some e)
      in
      let nodes' = Set.add (Set.remove (Set.remove nodes a) b) ab in
      aux (nodes', edges'))
  in
  Util.Sequence.nats |> Sequence.find_map ~f:(fun _ -> aux graph) |> Option.value_exn
;;

let part1 input = input |> P.parse_exn lines_p |> make_graph |> solve
