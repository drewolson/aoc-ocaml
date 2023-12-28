module P = Util.Parser
open P.Syntax

module Node = struct
  type t = string [@@deriving sexp, equal, compare]
end

module Edge = struct
  type t = Node.t * Node.t [@@deriving sexp, equal, compare]
end

module NodeSet = Set.Make (Node)
module EdgeSet = Set.Make (Edge)

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
    let edges = List.map conns ~f:(make_edge n) in
    let ns' = List.fold nodes ~init:ns ~f:Set.add in
    let es' = List.fold edges ~init:es ~f:Set.add in
    ns', es'
  in
  let nodes, edges = List.fold lines ~init:(NodeSet.empty, EdgeSet.empty) ~f:aux in
  nodes, Set.to_list edges
;;

let solve graph =
  let rec aux = function
    | nodes, edges when Set.length nodes = 2 && List.length edges = 3 ->
      Some (Set.fold nodes ~init:1 ~f:(fun acc node -> acc * (String.length node / 3)))
    | nodes, _ when Set.length nodes = 2 -> None
    | nodes, edges ->
      let n = edges |> List.length |> Random.int in
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
      aux (nodes', edges')
  in
  Util.Sequence.nats |> Sequence.find_map ~f:(fun _ -> aux graph) |> Option.value_exn
;;

let part1 input = input |> P.parse_exn lines_p |> make_graph |> solve
