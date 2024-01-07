module P = Util.Parser
open P.Syntax

module Node = struct
  type t = string [@@deriving eq, ord]
end

module Edge = struct
  type t = Node.t * Node.t [@@deriving eq, ord]
end

module NodeSet = Set.Make (Node)
module EdgeSet = Set.Make (Edge)

let token_p =
  P.take_while (function
    | 'a' .. 'z' -> true
    | _ -> false)
;;

let line_p =
  let+ node = token_p <* P.string ": "
  and+ nodes = P.sep_by1 (P.char ' ') token_p in
  node, nodes
;;

let lines_p = P.sep_by1 P.end_of_line line_p
let make_edge a b = if String.compare a b < 0 then a, b else b, a

let make_graph lines =
  let aux (ns, es) (n, conns) =
    let nodes = n :: conns in
    let edges = List.map conns ~f:(make_edge n) in
    let ns' = List.fold_left nodes ~init:ns ~f:(Fun.flip NodeSet.add) in
    let es' = List.fold_left edges ~init:es ~f:(Fun.flip EdgeSet.add) in
    ns', es'
  in
  let nodes, edges = List.fold_left lines ~init:(NodeSet.empty, EdgeSet.empty) ~f:aux in
  nodes, EdgeSet.to_list edges
;;

let solve graph =
  let rec aux = function
    | nodes, edges when NodeSet.cardinal nodes = 2 && List.length edges = 3 ->
      Some (NodeSet.fold (fun node acc -> acc * (String.length node / 3)) nodes 1)
    | nodes, _ when NodeSet.cardinal nodes = 2 -> None
    | nodes, edges ->
      let n = edges |> List.length |> Random.int |> Random.run in
      let a, b = List.nth edges n in
      let ab = a ^ b in
      let edges' =
        List.filter_map edges ~f:(function
          | e when Edge.equal e (a, b) -> None
          | x, y when Node.equal x a || Node.equal x b -> Some (make_edge ab y)
          | x, y when Node.equal y a || Node.equal y b -> Some (make_edge x ab)
          | e -> Some e)
      in
      let nodes' = nodes |> NodeSet.remove a |> NodeSet.remove b |> NodeSet.add ab in
      aux (nodes', edges')
  in
  Seq.ints 0 |> Seq.find_map (fun _ -> aux graph) |> Option.get_exn_or "none"
;;

let part1 input = input |> P.parse_exn lines_p |> make_graph |> solve
