module Coord = struct
  type t = int * int [@@deriving eq, ord]
end

module CoordMap = Map.Make (Coord)
module CoordSet = Set.Make (Coord)

let parse input =
  let grid =
    input
    |> String.lines
    |> List.mapi ~f:(fun y l ->
      l |> String.to_list |> List.mapi ~f:(fun x c -> (x, y), c))
    |> List.concat
    |> CoordMap.of_list
  in
  let nodes = CoordMap.to_list grid in
  let max_y = nodes |> List.map ~f:(fun ((_, y), _) -> y) |> Util.List.max_int in
  let start =
    nodes
    |> List.filter ~f:(fun ((_, y), _) -> y = 0)
    |> List.find_map ~f:(fun (k, v) -> if Char.equal v '.' then Some k else None)
    |> Option.get_exn_or "none"
  in
  let finish =
    nodes
    |> List.filter ~f:(fun ((_, y), _) -> y = max_y)
    |> List.find_map ~f:(fun (k, v) -> if Char.equal v '.' then Some k else None)
    |> Option.get_exn_or "none"
  in
  grid, start, finish
;;

let longest_path (grid, start, finish) =
  let neighbors prev (x, y) =
    [ x + 1, y, '<'; x - 1, y, '>'; x, y + 1, '^'; x, y - 1, 'v' ]
    |> List.filter ~f:(fun (x, y, _) -> not (Coord.equal (x, y) prev))
    |> List.filter_map ~f:(fun (x, y, b) ->
      grid
      |> CoordMap.find_opt (x, y)
      |> (Fun.flip Option.bind) (function
        | '.' -> Some (x, y)
        | '#' -> None
        | c when not (Char.equal c b) -> Some (x, y)
        | _ -> None))
  in
  let rec aux steps prev = function
    | node when Coord.equal node finish -> steps
    | node ->
      node
      |> neighbors prev
      |> List.map ~f:(fun node' -> aux (steps + 1) node node')
      |> List.fold_left ~init:0 ~f:max
  in
  aux 0 start start
;;

let make_graph (grid, start, finish) =
  let neighbors visited (x, y) =
    [ x + 1, y; x - 1, y; x, y + 1; x, y - 1 ]
    |> List.filter ~f:(fun c -> not (CoordSet.mem c visited))
    |> List.filter_map ~f:(fun c ->
      grid
      |> CoordMap.find_opt c
      |> (Fun.flip Option.bind) (function
        | '#' -> None
        | _ -> Some c))
  in
  let rec find_neighbors nodes steps visited = function
    | node when CoordSet.mem node nodes -> [ node, steps ]
    | node ->
      node
      |> neighbors visited
      |> List.concat_map ~f:(fun node' ->
        find_neighbors nodes (steps + 1) (CoordSet.add node' visited) node')
  in
  let nodes =
    grid
    |> CoordMap.keys
    |> List.of_iter
    |> List.filter ~f:(fun c -> List.length (neighbors CoordSet.empty c) > 2)
    |> CoordSet.of_list
    |> CoordSet.union (CoordSet.of_list [ start; finish ])
  in
  let graph =
    CoordSet.fold
      (fun node acc ->
        let ns =
          find_neighbors (CoordSet.remove node nodes) 0 (CoordSet.singleton node) node
        in
        CoordMap.add node ns acc)
      nodes
      CoordMap.empty
  in
  graph, start, finish
;;

let longest_path' (graph, start, finish) =
  let rec aux steps visited = function
    | node when Coord.equal node finish -> steps
    | node ->
      graph
      |> CoordMap.find node
      |> List.filter ~f:(fun (node', _) -> not (CoordSet.mem node' visited))
      |> List.map ~f:(fun (node', n) ->
        aux (steps + n) (CoordSet.add node' visited) node')
      |> List.fold_left ~init:0 ~f:max
  in
  aux 0 (CoordSet.singleton start) start
;;

let part1 input = input |> parse |> longest_path
let part2 input = input |> parse |> make_graph |> longest_path'
