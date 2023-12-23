module Coord = struct
  type t = int * int [@@deriving sexp, compare, equal]
end

module CoordMap = Map.Make (Coord)
module CoordSet = Set.Make (Coord)

let parse input =
  let grid =
    input
    |> String.split_lines
    |> List.concat_mapi ~f:(fun y l ->
      l |> String.to_list |> List.mapi ~f:(fun x c -> (x, y), c))
    |> CoordMap.of_alist_exn
  in
  let nodes = Map.to_alist grid in
  let max_y = nodes |> List.map ~f:(fun ((_, y), _) -> y) |> Util.List.max_int in
  let start =
    nodes
    |> List.filter ~f:(fun ((_, y), _) -> y = 0)
    |> List.find_map_exn ~f:(fun (k, v) -> if Char.equal v '.' then Some k else None)
  in
  let finish =
    nodes
    |> List.filter ~f:(fun ((_, y), _) -> y = max_y)
    |> List.find_map_exn ~f:(fun (k, v) -> if Char.equal v '.' then Some k else None)
  in
  grid, start, finish
;;

let longest_path grid start finish =
  let neighbors prev (x, y) =
    [ x + 1, y, '<'; x - 1, y, '>'; x, y + 1, '^'; x, y - 1, 'v' ]
    |> List.filter ~f:(fun (x, y, _) -> not (Coord.equal (x, y) prev))
    |> List.filter_map ~f:(fun (x, y, b) ->
      (x, y)
      |> Map.find grid
      |> Option.bind ~f:(function
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
      |> List.max_elt ~compare
      |> Option.value ~default:0
  in
  aux 0 start start
;;

let make_graph grid start finish =
  let neighbors visited (x, y) =
    [ x + 1, y; x - 1, y; x, y + 1; x, y - 1 ]
    |> List.filter ~f:(fun c -> not (Set.mem visited c))
    |> List.filter_map ~f:(fun c ->
      c
      |> Map.find grid
      |> Option.bind ~f:(function
        | '#' -> None
        | _ -> Some c))
  in
  let rec find_neighbors nodes steps visited = function
    | node when Set.mem nodes node -> [ node, steps ]
    | node ->
      node
      |> neighbors visited
      |> List.concat_map ~f:(fun node' ->
        find_neighbors nodes (steps + 1) (Set.add visited node') node')
  in
  let nodes =
    grid
    |> Map.keys
    |> List.filter ~f:(fun c -> List.length (neighbors CoordSet.empty c) > 2)
    |> CoordSet.of_list
    |> Set.union (CoordSet.of_list [ start; finish ])
  in
  Set.fold nodes ~init:CoordMap.empty ~f:(fun acc node ->
    let ns = find_neighbors (Set.remove nodes node) 0 (CoordSet.singleton node) node in
    Map.add_exn acc ~key:node ~data:ns)
;;

let longest_path' graph start finish =
  let rec aux steps visited = function
    | node when Coord.equal node finish -> steps
    | node ->
      node
      |> Map.find_exn graph
      |> List.filter ~f:(fun (node', _) -> not (Set.mem visited node'))
      |> List.map ~f:(fun (node', n) -> aux (steps + n) (Set.add visited node') node')
      |> List.max_elt ~compare
      |> Option.value ~default:0
  in
  aux 0 (CoordSet.singleton start) start
;;

let part1 input =
  let grid, start, finish = parse input in
  longest_path grid start finish
;;

let part2 input =
  let grid, start, finish = parse input in
  let graph = make_graph grid start finish in
  longest_path' graph start finish
;;
