module Coord = struct
  type t = int * int [@@deriving eq, ord]
end

module CoordMap = Map.Make (Coord)
module CoordSet = Set.Make (Coord)

type dir =
  | N
  | S
  | E
  | W

let make_grid input =
  input
  |> String.lines
  |> List.mapi ~f:(fun y line ->
    line |> String.to_list |> List.mapi ~f:(fun x c -> (x, y), c))
  |> List.concat
  |> CoordMap.of_list
;;

let find_start grid =
  grid
  |> CoordMap.to_list
  |> List.find_map ~f:(fun (k, v) -> if Char.equal 'S' v then Some k else None)
  |> Option.get_exn_or "not found"
;;

let neighbors grid (x, y) =
  [ E, (x + 1, y); W, (x - 1, y); S, (x, y + 1); N, (x, y - 1) ]
  |> List.filter_map ~f:(fun (d, c) ->
    grid |> CoordMap.find_opt c |> Option.map (fun v -> d, v))
;;

let initial_dir start grid =
  start
  |> neighbors grid
  |> List.find_map ~f:(function
    | N, v when List.mem v [ '|'; 'F'; '7' ] ~eq:Char.equal -> Some (N, v)
    | E, v when List.mem v [ '-'; 'J'; '7' ] ~eq:Char.equal -> Some (E, v)
    | S, v when List.mem v [ '|'; 'J'; 'L' ] ~eq:Char.equal -> Some (S, v)
    | W, v when List.mem v [ '-'; 'F'; 'L' ] ~eq:Char.equal -> Some (W, v)
    | _ -> None)
  |> Option.get_exn_or "empty"
  |> fst
;;

let follow_path grid start dir =
  let move (x, y) = function
    | N -> x, y - 1
    | S -> x, y + 1
    | E -> x + 1, y
    | W -> x - 1, y
  in
  let next_dir dir coord =
    let v = CoordMap.find coord grid in
    match dir, v with
    | N, '|' -> N
    | N, 'F' -> E
    | N, '7' -> W
    | S, '|' -> S
    | S, 'J' -> W
    | S, 'L' -> E
    | W, 'F' -> S
    | W, '-' -> W
    | W, 'L' -> N
    | E, '-' -> E
    | E, 'J' -> N
    | E, '7' -> S
    | _ -> failwith "boom"
  in
  let rec aux path coord dir =
    let coord' = move coord dir in
    let path' = CoordSet.add coord' path in
    if Coord.equal coord' start
    then path'
    else (
      let dir' = next_dir dir coord' in
      aux path' coord' dir')
  in
  aux CoordSet.empty start dir
;;

let in_loop s_vertical grid path (x, y) =
  let sum =
    List.range 0 x
    |> List.map ~f:(fun x' -> x', y)
    |> List.filter ~f:(fun c -> CoordSet.mem c path)
    |> List.map ~f:(fun c ->
      match CoordMap.find c grid with
      | '|' | 'J' | 'L' -> 1
      | 'S' when s_vertical -> 1
      | _ -> 0)
    |> Util.List.sum_int
  in
  sum mod 2 = 1
;;

let part1 input =
  let grid = make_grid input in
  let start = find_start grid in
  let dir = initial_dir start grid in
  let l = follow_path grid start dir |> CoordSet.cardinal in
  l / 2
;;

let part2 s_vertical input =
  let grid = make_grid input in
  let start = find_start grid in
  let dir = initial_dir start grid in
  let path = follow_path grid start dir in
  grid
  |> CoordMap.filter (fun c _ -> not (CoordSet.mem c path))
  |> CoordMap.keys
  |> List.of_iter
  |> List.filter ~f:(in_loop s_vertical grid path)
  |> List.length
;;
