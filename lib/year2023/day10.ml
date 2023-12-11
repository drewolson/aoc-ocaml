module Coord = struct
  type t = int * int [@@deriving compare, equal, sexp]
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
  |> String.split_lines
  |> List.concat_mapi ~f:(fun y line ->
    line |> String.to_list |> List.mapi ~f:(fun x c -> (x, y), c))
  |> CoordMap.of_alist_exn
;;

let find_start grid =
  grid
  |> Map.to_alist
  |> List.find_map ~f:(fun (k, v) -> if Char.equal 'S' v then Some k else None)
  |> Option.value_exn
;;

let neighbor_coords (x, y) = [ x + 1, y; x - 1, y; x, y + 1; x, y - 1 ]

let neighbors grid coord =
  coord
  |> neighbor_coords
  |> List.zip_exn [ E; W; S; N ]
  |> List.filter_map ~f:(fun (d, c) -> Map.find grid c |> Option.map ~f:(fun v -> d, v))
;;

let initial_dir start grid =
  start
  |> neighbors grid
  |> List.find_map ~f:(function
    | N, v when List.mem [ '|'; 'F'; '7' ] v ~equal:Char.equal -> Some (N, v)
    | E, v when List.mem [ '-'; 'J'; '7' ] v ~equal:Char.equal -> Some (E, v)
    | S, v when List.mem [ '|'; 'J'; 'L' ] v ~equal:Char.equal -> Some (S, v)
    | W, v when List.mem [ '-'; 'F'; 'L' ] v ~equal:Char.equal -> Some (W, v)
    | _ -> None)
  |> Option.value_exn
;;

let follow_path grid start dir =
  let move (x, y) = function
    | N -> x, y - 1
    | S -> x, y + 1
    | E -> x + 1, y
    | W -> x - 1, y
  in
  let next_dir dir coord =
    let v = Map.find_exn grid coord in
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
    let path' = Set.add path coord' in
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
    |> List.filter ~f:(Set.mem path)
    |> List.map ~f:(fun c ->
      match Map.find_exn grid c with
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
  let dir, coord = initial_dir start grid in
  let l = follow_path grid start dir |> Set.length in
  l / 2
;;

let part2 s_vertical input =
  let grid = make_grid input in
  let start = find_start grid in
  let dir, coord = initial_dir start grid in
  let path = follow_path grid start dir in
  grid
  |> Map.filter_keys ~f:(fun c -> not (Set.mem path c))
  |> Map.keys
  |> List.filter ~f:(in_loop s_vertical grid path)
  |> List.length
;;
