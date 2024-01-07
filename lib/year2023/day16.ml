module Coord = struct
  type t = int * int [@@deriving eq, ord]
end

module CoordMap = Map.Make (Coord)

type dir =
  | N
  | E
  | S
  | W
[@@deriving eq, ord]

module Beam = struct
  type t = dir * Coord.t [@@deriving eq, ord]
end

module BeamSet = Set.Make (Beam)
module CoordSet = Set.Make (Coord)

let parse input =
  input
  |> String.lines
  |> List.mapi ~f:(fun y l -> l |> String.to_list |> List.mapi ~f:(fun x c -> (x, y), c))
  |> List.concat
  |> CoordMap.of_list
;;

let solve entry grid =
  let move dir (x, y) =
    match dir with
    | N -> x, y - 1
    | E -> x + 1, y
    | S -> x, y + 1
    | W -> x - 1, y
  in
  let move_beam (dir, coord) =
    let coord' = move dir coord in
    match CoordMap.find_opt coord' grid, dir with
    | Some '/', N | Some '\\', S -> [ E, coord' ]
    | Some '/', S | Some '\\', N -> [ W, coord' ]
    | Some '/', E | Some '\\', W -> [ N, coord' ]
    | Some '\\', E | Some '/', W -> [ S, coord' ]
    | Some '-', N | Some '-', S -> [ E, coord'; W, coord' ]
    | Some '|', W | Some '|', E -> [ N, coord'; S, coord' ]
    | Some '|', N | Some '|', S | Some '-', W | Some '-', E | Some '.', _ ->
      [ dir, coord' ]
    | _ -> []
  in
  let rec step visited = function
    | [] -> visited
    | h :: t when BeamSet.mem h visited -> step visited t
    | h :: t -> step (BeamSet.add h visited) (t @ move_beam h)
  in
  entry
  |> move_beam
  |> step BeamSet.empty
  |> BeamSet.to_list
  |> List.map ~f:snd
  |> CoordSet.of_list
  |> CoordSet.cardinal
;;

let part1 input = input |> parse |> solve (E, (-1, 0))

let part2 input =
  let grid = parse input in
  let coords = grid |> CoordMap.keys |> List.of_iter in
  let max_x = coords |> List.map ~f:fst |> List.reduce_exn ~f:max in
  let max_y = coords |> List.map ~f:snd |> List.reduce_exn ~f:max in
  let h_starts =
    List.(0 --^ max_y) |> List.concat_map ~f:(fun y -> [ E, (-1, y); W, (max_x, y) ])
  in
  let v_starts =
    List.(0 --^ max_x) |> List.concat_map ~f:(fun x -> [ S, (x, -1); N, (max_y, x) ])
  in
  h_starts @ v_starts
  |> List.map ~f:(fun entry -> solve entry grid)
  |> List.reduce_exn ~f:max
;;
