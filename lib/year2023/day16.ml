module Coord = struct
  type t = int * int [@@deriving sexp, compare, equal]
end

module CoordMap = Map.Make (Coord)

type dir =
  | N
  | E
  | S
  | W
[@@deriving sexp, compare]

module Beam = struct
  type t = dir * Coord.t [@@deriving sexp, compare]
end

module BeamSet = Set.Make (Beam)
module CoordSet = Set.Make (Coord)

let parse input =
  input
  |> String.split_lines
  |> List.concat_mapi ~f:(fun y l ->
    l |> String.to_list |> List.mapi ~f:(fun x c -> (x, y), c))
  |> CoordMap.of_alist_exn
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
    match Map.find grid coord', dir with
    | Some '.', dir -> [ dir, coord' ]
    | Some '/', N -> [ E, coord' ]
    | Some '/', S -> [ W, coord' ]
    | Some '/', E -> [ N, coord' ]
    | Some '/', W -> [ S, coord' ]
    | Some '\\', N -> [ W, coord' ]
    | Some '\\', S -> [ E, coord' ]
    | Some '\\', E -> [ S, coord' ]
    | Some '\\', W -> [ N, coord' ]
    | Some '-', W | Some '-', E -> [ dir, coord' ]
    | Some '-', N | Some '-', S -> [ E, coord'; W, coord' ]
    | Some '|', W | Some '|', E -> [ N, coord'; S, coord' ]
    | Some '|', N | Some '|', S -> [ dir, coord' ]
    | _ -> []
  in
  let rec step visited = function
    | [] -> visited
    | h :: t when Set.mem visited h -> step visited t
    | h :: t -> step (Set.add visited h) (t @ move_beam h)
  in
  entry |> move_beam |> step BeamSet.empty |> CoordSet.map ~f:snd |> Set.length
;;

let part1 input = input |> parse |> solve (E, (-1, 0))

let part2 input =
  let grid = parse input in
  let coords = Map.keys grid in
  let max_x = coords |> List.map ~f:fst |> List.max_elt ~compare |> Option.value_exn in
  let max_y = coords |> List.map ~f:snd |> List.max_elt ~compare |> Option.value_exn in
  let h_starts =
    List.range 0 max_y |> List.concat_map ~f:(fun y -> [ E, (-1, y); W, (max_x, y) ])
  in
  let v_starts =
    List.range 0 max_x |> List.concat_map ~f:(fun x -> [ S, (x, -1); N, (max_y, x) ])
  in
  h_starts @ v_starts
  |> List.map ~f:(fun entry -> solve entry grid)
  |> List.max_elt ~compare
  |> Option.value_exn
;;
