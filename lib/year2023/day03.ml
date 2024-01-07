module Coord = struct
  type t = int * int [@@deriving eq, ord]
end

module CoordMap = Map.Make (Coord)

let make_grid input =
  input
  |> String.lines
  |> List.mapi ~f:(fun y line ->
    line |> String.to_list |> List.mapi ~f:(fun x c -> (y, x), c))
  |> List.concat
  |> CoordMap.of_list
;;

let find_num_locs grid =
  let max_x =
    grid |> CoordMap.keys |> List.of_iter |> List.map ~f:snd |> List.reduce_exn ~f:max
  in
  CoordMap.fold
    (fun key data (curr, l) ->
      match data, curr with
      | '0' .. '9', None -> Some key, l
      | '0' .. '9', _ -> curr, l
      | _, Some ((_, xstart) as start) ->
        let x = snd key in
        if x = 0
        then None, (start, max_x + 1 - xstart) :: l
        else None, (start, x - xstart) :: l
      | _, _ -> curr, l)
    grid
    (None, [])
  |> snd
;;

let neighbors ((y, x), len) =
  List.(x --^ (x + len))
  |> List.concat_map ~f:(fun x' ->
    [ y - 1, x' - 1
    ; y, x' - 1
    ; y + 1, x' - 1
    ; y - 1, x'
    ; y + 1, x'
    ; y - 1, x' + 1
    ; y, x' + 1
    ; y + 1, x' + 1
    ])
;;

let is_part_num grid num_loc =
  num_loc
  |> neighbors
  |> List.filter_map ~f:(fun c -> CoordMap.find_opt c grid)
  |> List.exists ~f:(function
    | '0' .. '9' -> false
    | '.' -> false
    | _ -> true)
;;

let loc_to_num grid ((y, x), len) =
  List.(x --^ (x + len))
  |> List.filter_map ~f:(fun x' -> CoordMap.find_opt (y, x') grid)
  |> String.of_list
  |> Int.of_string_exn
;;

let gear_ratio grid num_locs key data =
  if not (Char.equal data '*')
  then None
  else (
    let match_num_locs =
      List.filter num_locs ~f:(fun num_loc ->
        let ns = neighbors num_loc in
        List.mem ~eq:Coord.equal key ns)
    in
    match match_num_locs with
    | [ a; b ] -> Some (loc_to_num grid a * loc_to_num grid b)
    | _ -> None)
;;

let part1 input =
  let grid = make_grid input in
  grid
  |> find_num_locs
  |> List.filter ~f:(is_part_num grid)
  |> List.fold_left ~init:0 ~f:(fun acc l -> acc + loc_to_num grid l)
;;

let part2 input =
  let grid = make_grid input in
  let num_locs = find_num_locs grid in
  grid
  |> CoordMap.filter_map (gear_ratio grid num_locs)
  |> CoordMap.values
  |> List.of_iter
  |> Util.List.sum_int
;;
