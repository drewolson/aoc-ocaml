module Coord = struct
  type t = int * int [@@deriving sexp, equal, compare]
end

module CoordMap = Map.Make (Coord)

let make_grid input =
  input
  |> String.split_lines
  |> List.concat_mapi ~f:(fun y line ->
    line |> String.to_list |> List.mapi ~f:(fun x c -> (y, x), c))
  |> CoordMap.of_alist_exn
;;

let find_num_locs grid =
  let max_x =
    grid |> Map.keys |> List.map ~f:snd |> List.max_elt ~compare |> Option.value_exn
  in
  grid
  |> Map.fold ~init:(None, []) ~f:(fun ~key ~data (curr, l) ->
    match data, curr with
    | '0' .. '9', None -> Some key, l
    | '0' .. '9', _ -> curr, l
    | _, Some ((_, xstart) as start) ->
      let x = snd key in
      if x = 0
      then None, (start, max_x + 1 - xstart) :: l
      else None, (start, x - xstart) :: l
    | _, _ -> curr, l)
  |> snd
;;

let neighbors ((y, x), len) =
  List.range x (x + len)
  |> List.concat_map ~f:(fun x' ->
    let left = [ y - 1, x' - 1; y, x' - 1; y + 1, x' - 1 ] in
    let middle = [ y - 1, x'; y + 1, x' ] in
    let right = [ y - 1, x' + 1; y, x' + 1; y + 1, x' + 1 ] in
    if len = 1
    then left @ middle @ right
    else if x' = x
    then left @ middle
    else if x' = x + len - 1
    then middle @ right
    else middle)
;;

let is_part_num grid num_loc =
  num_loc
  |> neighbors
  |> List.filter_map ~f:(Map.find grid)
  |> List.exists ~f:(function
    | '0' .. '9' -> false
    | '.' -> false
    | _ -> true)
;;

let loc_to_num grid ((y, x), len) =
  List.range x (x + len)
  |> List.filter_map ~f:(fun x' -> Map.find grid (y, x'))
  |> String.of_list
  |> Int.of_string
;;

let gear_ratio grid num_locs ~key ~data =
  if not @@ Char.equal data '*'
  then None
  else (
    let match_num_locs =
      List.filter num_locs ~f:(fun num_loc ->
        let ns = neighbors num_loc in
        List.mem ns key ~equal:Coord.equal)
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
  |> List.sum (module Int) ~f:(loc_to_num grid)
;;

let part2 input =
  let grid = make_grid input in
  let num_locs = find_num_locs grid in
  grid |> Map.filter_mapi ~f:(gear_ratio grid num_locs) |> Map.data |> Util.List.sum_int
;;
