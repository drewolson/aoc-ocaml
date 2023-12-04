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
    grid
    |> Map.keys
    |> List.map ~f:snd
    |> List.max_elt ~compare
    |> Option.value ~default:0
  in
  grid
  |> Map.keys
  |> List.sort ~compare:Coord.compare
  |> List.fold ~init:(None, []) ~f:(fun (curr, l) ((y, x) as pos) ->
    match Map.find_exn grid pos, curr with
    | '0' .. '9', None -> Some pos, l
    | '0' .. '9', _ -> curr, l
    | _, Some ((_, xstart) as start) ->
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

let gear_ratio grid num_locs pos =
  match Map.find grid pos with
  | Some '*' ->
    let match_num_locs =
      List.filter num_locs ~f:(fun num_loc ->
        let ns = num_loc |> neighbors in
        List.mem ns pos ~equal:Coord.equal)
    in
    (match match_num_locs with
     | [ a; b ] -> Some (loc_to_num grid a * loc_to_num grid b)
     | _ -> None)
  | _ -> None
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
  grid |> Map.keys |> List.filter_map ~f:(gear_ratio grid num_locs) |> Util.List.sum_int
;;