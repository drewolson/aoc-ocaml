module Coord = struct
  type t = int * int [@@deriving eq, ord]
end

module CoordMap = Map.Make (Coord)
module CoordSet = Set.Make (Coord)

let parse input =
  input
  |> String.lines
  |> List.mapi ~f:(fun y l -> l |> String.to_list |> List.mapi ~f:(fun x c -> (x, y), c))
  |> List.concat
  |> CoordMap.of_list
;;

let take_steps n grid =
  let start =
    grid
    |> CoordMap.to_list
    |> List.find_map ~f:(fun (k, v) -> if Char.equal 'S' v then Some k else None)
    |> Option.get_exn_or "none"
  in
  let is_garden = function
    | '.' | 'S' -> true
    | _ -> false
  in
  let neighbors (x, y) =
    let candidates = [ x + 1, y; x - 1, y; x, y + 1; x, y - 1 ] in
    List.filter_map candidates ~f:(fun c ->
      grid
      |> CoordMap.find_opt c
      |> (Fun.flip Option.bind) (fun v -> if is_garden v then Some c else None))
  in
  let rec aux s set =
    if s = n
    then set
    else
      set
      |> CoordSet.to_list
      |> List.concat_map ~f:neighbors
      |> CoordSet.of_list
      |> aux (s + 1)
  in
  aux 0 (CoordSet.singleton start)
;;

(* all credit here goes to https://github.com/villuna/aoc23/wiki/A-Geometric-solution-to-advent-of-code-2023,-day-21 *)
let take_steps' grid =
  let odd_set = take_steps 131 grid in
  let even_set = take_steps 132 grid in
  let odd_corner_set = CoordSet.diff odd_set (take_steps 65 grid) in
  let even_corner_set = CoordSet.diff even_set (take_steps 64 grid) in
  let odd_full = CoordSet.cardinal odd_set in
  let even_full = CoordSet.cardinal even_set in
  let odd_corner = CoordSet.cardinal odd_corner_set in
  let even_corner = CoordSet.cardinal even_corner_set in
  let n = (26501365 - (131 / 2)) / 131 in
  (Int.pow (n + 1) 2 * odd_full)
  + (Int.pow n 2 * even_full)
  - ((n + 1) * odd_corner)
  + (n * even_corner)
;;

let part1 steps input = input |> parse |> take_steps steps |> CoordSet.cardinal
let part2 input = input |> parse |> take_steps'
