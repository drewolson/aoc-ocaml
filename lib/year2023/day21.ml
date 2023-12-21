module Coord = struct
  type t = int * int [@@deriving sexp, compare, equal, hash]
end

module CoordMap = Map.Make (Coord)
module CoordSet = Set.Make (Coord)

module Key = struct
  type t = Coord.t list [@@deriving sexp, compare, hash]
end

let parse input =
  input
  |> String.split_lines
  |> List.concat_mapi ~f:(fun y l ->
    l |> String.to_list |> List.mapi ~f:(fun x c -> (x, y), c))
  |> CoordMap.of_alist_exn
;;

let take_steps n grid =
  let is_garden = function
    | '.' | 'S' -> true
    | _ -> false
  in
  let neighbors (x, y) =
    let candidates = [ x + 1, y; x - 1, y; x, y + 1; x, y - 1 ] in
    List.filter_map candidates ~f:(fun c ->
      c |> Map.find grid |> Option.bind ~f:(fun v -> if is_garden v then Some c else None))
  in
  let rec aux s set =
    if s = n
    then set
    else
      set |> Set.to_list |> List.concat_map ~f:neighbors |> CoordSet.of_list |> aux (s + 1)
  in
  let start =
    grid
    |> Map.to_alist
    |> List.find_map_exn ~f:(fun (k, v) -> if Char.equal 'S' v then Some k else None)
  in
  aux 0 (CoordSet.singleton start)
;;

let cache = Hashtbl.create (module Key)

let take_steps' n (grid : char CoordMap.t) =
  let is_garden = function
    | '.' | 'S' -> true
    | _ -> false
  in
  let max_x = grid |> Map.keys |> List.map ~f:fst |> Util.List.max_int in
  let max_y = grid |> Map.keys |> List.map ~f:snd |> Util.List.max_int in
  let clamp tcoords = function
    | x, y when x > max_x -> (0, y), CoordSet.map tcoords ~f:(fun (x, y) -> x + 1, y)
    | x, y when x < 0 -> (max_x, y), CoordSet.map tcoords ~f:(fun (x, y) -> x - 1, y)
    | x, y when y > max_y -> (x, 0), CoordSet.map tcoords ~f:(fun (x, y) -> x, y + 1)
    | x, y when y < 0 -> (x, max_y), CoordSet.map tcoords ~f:(fun (x, y) -> x, y - 1)
    | c -> c, tcoords
  in
  let neighbors (((x, y), tcoords) : Coord.t * CoordSet.t) =
    let candidates =
      [ x + 1, y; x - 1, y; x, y + 1; x, y - 1 ] |> List.map ~f:(clamp tcoords)
    in
    List.filter_map candidates ~f:(fun (c, tcoords) ->
      c
      |> Map.find grid
      |> Option.bind ~f:(fun v -> if is_garden v then Some (c, tcoords) else None))
  in
  let rec aux s (mset : CoordSet.t CoordMap.t) =
    Hashtbl.update cache (Map.keys mset) ~f:(function
      | None -> [ s, mset |> Map.data |> List.sum (module Int) ~f:Set.length ]
      | Some v -> (s, mset |> Map.data |> List.sum (module Int) ~f:Set.length) :: v);
    if s = n
    then mset
    else
      mset
      |> Map.to_alist
      |> List.concat_map ~f:neighbors
      |> List.fold ~init:CoordMap.empty ~f:(fun acc (coord, tcoords) ->
        Map.update acc coord ~f:(function
          | None -> tcoords
          | Some set -> Set.union tcoords set))
      |> aux (s + 1)
  in
  let start : Coord.t =
    grid
    |> Map.to_alist
    |> List.find_map_exn ~f:(fun (k, v) -> if Char.equal 'S' v then Some k else None)
  in
  aux 0 (CoordMap.singleton start (CoordSet.singleton (0, 0)))
;;

let part1 steps input = input |> parse |> take_steps steps |> Set.length

let part2 steps input =
  input |> parse |> take_steps' steps |> Map.data |> List.sum (module Int) ~f:Set.length
;;
(* let counts = *)
(*   cache *)
(*   |> Hashtbl.data *)
(*   |> List.max_elt ~compare:(fun a b -> compare (List.length a) (List.length b)) *)
(*   |> Option.value_exn *)
(*   |> List.rev *)
(* in *)
(* Util.List.zip' counts (List.drop counts 1) *)
(* |> List.iter ~f:(fun ((s, c), (s', c')) -> *)
(*   Stdio.printf "step: %d, curr %d, next %d, diff %d\n" s c c' (c' - c)); *)
