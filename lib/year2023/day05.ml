module P = Util.Parser
open P.Syntax

type mapping =
  { source : int
  ; dest : int
  ; length : int
  }

type map =
  { name : string
  ; mappings : mapping list
  }

type almanac =
  { seeds : int list
  ; maps : map list
  }

let seeds_p = P.string "seeds: " *> P.sep_by1 (P.char ' ') P.integer

let mapping_p =
  let+ dest = P.integer <* P.char ' '
  and+ source = P.integer <* P.char ' '
  and+ length = P.integer in
  { source; dest; length }
;;

let name_p =
  P.take_while1 (function
    | '-' -> true
    | c -> Util.Char.is_lowercase c)
;;

let map_p =
  let+ name = name_p <* P.string " map:" <* P.end_of_line
  and+ mappings = P.sep_by1 P.end_of_line mapping_p in
  { name; mappings }
;;

let maps_p = P.sep_by1 (P.count 2 P.end_of_line) map_p

let almanac_p =
  let+ seeds = seeds_p <* P.count 2 P.end_of_line
  and+ maps = maps_p in
  { seeds; maps }
;;

let run_map ~f ~t ~m maps seed =
  let run_mappings value mappings =
    let mapping =
      mappings
      |> List.sort ~cmp:(fun a b -> -compare (f a) (f b))
      |> List.drop_while ~f:(fun m -> f m > value)
      |> List.head_opt
    in
    match mapping with
    | Some m when value <= f m + m.length -> t m + abs (value - f m)
    | _ -> value
  in
  maps
  |> m
  |> List.map ~f:(fun map -> map.mappings)
  |> List.fold_left ~init:seed ~f:run_mappings
;;

let solve almanac =
  almanac.seeds
  |> List.map
       ~f:(run_map ~f:(fun s -> s.source) ~t:(fun s -> s.dest) ~m:Fun.id almanac.maps)
  |> List.reduce_exn ~f:min
;;

let solve' almanac =
  Seq.ints 0
  |> Seq.filter (fun loc ->
    let result =
      run_map ~f:(fun s -> s.dest) ~t:(fun s -> s.source) ~m:List.rev almanac.maps loc
    in
    almanac.seeds
    |> List.chunks 2
    |> List.exists ~f:(function
      | [ a; b ] -> a <= result && result <= a + b
      | _ -> false))
  |> Seq.head_exn
;;

let part1 input = input |> P.parse_exn almanac_p |> solve
let part2 input = input |> P.parse_exn almanac_p |> solve'
