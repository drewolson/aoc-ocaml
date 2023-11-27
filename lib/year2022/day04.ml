module P = Util.Parser

type range =
  { start : int
  ; stop : int
  }

let range_p =
  let%map_open.P start = P.integer <* P.char '-'
  and stop = P.integer in
  { start; stop }
;;

let range_pair_p =
  let%map_open.P a = range_p <* P.char ','
  and b = range_p in
  a, b
;;

let range_pairs_p = P.sep_by (P.char '\n') range_pair_p

let is_subset a b =
  (a.start <= b.start && a.stop >= b.stop) || (b.start <= a.start && b.stop >= a.stop)
;;

let is_overlapping a b =
  (a.start <= b.start && a.stop >= b.start) || (b.start <= a.start && b.stop >= a.start)
;;

let part1 input =
  input
  |> P.parse_exn range_pairs_p
  |> List.filter ~f:(fun (a, b) -> is_subset a b)
  |> List.length
;;

let part2 input =
  input
  |> P.parse_exn range_pairs_p
  |> List.filter ~f:(fun (a, b) -> is_overlapping a b)
  |> List.length
;;
