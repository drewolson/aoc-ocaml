module P = Util.Parser
module A = Angstrom
open P.Syntax

type range =
  { start : int
  ; stop : int
  }

let rangeP =
  let%map start = P.integerP <* A.char '-'
  and stop = P.integerP in
  { start; stop }
;;

let rangePairP =
  let%map a = rangeP <* A.char ','
  and b = rangeP in
  a, b
;;

let rangePairsP = A.sep_by (A.char '\n') rangePairP

let is_subset a b =
  (a.start <= b.start && a.stop >= b.stop) || (b.start <= a.start && b.stop >= a.stop)
;;

let is_overlapping a b =
  (a.start <= b.start && a.stop >= b.start) || (b.start <= a.start && b.stop >= a.start)
;;

let part1 input =
  input
  |> A.parse_string ~consume:Prefix rangePairsP
  |> Result.ok_or_failwith
  |> List.filter ~f:(fun (a, b) -> is_subset a b)
  |> List.length
;;

let part2 input =
  input
  |> A.parse_string ~consume:Prefix rangePairsP
  |> Result.ok_or_failwith
  |> List.filter ~f:(fun (a, b) -> is_overlapping a b)
  |> List.length
;;
