open Angstrom
open Angstrom.Let_syntax

type range =
  { start : int
  ; stop : int
  }

let integerP =
  let%map tokens =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  Int.of_string tokens
;;

let rangeP =
  let%map start = integerP <* char '-'
  and stop = integerP in
  { start; stop }
;;

let rangePairP =
  let%map a = rangeP <* char ','
  and b = rangeP in
  a, b
;;

let rangePairsP = sep_by (char '\n') rangePairP

let is_subset a b =
  (a.start <= b.start && a.stop >= b.stop) || (b.start <= a.start && b.stop >= a.stop)
;;

let is_overlapping a b =
  (a.start <= b.start && a.stop >= b.start) || (b.start <= a.start && b.stop >= a.start)
;;

let part1 input =
  parse_string ~consume:Prefix rangePairsP input
  |> Result.ok_or_failwith
  |> List.filter ~f:(fun (a, b) -> is_subset a b)
  |> List.length
;;

let part2 input =
  parse_string ~consume:Prefix rangePairsP input
  |> Result.ok_or_failwith
  |> List.filter ~f:(fun (a, b) -> is_overlapping a b)
  |> List.length
;;
