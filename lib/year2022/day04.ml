module A = Angstrom
open A.Let_syntax

let ( <* ) = A.(( <* ))

type range =
  { start : int
  ; stop : int
  }

let integerP =
  let%map tokens =
    A.take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  Int.of_string tokens
;;

let rangeP =
  let%map start = integerP <* A.char '-'
  and stop = integerP in
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
