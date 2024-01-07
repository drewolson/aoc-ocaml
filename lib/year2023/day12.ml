module P = Util.Parser
open P.Syntax

let is_token = function
  | '.' | '#' | '?' -> true
  | _ -> false
;;

let line_p =
  let+ tokens = P.take_while is_token <* P.char ' '
  and+ counts = P.sep_by1 (P.char ',') P.integer in
  String.to_list tokens, counts
;;

let input_p = P.sep_by1 P.end_of_line line_p

let arrangements line =
  let cache = Hashtbl.create 1000 in
  let rec aux tokens counts count =
    Hashtbl.get_or_add cache ~k:(tokens, counts, count) ~f:(fun _ ->
      match tokens, counts, count with
      | [], [], 0 -> 1
      | [], [ c ], count when c = count -> 1
      | '?' :: ts, cs, count -> aux ('.' :: ts) cs count + aux ('#' :: ts) cs count
      | '.' :: ts, cs, count when count = 0 -> aux ts cs count
      | '.' :: ts, c :: cs, count when (not (count = 0)) && count = c -> aux ts cs 0
      | '#' :: ts, cs, count -> aux ts cs (count + 1)
      | _ -> 0)
  in
  aux (fst line) (snd line) 0
;;

let expand (tokens, counts) =
  ( tokens @ [ '?' ] @ tokens @ [ '?' ] @ tokens @ [ '?' ] @ tokens @ [ '?' ] @ tokens
  , counts @ counts @ counts @ counts @ counts )
;;

let part1 input =
  input
  |> P.parse_exn input_p
  |> List.fold_left ~init:0 ~f:(fun acc l -> acc + arrangements l)
;;

let part2 input =
  input
  |> P.parse_exn input_p
  |> List.map ~f:expand
  |> List.fold_left ~init:0 ~f:(fun acc l -> acc + arrangements l)
;;
