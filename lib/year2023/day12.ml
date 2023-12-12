module P = Util.Parser
open P.Syntax

module Key = struct
  type t = char list * int list * int [@@deriving compare, sexp, hash]
end

let is_token = function
  | '.' | '#' | '?' -> true
  | _ -> false
;;

let line_p =
  let%map tokens = P.take_while is_token <* P.char ' '
  and counts = P.sep_by1 (P.char ',') P.integer in
  String.to_list tokens, counts
;;

let arrangements input =
  let cache = Hashtbl.create (module Key) in
  let rec aux input counts count =
    Hashtbl.find_or_add cache (input, counts, count) ~default:(fun _ ->
      match input, counts, count with
      | [], [], 0 -> 1
      | [], [ c ], count when c = count -> 1
      | '?' :: r, counts, count ->
        aux ('.' :: r) counts count + aux ('#' :: r) counts count
      | '.' :: inputr, counts, count when count = 0 -> aux inputr counts count
      | '.' :: inputr, c :: countsr, count when (not (count = 0)) && count = c ->
        aux inputr countsr 0
      | '#' :: inputr, counts, count -> aux inputr counts (count + 1)
      | _ -> 0)
  in
  aux (fst input) (snd input) 0
;;

let expand (tokens, counts) =
  ( tokens @ [ '?' ] @ tokens @ [ '?' ] @ tokens @ [ '?' ] @ tokens @ [ '?' ] @ tokens
  , counts @ counts @ counts @ counts @ counts )
;;

let input_p = P.sep_by1 P.end_of_line line_p
let part1 input = input |> P.parse_exn input_p |> List.sum (module Int) ~f:arrangements

let part2 input =
  input
  |> P.parse_exn input_p
  |> List.map ~f:expand
  |> List.sum (module Int) ~f:arrangements
;;
