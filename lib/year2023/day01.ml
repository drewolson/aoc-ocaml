module StrMap = Map.Make (String)

let words = [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
let digits = List.range 1 10 |> List.map ~f:Int.to_string
let mapping = digits |> List.zip_exn words |> StrMap.of_alist_exn

let re_f =
  words |> List.append digits |> String.concat ~sep:"|" |> Re.Perl.re |> Re.compile
;;

let re_b =
  words
  |> List.map ~f:String.rev
  |> List.append digits
  |> String.concat ~sep:"|"
  |> Re.Perl.re
  |> Re.compile
;;

let replace_word m =
  match Map.find mapping m with
  | Some i -> i
  | _ -> m
;;

let to_digit' line =
  let a = Re.Group.get (Re.exec re_f line) 0 |> replace_word in
  let b = Re.Group.get (Re.exec re_b (String.rev line)) 0 |> String.rev |> replace_word in
  Int.of_string (a ^ b)
;;

let to_digit line =
  let first_num s =
    s |> String.find ~f:Char.is_digit |> Option.value_exn |> String.of_char
  in
  Int.of_string (first_num line ^ first_num (String.rev line))
;;

let part1 input = input |> String.split_lines |> List.map ~f:to_digit |> Util.List.sum_int

let part2 input =
  input |> String.split_lines |> List.map ~f:to_digit' |> Util.List.sum_int
;;
