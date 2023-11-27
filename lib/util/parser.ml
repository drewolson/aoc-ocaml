include Angstrom

module Syntax = struct
  include Angstrom.Let_syntax

  let ( >>= ), ( >>| ), ( *> ), ( <* ), ( <|> ), ( <$> ) =
    ( >>= ), ( >>| ), ( *> ), ( <* ), ( <|> ), ( <$> )
  ;;

  let ( $> ) p a = p >>| const a
  let ( <$ ) a p = p >>| const a
end

let integer =
  let open Syntax in
  let%map tokens =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  Int.of_string tokens
;;

let signed_integer =
  let open Syntax in
  let%map sign = 1 <$ char '+' <|> (-1 <$ char '-') <|> return 1
  and n = integer in
  sign * n
;;

let parse_exn parser input =
  input |> parse_string ~consume:Prefix parser |> Result.ok_or_failwith
;;

let parse_all_exn parser input =
  input |> parse_string ~consume:All parser |> Result.ok_or_failwith
;;
