include Angstrom

module Ops = struct
  let ( >>| ), ( *> ), ( <* ), ( <|> ), ( <$> ), ( >>= ) =
    ( >>| ), ( *> ), ( <* ), ( <|> ), ( <$> ), ( >>= )
  ;;

  let ( $> ) p a = p >>| const a
  let ( <$ ) a p = p >>| const a
end

module Let_syntax = struct
  module Let_syntax = struct
    include Angstrom.Let_syntax.Let_syntax

    module Open_on_rhs = struct
      include Ops

      let return = return
    end
  end
end

let integer =
  let open Let_syntax in
  let%map_open tokens =
    take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  Int.of_string tokens
;;

let signed_integer =
  let open Let_syntax in
  let%map_open sign = 1 <$ char '+' <|> (-1 <$ char '-') <|> return 1
  and n = integer in
  sign * n
;;

let parse_exn parser input =
  input |> parse_string ~consume:Prefix parser |> Result.ok_or_failwith
;;

let parse_all_exn parser input =
  input |> parse_string ~consume:All parser |> Result.ok_or_failwith
;;
