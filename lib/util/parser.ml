module A = Angstrom
open A.Let_syntax

let ( $> ) p a = p >>| const a
let ( <$ ) a p = p >>| const a

let integerP =
  let%map tokens =
    A.take_while1 (function
      | '0' .. '9' -> true
      | _ -> false)
  in
  Int.of_string tokens
;;
