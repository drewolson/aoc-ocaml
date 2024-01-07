module Syntax = struct
  let ( let* ) s f = CCSeq.(s >>= f)
  let ( and* ) = CCSeq.product
  let ( let+ ) s f = CCSeq.map f s
  let ( and+ ) = CCSeq.product
  let return = CCSeq.return
end

let fold_result s ~init ~f =
  let rec aux acc s =
    let a, s' = CCSeq.uncons s |> CCOption.get_exn_or "empty" in
    match f acc a with
    | Ok acc' -> aux acc' s'
    | Error v -> v
  in
  aux init s
;;
