module Syntax = struct
  let ( let* ) t f = Core.List.bind t ~f
  let ( and* ) = Core.List.Let_syntax.Let_syntax.both
  let ( let+ ) t f = Core.List.map t ~f
  let ( and+ ) = Core.List.Let_syntax.Let_syntax.both
  let return = Core.List.return
end

let max_int l = Core.List.max_elt ~compare l |> Option.value_exn
let min_int l = Core.List.min_elt ~compare l |> Option.value_exn
let replicate a ~n = Core.List.init n ~f:(const a)
let sum_int l = Core.List.sum (module Int) ~f:Core.Fn.id l
let take l ~n = Core.List.take l n
let drop l ~n = Core.List.drop l n

let rec zip' l r =
  match l, r with
  | [], _ -> []
  | _, [] -> []
  | a :: ar, b :: br -> (a, b) :: zip' ar br
;;

let rec pairs = function
  | a :: rest -> Core.List.map rest ~f:(fun b -> a, b) @ pairs rest
  | [] -> []
;;
