module Syntax = struct
  let ( let* ) t f = Core.Sequence.bind t ~f
  let ( and* ) = Core.Sequence.Let_syntax.Let_syntax.both
  let ( let+ ) t f = Core.Sequence.map t ~f
  let ( and+ ) = Core.Sequence.Let_syntax.Let_syntax.both
  let return = Core.Sequence.Let_syntax.Let_syntax.return
end

let nats = Core.Sequence.unfold ~init:0 ~f:(fun i -> Some (i, i + 1))

let fold_result_exn s ~init ~f =
  s |> Core.Sequence.fold_result ~init ~f |> Result.error |> Option.value_exn
;;
