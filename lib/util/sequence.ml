let nats = Core.Sequence.unfold ~init:0 ~f:(fun i -> Some (i, i + 1))

let fold_result_exn s ~init ~f =
  s |> Core.Sequence.fold_result ~init ~f |> Result.error |> Option.value_exn
;;
