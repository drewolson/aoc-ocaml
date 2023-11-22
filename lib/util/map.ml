let alter m ~key ~f =
  Core.Map.change m key ~f:(function
    | None -> None
    | Some v -> Some (f v))
;;

let change m ~key ~f = Core.Map.change m key ~f
let update m ~key ~f = Core.Map.update m key ~f
