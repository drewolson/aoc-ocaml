let max_int l = Core.List.max_elt ~compare l |> Option.value_exn
let replicate ~n a = Core.List.init n ~f:(const a)
let sum_int l = Core.List.sum (module Int) ~f:Fn.id l
let take l ~n = Core.List.take l n
let drop l ~n = Core.List.drop l n
