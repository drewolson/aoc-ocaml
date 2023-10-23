let max_int l = Core.List.max_elt ~compare l |> Option.value_exn
let sum_int l = Core.List.sum (module Int) ~f:Fn.id l
let take n l = Core.List.take l n
