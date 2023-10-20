let take n l = Base.List.take l n
let sum_int l = Base.List.sum (module Int) ~f:Fn.id l
let sort_desc l = Base.List.sort ~compare:(fun x y -> -compare x y) l
let max_int l = Base.List.max_elt ~compare l |> Option.value ~default:Int.min_value
