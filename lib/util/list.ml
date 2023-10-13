let maximum a l = Stdlib.List.fold_left max a l
let reverse l = Stdlib.List.fold_left (fun l x -> x :: l) [] l
let sort_asc l = Stdlib.List.sort compare l
let sort_desc l = Stdlib.List.sort (fun x y -> -compare x y) l
let sum l = Stdlib.List.fold_left ( + ) 0 l

let take n l =
  let rec take' acc n l =
    match n, l with
    | 0, _ -> acc
    | _, [] -> acc
    | n, h :: t -> take' (h :: acc) (n - 1) t
  in
  reverse (take' [] n l)
;;
