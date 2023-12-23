module P = Util.Parser
open P.Syntax

type coord =
  { x : int
  ; y : int
  ; z : int
  }
[@@deriving sexp, equal, compare]

module Brick = struct
  type t = int * coord * coord [@@deriving sexp, equal, compare]
end

module BrickSet = Set.Make (Brick)

let coord_p =
  let%map x = P.integer <* P.char ','
  and y = P.integer <* P.char ','
  and z = P.integer in
  { x; y; z }
;;

let brick_p =
  let%map a = coord_p <* P.char '~'
  and b = coord_p in
  a, b
;;

let bricks_p =
  let%map bricks = P.sep_by1 P.end_of_line brick_p in
  bricks |> List.mapi ~f:(fun i (a, b) -> i, a, b) |> BrickSet.of_list
;;

let is_overlap (a1, a2) (b1, b2) =
  let a = min a1 a2, max a1 a2 in
  let b = min b1 b2, max b1 b2 in
  fst a <= snd b && snd a >= fst b
;;

let will_collide (_, a1, a2) (_, b1, b2) =
  is_overlap (a1.x, a2.x) (b1.x, b2.x) && is_overlap (a1.y, a2.y) (b1.y, b2.y)
;;

let first_collision_z settled brick =
  settled
  |> Set.to_list
  |> List.filter ~f:(will_collide brick)
  |> List.map ~f:(fun (_, a, b) -> max a.z b.z)
  |> List.max_elt ~compare
  |> Option.value ~default:0
;;

let fall bricks =
  bricks
  |> Set.to_list
  |> List.sort ~compare:(fun (_, a1, a2) (_, b1, b2) ->
    compare (min a1.z a2.z) (min b1.z b2.z))
  |> List.fold ~init:BrickSet.empty ~f:(fun settled (i, a, b) ->
    let z' = first_collision_z settled (i, a, b) in
    let dz = min a.z b.z - z' in
    let brick = i, { a with z = a.z - dz + 1 }, { b with z = b.z - dz + 1 } in
    Set.add settled brick)
;;

let count_free bricks =
  bricks
  |> Set.filter ~f:(fun brick ->
    let bricks' = Set.remove bricks brick in
    Set.equal bricks' (fall bricks'))
  |> Set.length
;;

let count_drops bricks =
  bricks
  |> Set.to_list
  |> List.sum
       (module Int)
       ~f:(fun brick ->
         let bricks' = Set.remove bricks brick in
         Set.diff (fall bricks') bricks' |> Set.length)
;;

let part1 input = input |> P.parse_exn bricks_p |> fall |> count_free
let part2 input = input |> P.parse_exn bricks_p |> fall |> count_drops
