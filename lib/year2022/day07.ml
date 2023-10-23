module P = Util.Parser
module A = Angstrom
open A.Let_syntax

let ( <$ ) = Util.Parser.(( <$ ))
let ( *> ), ( <* ) = A.(( *> ), ( <* ))

type cmd =
  | Cd of string
  | Ls
  | DirItem of string
  | FileItem of int * string

type fs =
  | File of string * int
  | Dir of string * fs list

let nameP =
  A.take_while (function
    | 'a' .. 'z' | '.' | '/' -> true
    | _ -> false)
;;

let cdP =
  let%map dir = A.string "$ cd " *> nameP in
  Cd dir
;;

let lsP = Ls <$ A.string "$ ls"

let dirItemP =
  let%map dir = A.string "dir " *> nameP in
  DirItem dir
;;

let fileItemP =
  let%map size = P.integerP <* A.char ' '
  and file = nameP in
  FileItem (size, file)
;;

let cmdP = A.choice [ cdP; lsP; dirItemP; fileItemP ]
let cmdsP = A.sep_by1 A.end_of_line cmdP

let build_fs cmds =
  let rec build_nodes nodes = function
    | (DirItem _ | Ls) :: cmds -> build_nodes nodes cmds
    | FileItem (size, name) :: cmds -> build_nodes (File (name, size) :: nodes) cmds
    | Cd ".." :: cmds -> nodes, cmds
    | Cd name :: cmds ->
      let node, cmds' = build_dir name cmds in
      build_nodes (node :: nodes) cmds'
    | [] -> nodes, []
  and build_dir name cmds =
    let nodes, cmds' = build_nodes [] cmds in
    Dir (name, nodes), cmds'
  in
  build_dir "/" (List.drop cmds 2) |> fst
;;

let sizes fs =
  let rec sizes' = function
    | Dir (_, fs) ->
      let n, ns =
        List.fold fs ~init:(0, []) ~f:(fun (n, ns) f ->
          let n', ns' = sizes' f in
          n + n', ns @ ns')
      in
      n, n :: ns
    | File (_, size) -> size, []
  in
  fs |> sizes' |> snd
;;

let part1 input =
  input
  |> A.parse_string ~consume:Prefix cmdsP
  |> Result.ok_or_failwith
  |> build_fs
  |> sizes
  |> List.filter ~f:(fun x -> x <= 100000)
  |> Util.List.sum_int
;;

let part2 input =
  let ns =
    input
    |> A.parse_string ~consume:Prefix cmdsP
    |> Result.ok_or_failwith
    |> build_fs
    |> sizes
  in
  let goal = 30000000 - (70000000 - List.hd_exn ns) in
  ns |> List.tl_exn |> List.sort ~compare |> List.find_exn ~f:(fun n -> n >= goal)
;;
