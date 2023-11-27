module P = Util.Parser

type cmd =
  | Cd of string
  | Ls
  | DirItem of string
  | FileItem of int * string

type fs =
  | File of string * int
  | Dir of string * fs list

let name_p =
  P.take_while (function
    | 'a' .. 'z' | '.' | '/' -> true
    | _ -> false)
;;

let cd_p =
  let%map_open.P dir = P.string "$ cd " *> name_p in
  Cd dir
;;

let ls_p =
  let open P.Ops in
  Ls <$ P.string "$ ls"
;;

let dir_item_p =
  let%map_open.P dir = P.string "dir " *> name_p in
  DirItem dir
;;

let file_item_p =
  let%map_open.P size = P.integer <* P.char ' '
  and file = name_p in
  FileItem (size, file)
;;

let cmd_p = P.choice [ cd_p; ls_p; dir_item_p; file_item_p ]
let cmds_p = P.sep_by1 P.end_of_line cmd_p

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
  |> P.parse_exn cmds_p
  |> build_fs
  |> sizes
  |> List.filter ~f:(fun x -> x <= 100000)
  |> Util.List.sum_int
;;

let part2 input =
  let ns = input |> P.parse_exn cmds_p |> build_fs |> sizes in
  let goal = 30000000 - (70000000 - List.hd_exn ns) in
  ns |> List.tl_exn |> List.sort ~compare |> List.find_exn ~f:(fun n -> n >= goal)
;;
