let input =
  {|$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
|}
;;

let%expect_test "2022 day7 part1" =
  let result = Year2022.Day07.part1 input in
  Printf.printf "%i" result;
  [%expect {| 95437 |}]
;;

let%expect_test "2022 day7 part2" =
  let result = Year2022.Day07.part2 input in
  Printf.printf "%i" result;
  [%expect {| 24933642 |}]
;;
