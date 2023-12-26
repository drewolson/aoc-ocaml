let input =
  {|jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr
|}
;;

let%expect_test "2023 day25 part1" =
  Printf.printf "%i" @@ Year2023.Day25.part1 input;
  [%expect {| 54 |}]
;;
