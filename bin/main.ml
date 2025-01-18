open Cmdliner

let year =
  let doc = "Year to run" in
  let years = [ 2019; 2022; 2023 ] |> List.map ~f:(fun y -> Int.to_string y, y) in
  Arg.(value & opt (enum years) 2023 & info [ "y"; "year" ] ~docv:"YEAR" ~doc)
;;

let day =
  let doc = "Day to run (1 - 25)" in
  let days = List.range 1 26 |> List.map ~f:(fun d -> Int.to_string d, d) in
  Arg.(required & opt (some & enum days) None & info [ "d"; "day" ] ~docv:"DAY" ~doc)
;;

let part =
  let doc = "Part to run (1 or 2)" in
  let parts = [ "1", 1; "2", 2 ] in
  Arg.(required & opt (some & enum parts) None & info [ "p"; "part" ] ~docv:"PART" ~doc)
;;

let aoc_t = Term.(const Runner.run $ year $ day $ part)

let cmd =
  let doc = "Run aoc solution" in
  let info = Cmd.info "aoc" ~version:"1.0.0" ~doc in
  Cmd.v info aoc_t
;;

let () = exit (Cmd.eval cmd)
