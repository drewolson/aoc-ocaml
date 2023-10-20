let year =
  let open Cmdliner.Arg in
  let doc = "Year to run" in
  value & opt int 2022 & info [ "y"; "year" ] ~docv:"YEAR" ~doc
;;

let day =
  let open Cmdliner.Arg in
  let doc = "Day to run (1 - 25)" in
  let days = List.map ~f:(fun d -> Int.to_string d, d) (List.init 25 ~f:Int.succ) in
  required & opt (some & enum days) None & info [ "d"; "day" ] ~docv:"DAY" ~doc
;;

let part =
  let open Cmdliner.Arg in
  let doc = "Part to run (1 or 2)" in
  let parts = [ "1", 1; "2", 2 ] in
  required & opt (some & enum parts) None & info [ "p"; "part" ] ~docv:"PART" ~doc
;;

let aoc_t =
  let open Cmdliner.Term in
  const Runner.run $ year $ day $ part
;;

let cmd =
  let open Cmdliner.Cmd in
  let doc = "Run aoc solution" in
  let info = info "aoc" ~version:"1.0.0" ~doc in
  v info aoc_t
;;

let () = Stdlib.exit (Cmdliner.Cmd.eval cmd)
