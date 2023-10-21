module Arg = Cmdliner.Arg
module Cmd = Cmdliner.Cmd
module Term = Cmdliner.Term

let ( & ), ( $ ) = Arg.(( & )), Cmdliner.Term.(( $ ))

let year =
  let doc = "Year to run" in
  Arg.value & Arg.opt Arg.int 2022 & Arg.info [ "y"; "year" ] ~docv:"YEAR" ~doc
;;

let day =
  let doc = "Day to run (1 - 25)" in
  let days = List.map ~f:(fun d -> Int.to_string d, d) (List.init 25 ~f:Int.succ) in
  Arg.required
  & Arg.opt (Arg.some & Arg.enum days) None
  & Arg.info [ "d"; "day" ] ~docv:"DAY" ~doc
;;

let part =
  let doc = "Part to run (1 or 2)" in
  let parts = [ "1", 1; "2", 2 ] in
  Arg.required
  & Arg.opt (Arg.some & Arg.enum parts) None
  & Arg.info [ "p"; "part" ] ~docv:"PART" ~doc
;;

let aoc_t = Term.const Runner.run $ year $ day $ part

let cmd =
  let doc = "Run aoc solution" in
  let info = Cmd.info "aoc" ~version:"1.0.0" ~doc in
  Cmd.v info aoc_t
;;

let () = Stdlib.exit (Cmd.eval cmd)
