module IntSet = Set.Make (Int)

let years = IntSet.of_list [ 2022; 2023 ]

let year_arg =
  Command.Arg_type.create (fun year_str ->
    match Int.of_string_opt year_str with
    | Some year when Set.mem years year -> year
    | _ -> failwith "Year must be [2022, 2023]")
;;

let day_arg =
  Command.Arg_type.create (fun day_str ->
    match Int.of_string_opt day_str with
    | Some day when 1 <= day && day <= 25 -> day
    | _ -> failwith "Day must be between 1 - 25")
;;

let part_arg =
  Command.Arg_type.create (fun part_str ->
    match Int.of_string_opt part_str with
    | Some part when part = 1 || part = 2 -> part
    | _ -> failwith "Part must be 1 or 2")
;;

let command_param =
  let%map_open.Command year =
    flag
      "year"
      (optional_with_default 2023 year_arg)
      ~aliases:[ "y" ]
      ~doc:"int year to run (default: 2023)"
  and day = flag "day" (required day_arg) ~aliases:[ "d" ] ~doc:"int day to run (1 - 25)"
  and part =
    flag "part" (required part_arg) ~aliases:[ "p" ] ~doc:"int part to run (1 or 2)"
  in
  fun _ -> Runner.run year day part
;;

let () = Command_unix.run @@ Command.basic command_param ~summary:"Run aoc solution"
