let run year day part bench =
  let path = Printf.sprintf "data/%i/day%02i.txt" year day in
  let input = In_channel.read_all path in
  let maybe_bench ~f =
    let open Core_bench in
    let name = Printf.sprintf "Year %i, Day %i, Part %i" year day part in
    if bench then Bench.bench [ Bench.Test.create ~name f ] else f ()
  in
  maybe_bench ~f:(fun _ ->
    match year with
    | 2019 -> Run2019.run day part input bench
    | 2022 -> Run2022.run day part input bench
    | 2023 -> Run2023.run day part input bench
    | year -> failwith @@ Printf.sprintf "Unknown year: %i" year)
;;
