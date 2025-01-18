let run year day part =
  let path = Printf.sprintf "data/%i/day%02i.txt" year day in
  let input = In_channel.read_all path in
  match year with
  | 2019 -> Run2019.run day part input
  | 2022 -> Run2022.run day part input
  | 2023 -> Run2023.run day part input
  | year -> failwith @@ Printf.sprintf "Unknown year: %i" year
;;
