module P = Util.Parser
open P.Syntax
module StrMap = Map.Make (String)

type pulse =
  | High
  | Low
[@@deriving equal]

type state =
  | On
  | Off

type outputs = string list

type module' =
  | FlipFlop of
      { name : string
      ; state : state
      ; outputs : outputs
      }
  | Conjunction of
      { name : string
      ; last_inputs : pulse StrMap.t
      ; outputs : outputs
      }
  | Broadcaster of
      { name : string
      ; outputs : string list
      }

let mod_name = function
  | FlipFlop { name } -> name
  | Conjunction { name } -> name
  | Broadcaster { name } -> name
;;

let mod_outputs = function
  | FlipFlop { outputs } -> outputs
  | Conjunction { outputs } -> outputs
  | Broadcaster { outputs } -> outputs
;;

let name_p =
  P.take_while (function
    | 'a' .. 'z' -> true
    | _ -> false)
;;

let outputs_p = P.sep_by1 (P.string ", ") name_p

let broadcaster_p =
  let%map outputs = P.string "broadcaster -> " *> outputs_p in
  Broadcaster { name = "broadcaster"; outputs }
;;

let flip_flop_p =
  let%map name = P.char '%' *> name_p <* P.string " -> "
  and outputs = outputs_p in
  FlipFlop { name; state = Off; outputs }
;;

let conjunction_p =
  let%map name = P.char '&' *> name_p <* P.string " -> "
  and outputs = outputs_p in
  Conjunction { name; last_inputs = StrMap.empty; outputs }
;;

let module_p = P.choice [ flip_flop_p; conjunction_p; broadcaster_p ]

let modules_p =
  let%map modules = P.sep_by1 P.end_of_line module_p in
  modules |> List.map ~f:(fun m -> mod_name m, m) |> StrMap.of_alist_exn
;;

let initialize_inputs name mods =
  mods
  |> Map.filter ~f:(fun m -> List.mem (mod_outputs m) ~equal:String.equal name)
  |> Map.keys
  |> List.map ~f:(fun k -> k, Low)
  |> StrMap.of_alist_exn
;;

let parse input =
  let mods = P.parse_exn modules_p input in
  Map.map mods ~f:(function
    | Conjunction fields ->
      Conjunction { fields with last_inputs = initialize_inputs fields.name mods }
    | m -> m)
;;

let solve mods =
  let update_counts (low, high) n = function
    | Low -> low + n, high
    | High -> low, high + n
  in
  let rec send_pulse mods counts = function
    | [] -> mods, counts
    | (name, pulse, from) :: t ->
      (match Map.find mods name with
       | None -> send_pulse mods counts t
       | Some (Broadcaster { outputs; name }) ->
         let pulses = List.map outputs ~f:(fun n -> n, pulse, name) in
         let n = List.length pulses in
         send_pulse mods (update_counts counts n pulse) (t @ pulses)
       | Some (FlipFlop { state; outputs; name }) ->
         (match pulse with
          | High -> send_pulse mods counts t
          | Low ->
            let state', pulse' =
              match state with
              | Off -> On, High
              | On -> Off, Low
            in
            let mods' =
              Util.Map.alter mods ~key:name ~f:(function
                | FlipFlop f -> FlipFlop { f with state = state' }
                | m -> m)
            in
            let pulses = List.map outputs ~f:(fun n -> n, pulse', name) in
            let n = List.length pulses in
            send_pulse mods' (update_counts counts n pulse') (t @ pulses))
       | Some (Conjunction { last_inputs; outputs; name }) ->
         let last_inputs' = Map.set last_inputs ~key:from ~data:pulse in
         let mods' =
           Util.Map.alter mods ~key:name ~f:(function
             | Conjunction f -> Conjunction { f with last_inputs = last_inputs' }
             | m -> m)
         in
         let pulse' =
           if Map.for_all last_inputs' ~f:(equal_pulse High) then Low else High
         in
         let pulses = List.map outputs ~f:(fun n -> n, pulse', name) in
         let n = List.length pulses in
         send_pulse mods' (update_counts counts n pulse') (t @ pulses))
  in
  let results =
    List.folding_map (List.range 0 1000) ~init:mods ~f:(fun mods _ ->
      let mods', (low, high) = send_pulse mods (1, 0) [ "broadcaster", Low, "button" ] in
      mods', (low, high))
  in
  let lows = List.sum (module Int) results ~f:fst in
  let highs = List.sum (module Int) results ~f:snd in
  lows * highs
;;

let solve' mods =
  let rec send_pulse mods n = function
    | [] -> mods, n
    | (name, pulse, from) :: t ->
      let n = if String.equal "ll" name && equal_pulse High pulse then Some from else n in
      (match Map.find mods name with
       | None -> send_pulse mods n t
       | Some (Broadcaster { outputs; name }) ->
         let pulses = List.map outputs ~f:(fun n -> n, pulse, name) in
         send_pulse mods n (t @ pulses)
       | Some (FlipFlop { state; outputs; name }) ->
         (match pulse with
          | High -> send_pulse mods n t
          | Low ->
            let state', pulse' =
              match state with
              | Off -> On, High
              | On -> Off, Low
            in
            let mods' =
              Util.Map.alter mods ~key:name ~f:(function
                | FlipFlop f -> FlipFlop { f with state = state' }
                | m -> m)
            in
            let pulses = List.map outputs ~f:(fun n -> n, pulse', name) in
            send_pulse mods' n (t @ pulses))
       | Some (Conjunction { last_inputs; outputs; name }) ->
         let last_inputs' = Map.set last_inputs ~key:from ~data:pulse in
         let mods' =
           Util.Map.alter mods ~key:name ~f:(function
             | Conjunction f -> Conjunction { f with last_inputs = last_inputs' }
             | m -> m)
         in
         let pulse' =
           if Map.for_all last_inputs' ~f:(equal_pulse High) then Low else High
         in
         let pulses = List.map outputs ~f:(fun n -> n, pulse', name) in
         send_pulse mods' n (t @ pulses))
  in
  Util.Sequence.nats
  |> Util.Sequence.fold_result_exn ~init:(mods, StrMap.empty) ~f:(fun (mods, ns) c ->
    let mods', maybe_name = send_pulse mods None [ "broadcaster", Low, "button" ] in
    let ns' =
      maybe_name
      |> Option.map ~f:(fun name -> Map.add_exn ns ~key:name ~data:(c + 1))
      |> Option.value ~default:ns
    in
    if Map.length ns' = 4 then Error (Map.data ns') else Ok (mods', ns'))
  |> List.map ~f:Z.of_int
  |> List.fold ~init:Z.one ~f:Z.lcm
  |> Z.to_int
;;

let part1 input = input |> parse |> solve
let part2 input = input |> parse |> solve'
