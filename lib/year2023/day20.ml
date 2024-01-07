module P = Util.Parser
open P.Syntax
module StrMap = Map.Make (String)

type pulse =
  | High
  | Low
[@@deriving eq]

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
  | FlipFlop { name; _ } -> name
  | Conjunction { name; _ } -> name
  | Broadcaster { name; _ } -> name
;;

let mod_outputs = function
  | FlipFlop { outputs; _ } -> outputs
  | Conjunction { outputs; _ } -> outputs
  | Broadcaster { outputs; _ } -> outputs
;;

let name_p =
  P.take_while (function
    | 'a' .. 'z' -> true
    | _ -> false)
;;

let outputs_p = P.sep_by1 (P.string ", ") name_p

let broadcaster_p =
  let+ outputs = P.string "broadcaster -> " *> outputs_p in
  Broadcaster { name = "broadcaster"; outputs }
;;

let flip_flop_p =
  let+ name = P.char '%' *> name_p <* P.string " -> "
  and+ outputs = outputs_p in
  FlipFlop { name; state = Off; outputs }
;;

let conjunction_p =
  let+ name = P.char '&' *> name_p <* P.string " -> "
  and+ outputs = outputs_p in
  Conjunction { name; last_inputs = StrMap.empty; outputs }
;;

let module_p = P.choice [ flip_flop_p; conjunction_p; broadcaster_p ]

let modules_p =
  let+ modules = P.sep_by1 P.end_of_line module_p in
  modules |> List.map ~f:(fun m -> mod_name m, m) |> StrMap.of_list
;;

let initialize_inputs name mods =
  mods
  |> StrMap.filter (fun _ m -> List.mem ~eq:String.equal name (mod_outputs m))
  |> StrMap.keys
  |> List.of_iter
  |> List.map ~f:(fun k -> k, Low)
  |> StrMap.of_list
;;

let parse input =
  let mods = P.parse_exn modules_p input in
  StrMap.map
    (function
      | Conjunction fields ->
        Conjunction { fields with last_inputs = initialize_inputs fields.name mods }
      | m -> m)
    mods
;;

let solve mods =
  let update_counts (low, high) n = function
    | Low -> low + n, high
    | High -> low, high + n
  in
  let rec send_pulse mods counts = function
    | [] -> mods, counts
    | (name, pulse, from) :: t ->
      (match StrMap.find_opt name mods with
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
              StrMap.update
                name
                (function
                  | Some (FlipFlop f) -> Some (FlipFlop { f with state = state' })
                  | m -> m)
                mods
            in
            let pulses = List.map outputs ~f:(fun n -> n, pulse', name) in
            let n = List.length pulses in
            send_pulse mods' (update_counts counts n pulse') (t @ pulses))
       | Some (Conjunction { last_inputs; outputs; name }) ->
         let last_inputs' = StrMap.add from pulse last_inputs in
         let mods' =
           StrMap.update
             name
             (function
               | Some (Conjunction f) ->
                 Some (Conjunction { f with last_inputs = last_inputs' })
               | m -> m)
             mods
         in
         let pulse' =
           if StrMap.for_all (fun _ p -> equal_pulse High p) last_inputs'
           then Low
           else High
         in
         let pulses = List.map outputs ~f:(fun n -> n, pulse', name) in
         let n = List.length pulses in
         send_pulse mods' (update_counts counts n pulse') (t @ pulses))
  in
  let results =
    List.(0 --^ 1000)
    |> List.fold_map ~init:mods ~f:(fun mods _ ->
      let mods', (low, high) = send_pulse mods (1, 0) [ "broadcaster", Low, "button" ] in
      mods', (low, high))
    |> snd
  in
  let lows = List.fold_left ~init:0 ~f:(fun acc r -> acc + fst r) results in
  let highs = List.fold_left ~init:0 ~f:(fun acc r -> acc + snd r) results in
  lows * highs
;;

let solve' mods =
  let rec send_pulse mods n = function
    | [] -> mods, n
    | (name, pulse, from) :: t ->
      let n = if String.equal "ll" name && equal_pulse High pulse then Some from else n in
      (match StrMap.find_opt name mods with
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
              StrMap.update
                name
                (function
                  | Some (FlipFlop f) -> Some (FlipFlop { f with state = state' })
                  | m -> m)
                mods
            in
            let pulses = List.map outputs ~f:(fun n -> n, pulse', name) in
            send_pulse mods' n (t @ pulses))
       | Some (Conjunction { last_inputs; outputs; name }) ->
         let last_inputs' = StrMap.add from pulse last_inputs in
         let mods' =
           StrMap.update
             name
             (function
               | Some (Conjunction f) ->
                 Some (Conjunction { f with last_inputs = last_inputs' })
               | m -> m)
             mods
         in
         let pulse' =
           if StrMap.for_all (fun _ p -> equal_pulse High p) last_inputs'
           then Low
           else High
         in
         let pulses = List.map outputs ~f:(fun n -> n, pulse', name) in
         send_pulse mods' n (t @ pulses))
  in
  Seq.ints 0
  |> Util.Seq.fold_result ~init:(mods, StrMap.empty) ~f:(fun (mods, ns) c ->
    let mods', maybe_name = send_pulse mods None [ "broadcaster", Low, "button" ] in
    let ns' =
      maybe_name
      |> Option.map (fun name -> StrMap.add name (c + 1) ns)
      |> Option.value ~default:ns
    in
    if StrMap.cardinal ns' = 4 then Error (StrMap.values ns') else Ok (mods', ns'))
  |> List.of_iter
  |> List.map ~f:Z.of_int
  |> List.fold_left ~init:Z.one ~f:Z.lcm
  |> Z.to_int
;;

let part1 input = input |> parse |> solve
let part2 input = input |> parse |> solve'
