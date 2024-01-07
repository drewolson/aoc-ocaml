module P = Util.Parser
open P.Syntax

type type' =
  | FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard
[@@deriving eq, ord]

type card =
  | Ace
  | King
  | Queen
  | Jack
  | Ten
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two
[@@deriving eq, ord, enum]

module WildCard = struct
  type t = card

  let compare a b =
    match a, b with
    | Jack, Jack -> 0
    | Jack, _ -> 1
    | _, Jack -> -1
    | a, b -> compare_card a b
  ;;
end

let all_of_card = List.(min_card -- max_card) |> List.filter_map ~f:card_of_enum

let card_p =
  P.choice
    [ Ace <$ P.char 'A'
    ; King <$ P.char 'K'
    ; Queen <$ P.char 'Q'
    ; Jack <$ P.char 'J'
    ; Ten <$ P.char 'T'
    ; Nine <$ P.char '9'
    ; Eight <$ P.char '8'
    ; Seven <$ P.char '7'
    ; Six <$ P.char '6'
    ; Five <$ P.char '5'
    ; Four <$ P.char '4'
    ; Three <$ P.char '3'
    ; Two <$ P.char '2'
    ]
;;

let hand_p =
  let+ cards = P.many1 card_p <* P.spaces
  and+ score = P.integer in
  cards, score
;;

let hands_p = P.sep_by1 P.end_of_line hand_p

let hand_type hand =
  let counts =
    hand
    |> List.group_by ~eq:equal_card
    |> List.map ~f:List.length
    |> List.sort ~cmp:(fun a b -> -compare a b)
  in
  match counts with
  | [ 5 ] -> FiveOfAKind
  | [ 4; 1 ] -> FourOfAKind
  | [ 3; 2 ] -> FullHouse
  | 3 :: _ -> ThreeOfAKind
  | 2 :: 2 :: _ -> TwoPair
  | 2 :: _ -> OnePair
  | _ -> HighCard
;;

let rec expand = function
  | [] -> [ [] ]
  | Jack :: rest ->
    List.(min_card -- max_card)
    |> List.filter_map ~f:card_of_enum
    |> List.concat_map ~f:(function
      | Jack -> []
      | c -> List.map (expand rest) ~f:(List.cons c))
  | c :: rest -> List.map (expand rest) ~f:(List.cons c)
;;

let hand_type' hand =
  hand |> expand |> List.map ~f:hand_type |> List.sort ~cmp:compare_type' |> List.hd
;;

let solve input ~f =
  input
  |> P.parse_exn hands_p
  |> List.sort ~cmp:f
  |> List.mapi ~f:(fun i (_, s) -> (i + 1) * s)
  |> Util.List.sum_int
;;

let part1 input =
  solve input ~f:(fun (h1, _) (h2, _) ->
    -[%derive.ord: type' * card list] (hand_type h1, h1) (hand_type h2, h2))
;;

let part2 input =
  solve input ~f:(fun (h1, _) (h2, _) ->
    -[%derive.ord: type' * WildCard.t list] (hand_type' h1, h1) (hand_type' h2, h2))
;;
