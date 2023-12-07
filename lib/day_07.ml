open Base

type hand =
  { cards : int list
  ; bid : int
  ; group_counts : int list
  }

let cmp_hand (cards, group_counts) (cards', group_counts') =
  let rec cmp_cards hands hands' =
    match hands, hands' with
    | h :: tl, h' :: tl' -> if h > h' then 1 else if h < h' then -1 else cmp_cards tl tl'
    | _ -> 0
  in
  let open Float in
  match
    (of_int (List.length group_counts') /. of_int (List.hd_exn group_counts'))
    -. (of_int (List.length group_counts) /. of_int (List.hd_exn group_counts))
  with
  | x when x < 0. -> -1
  | x when x > 0. -> 1
  | _ -> cmp_cards cards cards'
;;

let get_hand ?(joker = false) line =
  let parse_cards cards =
    cards
    |> String.to_list
    |> List.filter_map ~f:(fun card ->
      match card with
      | 'A' -> Some 14
      | 'K' -> Some 13
      | 'Q' -> Some 12
      | 'J' -> Some (if joker then 0 else 11)
      | 'T' -> Some 10
      | '2' .. '9' -> Char.get_digit card
      | _ -> None)
  in
  let get_group_counts cards =
    cards
    |> List.sort ~compare:Int.compare
    |> List.group ~break:( <> )
    |> List.map ~f:List.length
    |> List.sort ~compare:descending
  in
  match String.lsplit2 line ~on:' ' with
  | Some (cards_s, bid_s) ->
    let bid = Int.of_string bid_s in
    let cards = parse_cards cards_s in
    let joker_n = List.length (List.filter cards ~f:(fun x -> x = 0)) in
    if joker_n > 0 && joker_n < 5
    then (
      let group_counts = get_group_counts (List.filter cards ~f:(fun x -> x <> 0)) in
      let max_count = List.hd_exn group_counts in
      let group_counts = (max_count + joker_n) :: List.drop group_counts 1 in
      Some { cards; bid; group_counts })
    else Some { cards; bid; group_counts = get_group_counts cards }
  | None -> None
;;

let solve input get_hand =
  input
  |> String.split_lines
  |> List.filter_map ~f:get_hand
  |> List.sort ~compare:(fun a b ->
    cmp_hand (a.cards, a.group_counts) (b.cards, b.group_counts))
  |> List.foldi ~init:0 ~f:(fun idx total hand -> total + ((idx + 1) * hand.bid))
  |> Int.to_string
;;

let part1 input = solve input get_hand
let part2 input = solve input (get_hand ~joker:true)
