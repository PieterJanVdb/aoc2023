open Base

let re_d = Re.compile Re.(rep1 digit)

let get_wins line =
  Re.matches re_d line
  |> (fun l -> List.drop l 1)
  |> List.find_all_dups ~compare:String.compare
  |> List.length
;;

let part1 input =
  input
  |> String.split_lines
  |> List.fold ~init:0 ~f:(fun score line ->
    let wins = get_wins line in
    if wins > 0 then score + Int.pow 2 (wins - 1) else score)
  |> Int.to_string
;;

let part2 input =
  let lines = String.split_lines input in
  let counts = List.init (List.length lines) ~f:(fun i -> i, 1) in
  List.foldi lines ~init:counts ~f:(fun card acc line ->
    let equal = ( = ) in
    let multiplier = List.Assoc.find_exn acc ~equal card in
    let copies = List.init (get_wins line) ~f:(fun i -> card + i + 1) in
    List.fold copies ~init:acc ~f:(fun acc key ->
      match List.Assoc.find acc ~equal key with
      | Some n ->
        let n' = n + multiplier in
        let acc' = List.Assoc.remove acc ~equal key in
        List.Assoc.add acc' ~equal key n'
      | None -> acc))
  |> List.fold ~init:0 ~f:(fun sum (_, n) -> sum + n)
  |> Int.to_string
;;
