open Base

type pattern = char list list

let parse input =
  input
  |> Re.split (Re.compile Re.(str "\n\n"))
  |> List.map ~f:(fun pattern ->
    pattern |> String.split_lines |> List.map ~f:(fun line -> line |> String.to_list))
;;

let rec find_reflection_n ?(exclude = -1) ?(prev = []) (pattern : pattern) =
  let eq = List.equal Char.( = ) in
  match pattern with
  | a :: b :: rest when eq a b ->
    let left = prev @ [ a ] in
    let right = b :: rest in
    let answer = List.length left in
    let cmp_len = Int.min answer (List.length right) in
    let left_cmp = List.drop left (answer - cmp_len) in
    let right_cmp = List.take right cmp_len |> List.rev in
    let found =
      List.zip_exn left_cmp right_cmp |> List.for_all ~f:(fun (a, b) -> eq a b)
    in
    if found && answer <> exclude
    then answer
    else find_reflection_n right ~exclude ~prev:left
  | a :: rest -> find_reflection_n rest ~exclude ~prev:(prev @ [ a ])
  | _ -> 0
;;

let rec try_variation pattern exclude x y =
  let variation =
    List.mapi pattern ~f:(fun y' row ->
      List.mapi row ~f:(fun x' tile ->
        match x', y' with
        | x', y' when x = x' && y = y' -> if Char.( = ) tile '#' then '.' else '#'
        | _ -> tile))
  in
  let reflection = find_reflection_n variation ~exclude in
  let max_y = List.length pattern - 1 in
  let max_x = List.length (List.hd_exn pattern) - 1 in
  let next_x, next_y = if x >= max_x then 0, y + 1 else x + 1, y in
  match next_y > max_y, reflection with
  | false, n when n = 0 -> try_variation pattern exclude next_x next_y
  | _, n -> n
;;

let solve ?(part2 = false) input =
  let patterns = parse input in
  let horizontal_sum =
    List.fold patterns ~init:0 ~f:(fun sum pattern ->
      let og = find_reflection_n pattern in
      sum + if not part2 then og else try_variation pattern og 0 0)
  in
  let vertical_sum =
    List.fold patterns ~init:0 ~f:(fun sum pattern ->
      let transposed = List.transpose_exn pattern in
      let og = find_reflection_n transposed in
      sum + if not part2 then og else try_variation transposed og 0 0)
  in
  vertical_sum + (horizontal_sum * 100) |> Int.to_string
;;

let part1 input = solve input
let part2 input = solve input ~part2:true
