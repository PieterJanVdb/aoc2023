open Base

type kind =
  | Empty
  | Galaxy

type point =
  { loc : int * int
  ; details : kind
  }

let get_universe input =
  let expands universe =
    List.foldi universe ~init:[] ~f:(fun row acc list ->
      let is_occupied = List.exists list ~f:(Poly.( = ) Galaxy) in
      if is_occupied then acc else row :: acc)
  in
  let grid =
    input
    |> String.split_lines
    |> List.map ~f:(fun line ->
      line
      |> String.to_list
      |> List.map ~f:(fun c -> if Char.( = ) c '#' then Galaxy else Empty))
  in
  let row_expands = expands grid in
  let column_expands = expands (List.transpose_exn grid) in
  let universe =
    List.foldi grid ~init:[] ~f:(fun x acc row ->
      acc @ List.mapi row ~f:(fun y kind -> { loc = x, y; details = kind }))
  in
  universe, row_expands, column_expands
;;

let rec get_pairs ?(acc = []) lst =
  if List.length acc = 2
  then [ List.hd_exn acc, List.last_exn acc ]
  else (
    match lst with
    | [] -> []
    | x :: xs -> get_pairs xs ~acc:(x :: acc) @ get_pairs xs ~acc)
;;

let get_distance (x, y) (x', y') x_exp y_exp expansion_rate =
  let get_expansion src a b =
    List.fold src ~init:0 ~f:(fun acc e ->
      if (e < a && e > b) || (e > a && e < b) then acc + 1 else acc)
    |> Int.( * ) (expansion_rate - 1)
  in
  let extra_rows = get_expansion x_exp x x' in
  let extra_columns = get_expansion y_exp y y' in
  let x_diff = if x > x' then x + extra_rows - x' else x' + extra_rows - x in
  let y_diff = if y > y' then y + extra_columns - y' else y' + extra_columns - y in
  Int.abs x_diff + Int.abs y_diff
;;

let solve input expansion_rate =
  let universe, x_exp, y_exp = get_universe input in
  let galaxies =
    List.filter universe ~f:(fun { details; _ } -> Poly.( = ) details Galaxy)
  in
  let pairs = get_pairs galaxies in
  List.fold pairs ~init:0 ~f:(fun sum (a, b) ->
    sum + get_distance a.loc b.loc x_exp y_exp expansion_rate)
  |> Int.to_string
;;

let part1 input = solve input 2
let part2 input = solve input 1000000
