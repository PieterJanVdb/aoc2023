open Base

let re_digit = Re.(compile (rep1 digit))
let re_symbol = Re.(compile (compl [ digit; char '.' ]))

let get_digit lines (row, l, r) =
  List.nth_exn lines row |> String.sub ~pos:l ~len:(r - l + 1) |> Int.of_string
;;

let get_locations lines =
  let get_line_locations re row line =
    List.map (Re.all re line) ~f:(fun group ->
      let l, r = (Re.Group.all_offset group).(0) in
      row, l, r - 1)
  in
  List.foldi lines ~init:([], []) ~f:(fun row (d_rows, s_rows) line ->
    ( d_rows @ get_line_locations re_digit row line
    , s_rows @ get_line_locations re_symbol row line ))
;;

let get_adjacents rows loc =
  let is_adjacent_row (row, _, _) (row', _, _) = row' >= row - 1 && row' <= row + 1 in
  let has_overlap (_, l, r) (_, l', r') = l' <= r + 1 && r' >= l - 1 in
  rows |> List.filter ~f:(fun loc' -> is_adjacent_row loc loc' && has_overlap loc loc')
;;

let part1 input =
  let lines = String.split_lines input in
  let d_locations, s_locations = get_locations lines in
  d_locations
  |> List.filter ~f:(fun loc -> List.length (get_adjacents s_locations loc) > 0)
  |> List.fold ~init:0 ~f:(fun sum loc -> sum + get_digit lines loc)
  |> Int.to_string
;;

let part2 input =
  let lines = String.split_lines input in
  let d_locations, s_locations = get_locations lines in
  s_locations
  |> List.map ~f:(get_adjacents d_locations)
  |> List.filter ~f:(fun adj -> List.length adj = 2)
  |> List.fold ~init:0 ~f:(fun sum adj ->
    let gears = List.map adj ~f:(get_digit lines) in
    sum + (List.nth_exn gears 0 * List.nth_exn gears 1))
  |> Int.to_string
;;
