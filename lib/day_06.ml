open Base

let re_d = Re.compile Re.(rep1 digit)

let get_races ?(transform_matches = Fn.id) input =
  input
  |> String.split_lines
  |> List.map ~f:(fun line ->
    Re.matches re_d line |> transform_matches |> List.map ~f:Int.of_string)
  |> fun l -> List.zip_exn (List.nth_exn l 0) (List.nth_exn l 1)
;;

let rec get_win_n ?(n = 0) ?(press = 1) (duration, dist) =
  if press >= duration - 1
  then n
  else (
    let next_n = if press * (duration - press) > dist then n + 1 else n in
    get_win_n ~n:next_n ~press:(press + 1) (duration, dist))
;;

let solve races =
  races
  |> List.map ~f:get_win_n
  |> List.fold ~init:1 ~f:(fun acc x -> acc * x)
  |> Int.to_string
;;

let part1 input = input |> get_races |> solve

let part2 input =
  let transform_matches r = List.fold r ~init:"" ~f:( ^ ) |> fun s -> s :: [] in
  input |> get_races ~transform_matches |> solve
;;
