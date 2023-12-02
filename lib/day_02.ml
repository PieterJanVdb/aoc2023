open Base

type rgb = int * int * int

type game =
  { id : int
  ; sets : rgb list
  }

let digit_re = Re.compile Re.(rep1 digit)

let cubes_re =
  Re.compile
    Re.(
      seq
        [ group (rep1 digit); space; group (alt [ str "red"; str "blue"; str "green" ]) ])
;;

let to_set line =
  line
  |> Re.all cubes_re
  |> List.fold ~init:(0, 0, 0) ~f:(fun (r, g, b) group ->
    let count = Int.of_string (Re.Group.get group 1) in
    let color = Re.Group.get group 2 in
    match color with
    | "red" -> count, g, b
    | "green" -> r, count, b
    | "blue" -> r, g, count
    | _ -> r, g, b)
;;

let to_game input =
  input
  |> String.split_lines
  |> List.map ~f:(fun l ->
    let id = l |> Re.matches digit_re |> List.hd_exn |> Int.of_string in
    let sets = l |> String.split ~on:';' |> List.map ~f:to_set in
    { id; sets })
;;

let part1 input =
  input
  |> to_game
  |> List.filter ~f:(fun { sets; _ } ->
    not (List.exists sets ~f:(fun (r, g, b) -> r > 12 || g > 13 || b > 14)))
  |> List.fold ~init:0 ~f:(fun sum game -> sum + game.id)
  |> Int.to_string
;;

let part2 input =
  input
  |> to_game
  |> List.fold ~init:0 ~f:(fun sum { sets; _ } ->
    let r, g, b =
      List.fold sets ~init:(0, 0, 0) ~f:(fun (r', g', b') (r, g, b) ->
        Int.max r' r, Int.max g' g, Int.max b' b)
    in
    sum + (r * g * b))
  |> Int.to_string
;;
