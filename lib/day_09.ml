open Base

let rec get_next ?(rev = false) seq =
  if List.for_all seq ~f:(fun n -> n = 0)
  then 0
  else (
    let rec get_diffs seq =
      match seq with
      | a :: b :: tl -> (b - a) :: get_diffs (b :: tl)
      | _ -> []
    in
    let next_seq = get_diffs seq in
    let op = if rev then Int.( - ) (List.hd_exn seq) else Int.( + ) (List.last_exn seq) in
    op (get_next next_seq ~rev))
;;

let solve ?(rev = false) input =
  input
  |> String.split_lines
  |> List.map ~f:(fun l -> String.split l ~on:' ' |> List.map ~f:Int.of_string)
  |> List.fold ~init:0 ~f:(fun acc seq -> acc + get_next seq ~rev)
  |> Int.to_string
;;

let part1 input = solve input
let part2 input = solve input ~rev:true
