open Base

let usage_message = "aocaml -d <day> [-i <input_file>]"
let day = ref 1
let input_file = ref ""
let anon_fun filename = input_file := filename

let speclist =
  [ "-d", Caml.Arg.Set_int day, "Specific day to be solved"
  ; "-i", Caml.Arg.Set_string input_file, "Path to puzzle input, or use stdin"
  ]
;;

exception Invalid_day of string

let () =
  let () = Caml.Arg.parse speclist anon_fun usage_message in
  let part1, part2 =
    match !day with
    | 1 -> Aoc2023.Day_01.(part1, part2)
    | 2 -> Aoc2023.Day_02.(part1, part2)
    | 3 -> Aoc2023.Day_03.(part1, part2)
    | 4 -> Aoc2023.Day_04.(part1, part2)
    | 5 -> Aoc2023.Day_05.(part1, part2)
    | 6 -> Aoc2023.Day_06.(part1, part2)
    | 7 -> Aoc2023.Day_07.(part1, part2)
    | _ -> raise (Invalid_day "Invalid day given")
  in
  let open Stdio in
  let input_text =
    if String.equal !input_file ""
    then In_channel.input_all In_channel.stdin
    else In_channel.with_file ~f:In_channel.input_all !input_file
  in
  let part1_solution = part1 input_text in
  let part2_solution = part2 input_text in
  printf "Part 1: %s\nPart 2: %s\n" part1_solution part2_solution
;;
