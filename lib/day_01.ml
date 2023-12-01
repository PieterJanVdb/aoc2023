open Base

let word_map =
  [
    ("one", "o1e");
    ("two", "t2o");
    ("three", "t3e");
    ("four", "f4r");
    ("five", "f5e");
    ("six", "s6x");
    ("seven", "s7n");
    ("eight", "e8t");
    ("nine", "n9e");
  ]

let replace_words txt =
  let open String.Search_pattern in
  List.fold ~init:txt
    ~f:(fun acc (word, repl) -> replace_all (create word) ~in_:acc ~with_:repl)
    word_map

let extract_digit line =
  line |> Re.matches Re.(compile (repn digit 1 (Some 1))) |> fun t ->
  List.hd_exn t ^ List.last_exn t |> Int.of_string

let solve input =
  input
  |> String.split_lines
  |> List.map ~f:extract_digit
  |> List.fold ~init:0 ~f:( + )

let part1 input = input |> solve |> Int.to_string
let part2 input = input |> replace_words |> solve |> Int.to_string
