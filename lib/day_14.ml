open Base

type tile =
  | Round
  | Cube
  | Empty

type platform = tile list list

let parse input =
  input
  |> String.split_lines
  |> List.map ~f:(fun line ->
    String.to_list line
    |> List.map ~f:(fun c ->
      match c with
      | 'O' -> Round
      | '#' -> Cube
      | _ -> Empty))
;;

let rec tilt (platform : platform) =
  let rec move platform =
    match platform with
    | [] -> false, []
    | [ a ] -> false, [ a ]
    | a :: b :: tl ->
      let had_mvmnt, pairs =
        List.zip_exn b a
        |> List.fold ~init:(false, []) ~f:(fun (mvmt, acc) (x, y) ->
          match x, y with
          | Round, Empty -> true, acc @ [ y, x ]
          | _ -> mvmt, acc @ [ x, y ])
      in
      let b, a = List.unzip pairs in
      let next_had_mvmnt, tl = move (b :: tl) in
      had_mvmnt || next_had_mvmnt, a :: tl
  in
  match move platform with
  | true, platform -> tilt platform
  | false, platform -> platform
;;

let get_load platform =
  List.foldi (List.rev platform) ~init:0 ~f:(fun idx sum row ->
    sum + (List.count row ~f:(Poly.( = ) Round) * (idx + 1)))
;;

let cycle platform =
  let north = tilt platform in
  let west = tilt (List.transpose_exn north) in
  let south = tilt (List.rev (List.transpose_exn west)) in
  let east = tilt (List.rev (List.transpose_exn south)) in
  List.rev (List.transpose_exn east) |> List.map ~f:List.rev
;;

let platform_eq a b = List.equal Poly.( = ) (List.concat a) (List.concat b)

let find_loop_len (platforms : platform list) =
  let rec loop pow lam x xs =
    match xs with
    | [] -> None
    | hd :: tl ->
      if platform_eq x hd
      then Some lam
      else if pow = lam
      then loop (2 * pow) 1 hd tl
      else loop pow (1 + lam) x tl
  in
  match platforms with
  | hd :: tl -> loop 1 1 hd tl
  | _ -> None
;;

let find_loop (platforms : platform list) =
  match find_loop_len platforms with
  | Some len ->
    let idx =
      List.zip_exn
        (List.take platforms (List.length platforms - len))
        (List.drop platforms len)
      |> List.foldi ~init:(-1) ~f:(fun idx found (a, b) ->
        if platform_eq a b then idx else found)
    in
    Some (List.sub platforms ~pos:idx ~len, idx)
  | None -> None
;;

let rec get_load_billionth ?(cache = []) ?(idx = 1) platform =
  let next_platform = cycle platform in
  let next_cache = cache @ [ next_platform ] in
  match idx, find_loop next_cache with
  | idx, _ when idx = 1000 -> None
  | _, None -> get_load_billionth next_platform ~cache:next_cache ~idx:(idx + 1)
  | _, Some (loop, idx) ->
    let bil_idx = (1000000000 - idx - 1) % List.length loop in
    (* If the lenght of the loop is less than the modulo *)
    (*  above I'm fucked but it worked so... *)
    Some (get_load (List.nth_exn loop bil_idx))
;;

let part1 input = input |> parse |> tilt |> get_load |> Int.to_string

let part2 input =
  let platform = parse input in
  match get_load_billionth platform with
  | Some load -> Int.to_string load
  | None -> failwith "No load could be found"
;;
