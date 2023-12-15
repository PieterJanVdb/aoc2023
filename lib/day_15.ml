open Base

let hash str =
  let chars = String.to_list str in
  List.fold chars ~init:0 ~f:(fun n c ->
    if Char.(c <> '\n') then (Char.to_int c + n) * 17 % 256 else n)
;;

let part1 input =
  input
  |> String.split ~on:','
  |> List.fold ~init:0 ~f:(fun sum str -> sum + hash str)
  |> Int.to_string
;;

let part2 input =
  let empty_boxes =
    Map.of_alist_exn (module Int) (List.init 256 ~f:(fun idx -> idx, []))
  in
  input
  |> String.split ~on:','
  |> List.fold ~init:empty_boxes ~f:(fun boxes instruction ->
    let label, focal =
      match String.split ~on:'=' instruction with
      | [ label; focal ] -> label, Some (Int.of_string (String.strip focal))
      | _ -> String.drop_suffix instruction 1, None
    in
    let box = hash label in
    let line = Map.find_exn boxes box in
    let new_line =
      match focal with
      | Some focal ->
        if List.Assoc.mem line ~equal:String.equal label
        then
          List.map line ~f:(fun (l, f) -> if String.equal l label then l, focal else l, f)
        else (label, focal) :: line
      | None -> List.Assoc.remove line ~equal:String.equal label
    in
    Map.set boxes ~key:box ~data:new_line)
  |> Map.fold ~init:0 ~f:(fun ~key ~data sum ->
    let focusing_powers =
      List.foldi (List.rev data) ~init:0 ~f:(fun idx sum (_, focal) ->
        sum + ((key + 1) * (idx + 1) * focal))
    in
    sum + focusing_powers)
  |> Int.to_string
;;
