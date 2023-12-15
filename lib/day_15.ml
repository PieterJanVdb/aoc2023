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
  let boxes = Hashtbl.create (module Int) in
  let () =
    input
    |> String.split ~on:','
    |> List.iter ~f:(fun instruction ->
      let label, focal =
        match String.split ~on:'=' instruction with
        | [ label; focal ] -> label, Some (Int.of_string (String.strip focal))
        | _ -> String.drop_suffix instruction 1, None
      in
      let box = hash label in
      let line = Hashtbl.find_or_add boxes box ~default:(fun _ -> []) in
      let new_line =
        match focal with
        | Some focal ->
          if List.Assoc.mem line ~equal:String.equal label
          then
            List.map line ~f:(fun (l, f) ->
              if String.equal l label then l, focal else l, f)
          else (label, focal) :: line
        | None -> List.Assoc.remove line ~equal:String.equal label
      in
      Hashtbl.set boxes ~key:box ~data:new_line)
  in
  Hashtbl.fold boxes ~init:0 ~f:(fun ~key ~data sum ->
    let focusing_powers =
      List.foldi (List.rev data) ~init:0 ~f:(fun idx sum (_, focal) ->
        sum + ((key + 1) * (idx + 1) * focal))
    in
    sum + focusing_powers)
  |> Int.to_string
;;
