open Base

let re_nodes = Re.compile Re.(group (rep1 wordc))
let re_newlines = Re.compile Re.(str "\n\n")
let rec gcd a b = if b = 0 then a else gcd b (a % b)
let lcm a b = a / gcd a b * b

let rec least_common_multiple numbers =
  match numbers with
  | [] -> failwith "Numbers can't be empty"
  | [ n1; n2 ] -> lcm n1 n2
  | n :: ns -> lcm n (least_common_multiple ns)
;;

let parse input =
  let instructions, node_map =
    match String.split_lines input with
    | instructions_str :: _ :: tail ->
      let instructions = String.to_list instructions_str in
      let node_map =
        tail
        |> List.fold
             ~init:(Map.empty (module String))
             ~f:(fun map line ->
               match Re.matches re_nodes line with
               | [ key; left; right ] -> Map.set map ~key ~data:(left, right)
               | _ -> map)
      in
      instructions, node_map
    | _ -> failwith "Invalid input"
  in
  instructions, node_map
;;

let key_ends_in char key = Char.equal (List.last_exn (String.to_list key)) char

let rec traverse ?(steps = 0) map instructions key target =
  if String.equal key target
  then steps
  else (
    match Sequence.next instructions with
    | Some (hd, tl) ->
      let l, r = Map.find_exn map key in
      let next_key = if Char.equal hd 'L' then l else r in
      traverse map tl next_key target ~steps:(steps + 1)
    | None -> steps)
;;

let rec traverse_many
  ?(steps = 0)
  ?(z_steps = Map.empty (module Int))
  map
  instructions
  keys
  =
  if List.length (Map.keys z_steps) = List.length keys
  then least_common_multiple (Map.data z_steps)
  else (
    let next_z_steps =
      List.foldi keys ~init:z_steps ~f:(fun i acc k ->
        if key_ends_in 'Z' k then Map.set acc ~key:i ~data:steps else acc)
    in
    match Sequence.next instructions with
    | Some (hd, tl) ->
      let next_keys =
        List.map keys ~f:(fun k ->
          let l, r = Map.find_exn map k in
          if Char.equal hd 'L' then l else r)
      in
      traverse_many map tl next_keys ~steps:(steps + 1) ~z_steps:next_z_steps
    | None -> steps)
;;

let part1 input =
  let instructions, node_map = parse input in
  let start, target = Map.keys node_map |> fun l -> List.hd_exn l, List.last_exn l in
  let steps = traverse node_map (Sequence.cycle_list_exn instructions) start target in
  steps |> Int.to_string
;;

let part2 input =
  let instructions, node_map = parse input in
  let keys = Map.keys node_map |> List.filter ~f:(key_ends_in 'A') in
  let steps = traverse_many node_map (Sequence.cycle_list_exn instructions) keys in
  steps |> Int.to_string
;;
