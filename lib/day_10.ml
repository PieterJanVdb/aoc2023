open Base

type direction =
  | North
  | East
  | South
  | West

type location = int * int
type visible_pipe = direction * direction

type tile =
  | Start
  | Visible of visible_pipe

let parse input =
  input
  |> String.split_lines
  |> List.foldi ~init:[] ~f:(fun row acc line ->
    let pipes =
      List.filter_mapi (String.to_list line) ~f:(fun column c ->
        let loc = row, column in
        match c with
        | '|' -> Some (loc, Visible (North, South))
        | '-' -> Some (loc, Visible (West, East))
        | 'L' -> Some (loc, Visible (North, East))
        | 'J' -> Some (loc, Visible (West, North))
        | '7' -> Some (loc, Visible (West, South))
        | 'F' -> Some (loc, Visible (South, East))
        | 'S' -> Some (loc, Start)
        | _ -> None)
    in
    acc @ pipes)
;;

let get_connected_pipes grid (x, y) (dir : direction option) =
  List.fold grid ~init:[] ~f:(fun connected (loc', pipe') ->
    let eq = Poly.equal in
    let result =
      match dir, loc' with
      | (None | Some North), (x', y') when y = y' && x = x' + 1 ->
        (match pipe' with
         | Visible (l, r) when eq l South -> Some r, Some (loc', pipe')
         | Visible (l, r) when eq r South -> Some l, Some (loc', pipe')
         | Start -> None, Some (loc', pipe')
         | _ -> None, None)
      | (None | Some East), (x', y') when x = x' && y = y' - 1 ->
        (match pipe' with
         | Visible (l, r) when eq l West -> Some r, Some (loc', pipe')
         | Visible (l, r) when eq r West -> Some l, Some (loc', pipe')
         | Start -> None, Some (loc', pipe')
         | _ -> None, None)
      | (None | Some South), (x', y') when y = y' && x = x' - 1 ->
        (match pipe' with
         | Visible (l, r) when eq l North -> Some r, Some (loc', pipe')
         | Visible (l, r) when eq r North -> Some l, Some (loc', pipe')
         | Start -> None, Some (loc', pipe')
         | _ -> None, None)
      | (None | Some West), (x', y') when x = x' && y = y' + 1 ->
        (match pipe' with
         | Visible (l, r) when eq l East -> Some r, Some (loc', pipe')
         | Visible (l, r) when eq r East -> Some l, Some (loc', pipe')
         | Start -> None, Some (loc', pipe')
         | _ -> None, None)
      | _ -> None, None
    in
    match result with
    | Some dir, Some loc -> (Some dir, loc) :: connected
    | None, Some loc -> (None, loc) :: connected
    | _ -> connected)
;;

let rec traverse ?(route = []) grid (loc, pipe) dir =
  match List.is_empty route, pipe with
  | false, Start -> Some route
  | _ ->
    (match get_connected_pipes grid loc dir with
     | [] -> None
     | pipes ->
       List.find_map pipes ~f:(fun (dir', (loc', pipe')) ->
         traverse grid (loc', pipe') dir' ~route:((loc, pipe) :: route)))
;;

let get_enclosed (route : location list) =
  let route_cycle_a = Sequence.cycle_list_exn route in
  let route_cycle_b = Sequence.drop_eagerly route_cycle_a 1 in
  let pairs =
    Sequence.to_list
      (Sequence.take (Sequence.zip route_cycle_a route_cycle_b) (List.length route))
  in
  (* Shoelace formula for polygon area *)
  let area =
    List.fold pairs ~init:0 ~f:(fun acc ((x, y), (x', y')) -> acc + ((x * y') - (y * x')))
    |> fun x -> Int.abs x / 2
  in
  let border_points = List.length route / 2 in
  (* Pick's theorem but with known area instead of known points inside polygon *)
  area - border_points + 1
;;

let get_route input =
  let grid = parse input in
  let start =
    List.find_exn grid ~f:(fun p ->
      match p with
      | _, Start -> true
      | _ -> false)
  in
  match traverse grid start None with
  | Some route -> route
  | None -> failwith "No loop could be found"
;;

let part1 input =
  let route = get_route input in
  let max_steps = Int.round_down (List.length route / 2) ~to_multiple_of:1 in
  Int.to_string max_steps
;;

let part2 input =
  let route = get_route input in
  let enclosed = get_enclosed (List.map route ~f:(fun (loc, _) -> loc)) in
  Int.to_string enclosed
;;
