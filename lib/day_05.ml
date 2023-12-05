open Base

type mapping =
  { source : int
  ; target : int
  ; range : int
  }

type almanac =
  { seeds : int list
  ; trajectories : mapping list list
  }

let re_digits = Re.compile Re.(rep1 digit)
let re_newlines = Re.compile Re.(str "\n\n")

let get_mappings (digits : int list) =
  digits
  |> List.chunks_of ~length:3
  |> List.fold ~init:[] ~f:(fun acc line ->
    match line with
    | [ target; source; range ] -> acc @ [ { source; target; range } ]
    | _ -> acc)
  |> List.sort ~compare:(fun a b -> a.source - b.source)
;;

let trace_seed trajectories seed =
  List.fold trajectories ~init:seed ~f:(fun source mappings ->
    match
      List.find mappings ~f:(fun mapping ->
        source >= mapping.source && source < mapping.source + mapping.range)
    with
    | Some mapping -> mapping.target + (source - mapping.source)
    | None -> source)
;;

let rec trace_seed_range ?smallest trajectories seed limit =
  if seed >= limit
  then smallest
  else (
    let location = trace_seed trajectories seed in
    let next_smallest =
      match smallest with
      | Some n -> Int.min n location
      | None -> location
    in
    trace_seed_range trajectories (seed + 1) limit ~smallest:next_smallest)
;;

let to_almanac input =
  let empty_almanac = { seeds = []; trajectories = [] } in
  input
  |> Re.split re_newlines
  |> List.foldi ~init:empty_almanac ~f:(fun idx alm category ->
    let digits = Re.matches re_digits category |> List.map ~f:Int.of_string in
    match idx with
    | 0 -> { alm with seeds = digits }
    | _ ->
      let mappings = get_mappings digits in
      { alm with trajectories = alm.trajectories @ [ mappings ] })
;;

let part1 input =
  let almanac = to_almanac input in
  let locations = List.map almanac.seeds ~f:(trace_seed almanac.trajectories) in
  locations |> List.min_elt ~compare:ascending |> Option.value_exn |> Int.to_string
;;

let part2 input =
  let almanac = to_almanac input in
  let locations =
    almanac.seeds
    |> List.chunks_of ~length:2
    |> List.filter_map ~f:(fun pair ->
      match pair with
      | [ start; range ] -> trace_seed_range almanac.trajectories start (start + range)
      | _ -> None)
  in
  locations |> List.min_elt ~compare:ascending |> Option.value_exn |> Int.to_string
;;
