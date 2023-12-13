open Base

module Line = struct
  module T = struct
    type spring =
      | Operational
      | Damaged
      | Unknown
    [@@deriving compare, hash, sexp_of]

    type t =
      { springs : spring list
      ; groups : int list
      }
    [@@deriving compare, hash, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

let spring_of_char = function
  | '#' -> Some Line.Damaged
  | '?' -> Some Line.Unknown
  | '.' -> Some Line.Operational
  | _ -> None
;;

let repeat list n =
  Sequence.cycle_list_exn list
  |> (fun s -> Sequence.take s (List.length list * n))
  |> Sequence.to_list
;;

let line_of_string str ~unfold =
  let split = String.split str ~on:' ' in
  match split with
  | [ a; b ] ->
    let springs =
      String.to_list a
      |> List.filter_map ~f:spring_of_char
      |> fun l ->
      if unfold
      then (
        let l' = l @ [ Line.Unknown ] in
        repeat l' 5 |> List.drop_last_exn)
      else l
    in
    let groups =
      String.split b ~on:','
      |> List.map ~f:Int.of_string
      |> fun l -> if unfold then repeat l 5 else l
    in
    Some ({ springs; groups } : Line.t)
  | _ -> None
;;

let parse ?(unfold = false) input =
  input |> String.split_lines |> List.filter_map ~f:(line_of_string ~unfold)
;;

let get_n line =
  let cache = Hashtbl.create ~growth_allowed:true ~size:100 (module Line) in
  let rec continue line =
    let try_group (line : Line.t) group_n =
      let group = List.take line.springs group_n in
      let rest = List.drop line.springs group_n in
      (* Length of the created group has to be equal to group_n *)
      let is_right_size = List.length group = group_n in
      (* Group can't contain operational springs *)
      let has_no_operationals = not (List.mem group Operational ~equal:Poly.( = )) in
      let possible = is_right_size && has_no_operationals in
      match rest with
      (* We have no springs left so we check if creating the group is possible *)
      | [] -> if possible then continue { line with springs = [] } else 0
      (* If we have springs left, we check if the next one isn't damaged (otherwise, the group is no longer valid). *)
      (* If isn't we skip the spring (gotta have at least 1 operational between groups) and proceed with the rest *)
      | spring :: rest ->
        let is_next_damaged = Poly.( = ) spring Damaged in
        if (not is_next_damaged) && possible
        then continue { line with springs = rest }
        else 0
    in
    match Hashtbl.find cache line with
    | Some n -> n
    | None ->
      let n =
        match line.groups with
        | group :: gs ->
          (match line.springs with
           (* If operational continue with all springs to the right *)
           | Operational :: rest -> continue { line with springs = rest }
           (* If damaged try to create the current group *)
           | Damaged :: _ -> try_group { line with groups = gs } group
           (* If unknown it could be either operational or damaged so try both approaches and sum *)
           | Unknown :: rest ->
             try_group { line with groups = gs } group
             + continue { line with springs = rest }
           (* If we still have groups but springs the solution is invalid *)
           | _ -> 0)
        (* If we have no groups left while there are still damaged springs the solution is invalid *)
        | [] -> if not (List.mem line.springs Damaged ~equal:Poly.( = )) then 1 else 0
      in
      (* Add the line and its result to the cache to prevent recalculations *)
      Hashtbl.set cache ~key:line ~data:n;
      n
  in
  continue line
;;

let solve ?(part2 = false) input =
  input
  |> parse ~unfold:part2
  |> List.fold ~init:0 ~f:(fun sum line -> sum + get_n line)
  |> Int.to_string
;;

let part1 input = solve input
let part2 input = solve input ~part2:true
