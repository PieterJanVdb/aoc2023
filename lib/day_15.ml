open Base

module BoxKey = struct
  module T = struct
    type t = string

    let hash x =
      let chars = String.to_list x in
      List.fold chars ~init:0 ~f:(fun n c ->
        if Char.(c <> '\n') then (Char.to_int c + n) * 17 % 256 else n)
    ;;

    let compare x y = Int.compare (hash x) (hash y)
    let sexp_of_t x = Int.sexp_of_t (hash x)
  end

  include T
  include Comparable.Make (T)
end

let part1 input =
  input
  |> String.split ~on:','
  |> List.fold ~init:0 ~f:(fun sum str -> sum + BoxKey.hash str)
  |> Int.to_string
;;

let part2 input =
  let boxes = Hashtbl.create (module BoxKey) in
  let () =
    input
    |> String.split ~on:','
    |> List.iter ~f:(fun instruction ->
      let label, focal =
        match String.split ~on:'=' instruction with
        | [ label; focal ] -> label, Some (Int.of_string (String.strip focal))
        | _ -> String.drop_suffix instruction 1, None
      in
      let line = Hashtbl.find_or_add boxes label ~default:(fun _ -> []) in
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
      Hashtbl.set boxes ~key:label ~data:new_line)
  in
  Hashtbl.fold boxes ~init:0 ~f:(fun ~key ~data sum ->
    let focusing_powers =
      List.foldi (List.rev data) ~init:0 ~f:(fun idx sum (_, focal) ->
        sum + ((BoxKey.hash key + 1) * (idx + 1) * focal))
    in
    sum + focusing_powers)
  |> Int.to_string
;;
