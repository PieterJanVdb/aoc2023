open Base

type direction =
  | Up
  | Down
  | Left
  | Right

type tile_kind =
  | Empty
  | Left_mirror
  | Right_mirror
  | Vertical_splitter
  | Horizontal_splitter

type tile =
  { loc : int * int
  ; kind : tile_kind
  }

module Pair = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

let parse input =
  String.split_lines input
  |> List.foldi ~init:[] ~f:(fun y acc line ->
    let tiles =
      List.mapi (String.to_list line) ~f:(fun x c ->
        let kind =
          match c with
          | '\\' -> Left_mirror
          | '/' -> Right_mirror
          | '|' -> Vertical_splitter
          | '-' -> Horizontal_splitter
          | _ -> Empty
        in
        { loc = x, y; kind })
    in
    acc @ tiles)
;;

let get tiles (x, y) = List.find tiles ~f:(fun { loc = x', y'; _ } -> x = x' && y = y')

let get_v visited (x, y) dir =
  List.find visited ~f:(fun ({ loc = x', y'; _ }, dir') ->
    x = x' && y = y' && Poly.(dir = dir'))
;;

let get_next_loc (x, y) dir =
  match dir with
  | Up -> x, y - 1
  | Down -> x, y + 1
  | Left -> x - 1, y
  | Right -> x + 1, y
;;

let get_next_dirs tile dir =
  match tile.kind with
  | Empty -> [ dir ]
  | Left_mirror ->
    (match dir with
     | Up -> [ Left ]
     | Down -> [ Right ]
     | Left -> [ Up ]
     | Right -> [ Down ])
  | Right_mirror ->
    (match dir with
     | Up -> [ Right ]
     | Down -> [ Left ]
     | Left -> [ Down ]
     | Right -> [ Up ])
  | Vertical_splitter ->
    (match dir with
     | Left | Right -> [ Up; Down ]
     | Up -> [ Up ]
     | Down -> [ Down ])
  | Horizontal_splitter ->
    (match dir with
     | Up | Down -> [ Left; Right ]
     | Left -> [ Left ]
     | Right -> [ Right ])
;;

let rec energize ?(visited = []) tiles tile dir =
  match get_v visited tile.loc dir with
  | Some _ -> visited
  | None ->
    let next_dirs = get_next_dirs tile dir in
    let next_visited = (tile, dir) :: visited in
    List.fold next_dirs ~init:next_visited ~f:(fun v d ->
      let next_loc = get_next_loc tile.loc d in
      let next_tile = get tiles next_loc in
      match next_tile with
      | None -> v
      | Some next_tile -> energize ~visited:v tiles next_tile d)
;;

let get_energized_n tiles tile dir =
  let visited = energize tiles tile dir in
  let energized_tiles =
    Set.of_list (module Pair) (List.map visited ~f:(fun (tile, _) -> tile.loc))
  in
  Set.length energized_tiles
;;

let part1 input =
  let tiles = parse input in
  let start = get tiles (0, 0) in
  match start with
  | Some tile -> Int.to_string (get_energized_n tiles tile Right)
  | None -> failwith "Start tile could not be found"
;;

let part2 input =
  let tiles = parse input in
  let xs = Set.of_list (module Int) (List.map tiles ~f:(fun tile -> fst tile.loc)) in
  let ys = Set.of_list (module Int) (List.map tiles ~f:(fun tile -> snd tile.loc)) in
  let min_x = Set.min_elt_exn xs in
  let max_x = Set.max_elt_exn xs in
  let min_y = Set.min_elt_exn ys in
  let max_y = Set.max_elt_exn ys in
  List.fold tiles ~init:0 ~f:(fun max tile ->
    let dir_x =
      match fst tile.loc with
      | x when x = min_x -> Some Right
      | x when x = max_x -> Some Left
      | _ -> None
    in
    let dir_y =
      match snd tile.loc with
      | y when y = min_y -> Some Down
      | y when y = max_y -> Some Up
      | _ -> None
    in
    let dirs = List.filter_map [ dir_x; dir_y ] ~f:Fn.id in
    List.fold dirs ~init:max ~f:(fun max dir ->
      Int.max max (get_energized_n tiles tile dir)))
  |> Int.to_string
;;
