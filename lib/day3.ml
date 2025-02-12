open Core

module PointKey = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp]
  end

  include T
  include Comparator.Make (T)
end

module Input = struct
  let print_map map =
    map |> Map.to_alist |> [%sexp_of: ((int * int) * (bool * int)) list]
    |> Sexp.to_string_hum |> print_endline

  let direction = function
    | 'U' ->
        fun (x, y) -> (x, y + 1)
    | 'R' ->
        fun (x, y) -> (x + 1, y)
    | 'D' ->
        fun (x, y) -> (x, y - 1)
    | 'L' ->
        fun (x, y) -> (x - 1, y)
    | _ ->
        failwith "no a proper character"

  let crack_input input =
    String.split ~on:'\n' input
    |> Fn.flip List.take 2
    |> List.map ~f:(String.split ~on:',')

  let read command = (command.[0], String.drop_prefix command 1 |> int_of_string)

  (* The goal is to merge both wires and change the false to a true *)
  let merge_wires p1 p2 =
    let panel = Map.empty (module PointKey) in
    Map.fold2 ~init:panel
      ~f:(fun ~key ~data panel ->
        match data with
        | `Both ((_, step1), (_, step2)) ->
            Map.add_exn panel ~key ~data:(true, step1 + step2)
        | `Right a | `Left a ->
            Map.add_exn panel ~key ~data:a )
      p1 p2

  let[@ocaml.warning "-9-11"] move_wire panel position step_start (dir, count) =
    let direction = direction dir in
    (* put the value in map at each step *)
    let rec loop cur_pos map steps = function
      | n when n > 0 ->
          (* let x, y = cur_pos in *)
          (* printf "(%d,%d)\n" x y ; *)
          let next_step = direction cur_pos in
          let map =
            Map.update map next_step ~f:(Option.value ~default:(false, steps))
          in
          loop next_step map (steps + 1) (n - 1)
      | _ ->
          (map, cur_pos, steps)
    in
    loop position panel step_start count

  let unroll_wire wire =
    let mapping = Map.empty (module PointKey) in
    let wires = List.map wire ~f:read in
    let unrolled_wire, _, _ =
      List.fold
        ~init:(mapping, (0, 0), 1)
        ~f:(fun (panel, position, step) dir -> move_wire panel position step dir)
        wires
    in
    unrolled_wire

  let wrap_and_roll ~f input =
    let panel = Map.empty (module PointKey) in
    let wrapped =
      List.fold ~init:panel
        ~f:(fun panel wire -> unroll_wire wire |> merge_wires panel)
        input
    in
    let inter = Map.filter ~f:fst wrapped in
    (* print_map inter ; *)
    inter |> Map.map ~f:snd |> Map.fold ~init:Int.max_value ~f
end

(* Plan is to create a map of each point and also keep track of steps for each point *)

module Day3 = struct
  let input = None

  let part1_expected = 316

  let part1 input =
    let wires = Input.crack_input input in
    let manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2) in
    let f_part1 ~key ~data:_ min_distance =
      min (manhattan (0, 0) key) min_distance
    in
    Input.wrap_and_roll ~f:f_part1 wires
  (* unroll wires *)

  let part2_expected = 16368

  let part2 input =
    let wires = Input.crack_input input in
    let f_part1 ~key:_ ~data min_distance = min min_distance data in
    Input.wrap_and_roll ~f:f_part1 wires

  let day = 3

  let year = 2019
end
