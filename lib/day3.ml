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

  let[@ocaml.warning "-9-11"] move_wire panel position step_start (dir, count) =
    let direction = direction dir in
    (* put the value in map at each step *)
    let rec loop cur_pos map steps = function
      | n when n > 0 ->
          let next_step = direction cur_pos in
          let map =
            Map.update panel next_step ~f:(Option.value ~default:(false, steps))
          in
          loop next_step map (steps + 1) (n - 1)
      | _ ->
          (map, cur_pos, steps)
    in
    loop position panel step_start count

  let unroll_wire wire =
    let mapping = Map.empty (module PointKey) in
    let wires = List.map wire ~f:read in
    List.fold
      ~init:(mapping, (0, 0), 0)
      ~f:(fun (panel, position, step) dir -> move_wire panel position step dir)
      wires
end

(* Plan is to create a map of each point and also keep track of steps for each point *)

module Day3 = struct
  (* let input = None *)

  (* let input = Some "12\n14\n1969\n100756" *)
  let input =
    Some "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83"

  (* let input = Some "R8,U5,L5,D3\nU7,R6,D4,L4" *)

  let part1_expected = 159

  let part1 input =
    let wires = Input.crack_input input in
    let mapping, _, _ =
      List.map ~f:Input.unroll_wire wires |> List.hd |> Option.value_exn
    in
    Input.print_map mapping ;
    (* unroll wires *)
    10

  let part2_expected = 610

  let part2 _input = 10

  let day = 3

  let year = 2019
end
