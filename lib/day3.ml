(* TODO Refractor this piece of shit make it a lot better *)
(* TODO Refractor this piece of shit make it a lot better *)
(* TODO Refractor this piece of shit make it a lot better *)
open Core

module Input = struct
  type direction = Left | Right | Down | Up

  let map_direction = function
    | 'R' ->
        Right
    | 'L' ->
        Left
    | 'D' ->
        Down
    | 'U' ->
        Up
    | _ ->
        failwith "no matching direction"

  let directions_string = function
    | Left ->
        "Left"
    | Right ->
        "Right"
    | Down ->
        "Down"
    | Up ->
        "Up"

  let number_parser =
    let open Angstrom in
    take_while1 (function '0' .. '9' -> true | _ -> false)
    >>| fun digits -> int_of_string digits

  let parse_char = Angstrom.(any_char >>| map_direction)

  let parse_tuple =
    Angstrom.lift2 (fun dir num -> (dir, num)) parse_char number_parser

  let parse_input input =
    Angstrom.parse_string ~consume:All parse_tuple input
    |> Result.ok
    |> Option.value ~default:(Right, 0)

  let parse_input_unit input =
    match Angstrom.parse_string ~consume:All parse_tuple input with
    | Ok (dir, amount) ->
        printf "(%s, %d) " (directions_string dir) amount
    | Error msg ->
        printf "error: %s" msg

  type string_list = string list [@@deriving show]

  let crack_input input =
    (* String.split input ~on:'\n' |> show_string_list |> printf "%s" ; *)
    String.split input ~on:'\n'
    |> List.map ~f:(fun path -> String.split path ~on:',')

  type string_list_list = string list list [@@deriving show]
  (* |> Core.List.filter ~f:(fun a -> Core.String.strip a <> "") *)
end

module PointSet = Set.Make (struct
  type t = int * int [@@deriving compare, sexp]
end)

module Day3 = struct
  let input = None

  (* let input = Some "12\n14\n1969\n100756" *)
  (* let input = *)
  (*   Some "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83" *)

  (* let input = Some "R8,U5,L5,D3\nU7,R6,D4,L4" *)

  (* let input = *)
  (*   Some "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83" *)

  let part1_expected = 316

  let part1 input =
    (* Input.crack_input input |> Input.show_string_list_list |> print_endline ; *)
    let path1, path2 =
      Input.crack_input input
      |> List.map ~f:(fun a -> List.map a ~f:(fun a -> Input.parse_input a))
      |> function a :: b :: _ -> (a, b) | _ -> failwith "should be two paths"
    in
    let calc_points setup =
      let generate_range x y num = function
        | `X ->
            if num >= 0 then
              List.map
                (List.range ~start:`inclusive ~stop:`inclusive x (x + num))
                ~f:(fun n -> (n, y))
            else
              List.map
                (List.range ~start:`inclusive ~stop:`inclusive (x + num) x)
                ~f:(fun n -> (n, y))
        | `Y ->
            if num >= 0 then
              List.map
                (List.range ~start:`inclusive ~stop:`inclusive y (y + num))
                ~f:(fun n -> (x, n))
            else
              List.map
                (List.range ~start:`inclusive ~stop:`inclusive (y + num) y)
                ~f:(fun n -> (x, n))
      in
      let _, _, lst =
        List.fold_map ~init:(0, 0, [])
          ~f:(fun (x, y, acc) (dir, num) ->
            match dir with
            | Input.Right ->
                ((x + num, y, generate_range x y num `X @ acc), (x + num, y))
            | Left ->
                ((x - num, y, generate_range x y (-num) `X @ acc), (x - num, y))
            | Up ->
                ((x, y + num, generate_range x y num `Y @ acc), (x, y + num))
            | Down ->
                ((x, y - num, generate_range x y (-num) `Y @ acc), (x, y - num)) )
          setup
        |> fst
      in
      lst
    in
    let set1, set2 =
      ( PointSet.of_list (calc_points path1)
      , PointSet.of_list (calc_points path2) )
    in
    (* printf "Set1 -> %s\n" (Sexp.to_string_hum (PointSet.sexp_of_t set1)) ; *)
    (* printf "Set2 -> %s\nSet3 -> " (Sexp.to_string_hum (PointSet.sexp_of_t set2)) ; *)
    let manhattan (x1, y1) (x2, y2) =
      float_of_int (abs (x1 - x2) + abs (y1 - y2))
    in
    Set.inter set1 set2
    |> Set.filter ~f:(function 0, 0 -> false | _ -> true)
    |> Set.fold ~init:Float.infinity ~f:(fun acc (x, y) ->
           let m = manhattan (x, y) (0, 0) in
           if Float.( < ) m acc then m else acc )
    |> int_of_float

  let part2_expected = 11

  let part2 _input = 10

  let day = 3

  let year = 2019
end
