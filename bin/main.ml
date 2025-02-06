open Core

let () =
  let numbers = [1; 2; 3; 4; 5; 6; 7; 8] in
  let _sum = List.sum (module Int) numbers ~f:(fun a -> a) in
  ()

(* 2    ; 2  ; 654   33583 *)
(* let input = *)
(*   Input.fetch_input 2019 1 |> String.split ~on:'\n' *)
(*   |> List.filter ~f:(fun a -> String.( <> ) (Core.String.strip a) "") *)

let _input = ["12"; "14"; "1969"; "100756"]

let _input = ["100756"]

let part2 _input =
  let input = ["12"; "14"; "1969"; "100756"] in
  let rec calc_fuel acc mass =
    printf "%d\n" mass ;
    let fuel = (mass / 3) - 2 in
    if fuel < 0 then acc else calc_fuel (fuel + acc) fuel
  in
  Core.List.fold ~init:0 ~f:calc_fuel (List.map input ~f:int_of_string)

let part1 _input =
  let input = ["12"; "14"; "1969"; "100756"] in
  let calc_fuel acc mass = acc + (int_of_string mass / 3) - 2 in
  Core.List.fold ~init:0 ~f:calc_fuel input

let _answer = part1 input |> printf "part1 -> %d\n"

let _ = part2 input |> printf "part2 -> %d"
