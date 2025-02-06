module Input = struct
  let crack_input input =
    Core.String.split input ~on:'\n'
    |> Core.List.filter ~f:(fun a -> Core.String.strip a <> "")
end

module Day1 = struct
  let input = None

  (* let input = Some "12\n14\n1969\n100756" *)

  let part1_expected = 3412094

  let part1 input =
    let str_input = Input.crack_input input in
    let calc_fuel acc mass = acc + (int_of_string mass / 3) - 2 in
    Core.List.fold ~init:0 ~f:calc_fuel str_input

  let part2_expected = 5115267

  let part2 input =
    let str_input = Input.crack_input input in
    let rec calc_fuel acc mass =
      let fuel = (mass / 3) - 2 in
      if fuel < 0 then acc else calc_fuel (fuel + acc) fuel
    in
    Core.List.fold ~init:0 ~f:calc_fuel
      (Core.List.map str_input ~f:int_of_string)

  let day = 1

  let year = 2019
end
