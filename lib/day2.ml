open Core

module Input = struct
  type int_list = int list [@@deriving show]

  type string_list = string list [@@deriving show]

  let crack_input input =
    String.split_on_chars input ~on:['\n'; ',']
    |> List.filter ~f:(fun a -> String.( <> ) (String.strip a) "")
    |> List.map ~f:int_of_string

  let cycle codes program (zero, one) =
    program.(1) <- zero ;
    program.(2) <- one ;
    let rec help = function
      | 1 :: pos1 :: pos2 :: out_pos :: t ->
          program.(out_pos) <- program.(pos1) + program.(pos2) ;
          help t
      | 2 :: pos1 :: pos2 :: out_pos :: t ->
          program.(out_pos) <- program.(pos1) * program.(pos2) ;
          help t
      | _ ->
          ()
    in
    help codes ; program.(0)
end

module Day2 = struct
  (* let input = Some "1,9,10,3,2,3,11,0,99,30,40,50" *)
  let input = None

  (* let input = Some "\n14\n1969\n100756" *)

  let part1_expected = 4023471

  let part1 input =
    (* Input.crack_input input |> Input.show_string_list |> print_endline ; *)
    let codes = Input.crack_input input in
    (* List.iter t ~f:(fun a -> Input.show_string_list a |> print_endline) ; *)
    let program = List.to_array codes in
    Input.cycle codes program (12, 2)

  let part2_expected = 8051

  let part2 input =
    let codes = Input.crack_input input in
    let program = List.to_array codes in
    let range = List.range 0 101 in
    let v =
      List.fold_until ~init:(0, 0)
        ~f:(fun _acc i ->
          let out =
            List.fold_until ~init:(0, 0)
              ~f:(fun _acc j ->
                let output = Input.cycle codes (Array.copy program) (i, j) in
                if output = 19690720 then Stop (i, j) else Continue (i, j) )
              ~finish:(fun _ -> (0, 0))
              range
          in
          match out with 0, 0 -> Continue (0, 0) | a -> Stop a )
        ~finish:(fun _ -> failwith "can't finish")
        range
    in
    let noun, verb = v in
    (noun * 100) + verb

  let day = 2

  let year = 2019
end
