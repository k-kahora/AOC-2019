open Core
(* Requirements *)

(* 1 -> negative numbers *)
(* 2 -> a instruction pointer *)
(* 3 -> Some instructioins have a set number of params they can take *)
module Day5 = struct
  (* Required feilds *)
  type program = string array [@@deriving show]

  type int_list = int list [@@deriving show]

  let create_array input =
    String.split ~on:',' input |> List.map ~f:String.strip

  (* run memory is what we update input is the starting value position is the current memory output is the list of otuputs and code is the current code *)
  let mode ~parameter memory mode_bit =
    match mode_bit with
    | '0' ->
        memory.(parameter |> int_of_string)
    | '1' ->
        parameter
    | _ ->
        failwith "no a recognized mode bit"

  let rec run memory input outputs code =
    match code with
    | [] ->
        outputs |> show_int_list |> printf "%s" ;
        outputs
    | "3" :: pos :: tail ->
        memory.(pos |> int_of_string) <- input ;
        run memory input outputs tail
    | "4" :: adress :: tail ->
        run memory input ((adress |> int_of_string) :: outputs) tail
    | opcode :: param1 :: param2 :: param3 :: tail when List.length tail = 3
      -> (
      match String.to_list opcode with
      | [b; c; _; inst] ->
          let x = mode memory c ~parameter:param1 |> int_of_string in
          let y = mode memory b ~parameter:param2 |> int_of_string in
          let result = if Char.( = ) inst '1' then x + y else x * y in
          memory.(param3 |> int_of_string) <- result |> string_of_int ;
          run memory input outputs tail
      | _ ->
          failwith "empty opcode" )
    | _ ->
        failwith "invalid opcode"

  let input = None

  let part1_expected = 3412094

  let part1 input =
    let codes = create_array input in
    let memory = codes |> Array.of_list in
    let _outputs = run memory "1" [] codes in
    10

  let part2_expected = 5115267

  let part2 _input = 10

  let day = 5

  let year = 2019
end
