open Core
(* Requirements *)

(* 1 -> negative numbers *)
(* 2 -> a instruction pointer *)
(* 3 -> Some instructioins have a set number of params they can take *)
module Day5 = struct
  (* Required feilds *)
  type program = string array [@@deriving show]

  type int_list = int list [@@deriving show]

  type string_list = string list [@@deriving show]

  let add_mult ~memory p1 p2 p3 inst =
    printf "add_mult -> %d" p3 ;
    ( match inst with
    | '1' ->
        p1 + p2
    | '2' ->
        p1 * p2
    | _ ->
        failwith "non recognized opcode" )
    |> string_of_int
    |> fun a -> memory.(p3) <- a

  let create_array input =
    String.split ~on:',' input |> List.map ~f:String.strip

  (* run memory is what we update input is the starting value position is the current memory output is the list of otuputs and code is the current code *)
  let mode ~p memory mode_bit =
    printf "mode p -> %s" p ;
    match mode_bit with
    | '0' ->
        memory.(p |> int_of_string)
    | '1' ->
        p
    | _ ->
        failwith "no a recognized mode bit"

  let execute_opcode ~memory ~f _ b c inst param1 param2 param3 =
    let param3 = param3 |> int_of_string in
    let param2 = mode ~p:param2 memory b |> int_of_string in
    let param1 = mode ~p:param1 memory c |> int_of_string in
    f ~memory param1 param2 param3 inst

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
    | opcode :: param1 :: param2 :: param3 :: tail -> (
        printf "\ncurrent opcode -> %s\n" opcode ;
        match String.to_list opcode with
        | [b; c; _; inst] ->
            execute_opcode '0' b c inst param1 param2 param3 ~f:add_mult ~memory ;
            run memory input outputs tail
        | [inst] ->
            execute_opcode '0' '1' '1' inst param1 param2 param3 ~f:add_mult
              ~memory ;
            run memory input outputs tail
        | _ ->
            failwith "empty opcode" )
    | a ->
        show_string_list a |> printf "%s" ;
        failwith "invalid opcode"

  let input = None

  let part1_expected = 3412094

  let part1 input =
    let codes = create_array input in
    let memory = codes |> Array.of_list in
    printf "array length -> %d" (Array.length memory) ;
    let _outputs = run memory "1" [] codes in
    10

  let part2_expected = 5115267

  let part2 _input = 10

  let day = 5

  let year = 2019
end
