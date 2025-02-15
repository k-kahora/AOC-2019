open Core

(* 1 -> negative numbers *)
(* 2 -> a instruction pointer *)
(* 3 -> Some instructioins have a set number of params they can take *)
module Day5 = struct
  (* Required feilds *)
  type program = string array [@@deriving show]

  type int_list = int list [@@deriving show]

  type string_list = string list [@@deriving show]

  let store ~memory ~mode i value =
    match mode with
    | 1 ->
        memory.(i) <- value
    | 0 ->
        memory.(memory.(i)) <- value
    | _ ->
        failwith "non recognized mode bit"

  let load ~memory i = function
    | 1 ->
        memory.(i)
    | 0 ->
        memory.(memory.(i))
    | _ ->
        failwith "unrecognized mode bit"

  let get_mode_bit n =
    (n / 10000 mod 10, n / 1000 mod 10, n / 100 mod 10, n mod 100)

  let rec run_operations ~memory ~limit ~input ip outputs =
    let ld = load ~memory in
    let st = store ~memory in
    if ip >= limit then outputs
    else
      let inst = memory.(ip) in
      match inst |> get_mode_bit with
      | _, _, _, 99 ->
          outputs
      | 0, b, c, 1 ->
          ld (ip + 1) c + ld (ip + 2) b |> st ~mode:0 (ip + 3) ;
          run_operations ~memory ~limit ~input (ip + 4) outputs
      | 0, b, c, 2 ->
          ld (ip + 1) c * ld (ip + 2) b |> st ~mode:0 (ip + 3) ;
          run_operations ~memory ~limit ~input (ip + 4) outputs
      | _, _, 0, 3 ->
          st ~mode:0 (ip + 1) input ;
          run_operations ~memory ~limit ~input (ip + 2) outputs
      | _, _, a, 4 ->
          let output = ld (ip + 1) a in
          printf "output -> %d\n" output ;
          run_operations ~memory ~limit ~input (ip + 2) (output :: outputs)
      | _ ->
          run_operations ~memory ~limit ~input (ip + 1) outputs

  let create_array input =
    String.split ~on:',' input |> List.map ~f:String.strip
    |> List.map ~f:int_of_string |> Array.of_list

  let input = None

  let part1_expected = 3412094

  let part1 input =
    let memory = create_array input in
    let outputs =
      run_operations ~memory ~limit:(Array.length memory) ~input:1 0 []
    in
    show_int_list outputs |> printf "outputs -> %s" ;
    10

  let part2_expected = 5115267

  let part2 _input = 10

  let day = 5

  let year = 2019
end
