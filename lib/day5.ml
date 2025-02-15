open Core

(* 1 -> negative numbers *)
(* 2 -> a instruction pointer *)
(* 3 -> Some instructioins have a set number of params they can take *)
module Day5 = struct
  (* Required feilds *)
  type program = int array [@@deriving show]

  type int_list = int array [@@deriving show]

  type string_list = string list [@@deriving show]

  let store ~memory ~mode ~ip value =
    match mode with
    | 1 ->
        memory.(ip) <- value
    | 0 ->
        memory.(memory.(ip)) <- value
    | _ ->
        failwith "non recognized mode bit"

  let load ~memory ~ip = function
    | 1 ->
        memory.(ip)
    | 0 ->
        memory.(memory.(ip))
    | _ ->
        failwith "unrecognized mode bit"

  let get_mode_bit n =
    (n / 10000 mod 10, n / 1000 mod 10, n / 100 mod 10, n mod 100)

  let rec run_operations ~memory ~limit ~input ip outputs =
    let ld = load ~memory in
    let st = store ~memory in
    if ip >= limit then -1
    else
      let inst = memory.(ip) in
      match inst |> get_mode_bit with
      | 0, b, c, 1 ->
          ld ~ip:(ip + 1) c + ld ~ip:(ip + 2) b |> st ~mode:0 ~ip:(ip + 3) ;
          run_operations ~memory ~limit ~input (ip + 4) outputs
      | 0, b, c, 2 ->
          ld ~ip:(ip + 1) c * ld ~ip:(ip + 2) b |> st ~mode:0 ~ip:(ip + 3) ;
          run_operations ~memory ~limit ~input (ip + 4) outputs
      | _, b, c, 5 ->
          if ld ~ip:(ip + 1) c <> 0 then
            run_operations ~memory ~limit ~input (ld ~ip:(ip + 2) b) outputs
          else run_operations ~memory ~limit ~input (ip + 3) outputs
      | _, b, c, 6 ->
          if ld ~ip:(ip + 1) c = 0 then
            run_operations ~memory ~limit ~input (ld ~ip:(ip + 2) b) outputs
          else run_operations ~memory ~limit ~input (ip + 3) outputs
      | a, b, c, 7 ->
          if ld ~ip:(ip + 1) c < ld ~ip:(ip + 2) b then
            st ~mode:a ~ip:(ip + 3) 1
          else st ~mode:a ~ip:(ip + 3) 0 ;
          run_operations ~memory ~limit ~input (ip + 4) outputs
      | a, b, c, 8 ->
          if ld ~ip:(ip + 1) c = ld ~ip:(ip + 2) b then
            st ~mode:a ~ip:(ip + 3) 1
          else st ~mode:a ~ip:(ip + 3) 0 ;
          run_operations ~memory ~limit ~input (ip + 4) outputs
      | _, _, 0, 3 ->
          st ~mode:0 ~ip:(ip + 1) input ;
          run_operations ~memory ~limit ~input (ip + 2) outputs
      | _, _, a, 4 ->
          let output = ld ~ip:(ip + 1) a in
          run_operations ~memory ~limit ~input (ip + 2) (output :: outputs)
      | _, _, _, 99 ->
          outputs |> List.hd |> Option.value ~default:(-1)
      | _ ->
          run_operations ~memory ~limit ~input (ip + 1) outputs

  let create_array input =
    String.split ~on:',' input |> List.map ~f:String.strip
    |> List.map ~f:int_of_string |> Array.of_list

  let input = None
  (* let input = *)
  (*   Some *)
  (*     "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,\n\ *)
  (*      1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,\n\ *)
  (*      999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99" *)

  let part1_expected = 10987514

  let part1 input =
    let memory = create_array input in
    run_operations ~memory ~limit:(Array.length memory) ~input:1 0 []

  let part2_expected = 14195011

  let part2 input =
    let memory = create_array input in
    run_operations ~memory ~limit:(Array.length memory) ~input:5 0 []

  let day = 5

  let year = 2019
end
