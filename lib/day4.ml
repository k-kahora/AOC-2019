open Core

module Input = struct
  let crack_input input = String.split ~on:'-' input
end

module Day4 = struct
  (* The goal is to brute force it  *)
  (* i *)
  let input = None
  (* 128392-643281 *)

  (* let input = Some "12\n14\n1969\n100756" *)

  let part1_expected = 3412094

  let rec two_adjacent = function
    | [] ->
        false
    | a :: b :: _ when Char.( = ) a b ->
        true
    | _ :: t ->
        two_adjacent t

  let rec increasing = function
    | [] ->
        true
    | a :: b :: _rest when Char.( > ) a b ->
        false
    | _ :: rest ->
        increasing rest

  let part1 input =
    let start, stop =
      Input.crack_input input |> List.map ~f:String.strip |> Fn.flip List.take 2
      |> function
      | [a; b] ->
          (int_of_string a, int_of_string b)
      | _ ->
          failwith "2 needed minimum"
    in
    printf "start: %d, stop: %d\n" start stop ;
    let possible =
      Sequence.range ~stop:`inclusive start stop
      |> Sequence.map ~f:string_of_int
      |> Sequence.map ~f:String.to_list
    in
    let filtered =
      Sequence.filter possible ~f:increasing
      |> Sequence.filter ~f:two_adjacent
      |> Sequence.map ~f:String.of_list
    in
    filtered |> Sequence.length
  (* Sequence.iter ~f:(printf "%s\n") filtered ; *)
  (* 10 *)

  let part2_expected = 5115267

  let part2 _input = 10

  let day = 4

  let year = 2019
end
