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

  let part1_expected = 2050

  (* get three elements and check that 2 are the same an not the third  *)
  (* if three are the same just chop off that group *)
  (* This skips when there is exactly 2 numbers in the list at the very end *)
  let rec exactly_two_next = function
    | [] ->
        false
    (* Why is this line needed *)
    | [a; b] when Char.( = ) a b ->
        true
    | a :: b :: c :: _ when Char.( = ) a b && Char.( <> ) a c ->
        true
    (* a b and c are equal *)
    | a :: b :: c :: _ as tail when Char.( = ) a b && Char.( = ) b c ->
        List.drop_while ~f:(fun d -> Char.( = ) d c) tail |> exactly_two_next
    | _ :: tail ->
        exactly_two_next tail

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
  (* 10 *)

  let part2_expected = 1390

  let part2 input =
    let start, stop =
      Input.crack_input input |> List.map ~f:String.strip |> Fn.flip List.take 2
      |> function
      | [a; b] ->
          (int_of_string a, int_of_string b)
      | _ ->
          failwith "2 needed minimum"
    in
    let possible =
      Sequence.range ~stop:`inclusive start stop
      |> Sequence.map ~f:string_of_int
      |> Sequence.map ~f:String.to_list
    in
    let filtered =
      Sequence.filter possible ~f:increasing
      |> Sequence.filter ~f:exactly_two_next
      |> Sequence.map ~f:String.of_list
    in
    (* Sequence.iter ~f:(printf "%s\n") filtered ; *)
    filtered |> Sequence.length

  let day = 4

  let year = 2019
end
