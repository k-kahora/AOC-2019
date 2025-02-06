open Alcotest

let fetch_input year day =
  let open Lwt in
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let fetch_input_cookie session_cookie year day =
    let uri =
      Uri.of_string
        (Printf.sprintf "https://adventofcode.com/%d/day/%d/input" year day)
    in
    let headers = Header.init_with "Cookie" ("session=" ^ session_cookie) in
    Client.get ~headers uri >>= fun (_, body) -> Cohttp_lwt.Body.to_string body
  in
  let session_cookie =
    "53616c7465645f5f6a80591ce57c9834ada49842ec40afbd44bba9de4fb052148f9f2d38f717608843e7387770aefd769f17b4079d6d66fb9ea7022691c73239"
  in
  Lwt_main.run @@ fetch_input_cookie session_cookie year day

(* You give it the year and day as well as expected output *)
module type Day = sig
  val input : string option

  val part1 : string -> int

  val part2 : string -> int

  val day : int

  val year : int

  val part1_expected : int

  val part2_expected : int
end

(* If D.day is None call with day and year if it is something then run the input *)
module Test (D : Day) = struct
  let test_input input =
    let p1, p2 = (D.part1 input, D.part2 input) in
    let test_string day part =
      Format.sprintf "Testing day %d part%d" day part
    in
    check int (test_string D.day 1) D.part1_expected p1 ;
    check int (test_string D.day 2) D.part2_expected p2

  let test () =
    match D.input with
    | None ->
        fetch_input D.year D.day |> test_input
    | Some rolled_input ->
        rolled_input |> test_input

  let run () =
    let open Alcotest in
    run "Advent" [("Day1", [test_case "part1 and 2" `Quick test])]
end
