open Lwt
open Cohttp
open Cohttp_lwt_unix

let fetch_input year day =
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

(* let () = *)
(*   let year = 2024 in *)
(*   let day = 1 in *)
(*   let input = Lwt_main.run (fetch_input session_cookie year day) in *)
(*   print_endline input *)
