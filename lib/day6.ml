open Core

let ( =? ) = Poly.( = )

module Input = struct
  let crack_input _input = ()
end

module Day6 = struct
  (* let input = Some "12\n14\n1969\n100756" *)
  let input = Some {|
COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
|}

  let part1_expected = 10

  type hash_alist = (string * string list) list [@@deriving show]

  let hashtbl_to_alist (ht : (string, string list) Hashtbl.t) :
      (string * string list) list =
    Hashtbl.to_alist ht

  let part1 input =
    let graph = Hashtbl.create (module String) in
    List.iter
      (String.split ~on:'\n' input |> List.filter ~f:(Poly.( <> ) ""))
      ~f:(fun mapping ->
        match String.split ~on:')' mapping with
        | [orbit; orbitter] ->
            Hashtbl.update graph orbit ~f:(function
              | None ->
                  [orbitter]
              | Some children ->
                  orbitter :: children )
        | _ ->
            failwith "Should not trigger" ) ;
    show_hash_alist (hashtbl_to_alist graph) |> printf "hash -> %s" ;
    10

  let part2_expected = 5115267

  let part2 _input = 10

  let day = 6

  let year = 2019
end
