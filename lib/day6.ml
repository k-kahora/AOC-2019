open Core

let ( =? ) = Poly.( = )

module Input = struct
  let crack_input _input = ()
end

module Day6 = struct
  (* let input = Some "12\n14\n1969\n100756" *)
  let part1_expected = 417916

  let input = None

  (*   let input = *)
  (*     Some {| *)
(* COM)B *)
(* B)C *)
(* C)D *)
(* D)E *)
(* E)F *)
(* B)G *)
(* G)H *)
(* D)I *)
(* E)J *)
(* J)K *)
(* K)L *)
(* K)YOU *)
(* I)SAN *)
(* |} *)

  (*   let part1_expected = 54 *)

  (* let part1_expected = 42 *)
  type string_int_list = (string * int) list [@@deriving show]

  type hash_alist = (string * string list) list [@@deriving show]

  let hashtbl_to_alist (ht : (string, string list) Hashtbl.t) :
      (string * string list) list =
    Hashtbl.to_alist ht

  let queue_to_list q = Queue.to_list q

  (* BFS will give distance from root *)
  let compute_orbits graph =
    let rec bfs q total_orbits =
      match q with
      | (node, depth) :: rest ->
          let children = Hashtbl.find graph node |> Option.value ~default:[] in
          let updated_q =
            rest @ List.map children ~f:(fun a -> (a, depth + 1))
          in
          bfs updated_q (total_orbits + depth)
      | [] ->
          total_orbits
    in
    bfs [("COM", 0)] 0

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
    (* (\* show_hash_alist (hashtbl_to_alist graph) |> printf "hash -> %s" ; *\) *)
    (* printf "result -> %d" (copute_orbits graph) ; *)
    compute_orbits graph

  let part2_expected = 523

  (* Need to do a bfs on a non directional graph *)
  (* This is going to be a bfs with a visited as well as a total accum *)

  let compute_closest graph (you : string * int) (san : string) =
    let visited = Hashtbl.create (module String) in
    let queue = Queue.create () in
    Queue.enqueue queue you ;
    let rec bfs () =
      (* printf "q -> %s\n" (show_string_int_list (queue_to_list queue)) ; *)
      if Queue.is_empty queue then None
      else
        let node, depth = Queue.dequeue_exn queue in
        if String.( = ) node san then Some depth
        else if Hashtbl.mem visited node then bfs ()
        else (
          Hashtbl.set visited ~key:node ~data:true ;
          let neighbors = Hashtbl.find graph node |> Option.value ~default:[] in
          List.iter neighbors ~f:(fun neighbor ->
              if not (Hashtbl.mem visited neighbor) then
                Queue.enqueue queue (neighbor, depth + 1) ) ;
          bfs () )
    in
    bfs ()

  let part2 input =
    let graph = Hashtbl.create (module String) in
    let update_hash key value =
      Hashtbl.update graph key ~f:(function
        | None ->
            [value]
        | Some values ->
            value :: values )
    in
    List.iter
      (String.split ~on:'\n' input |> List.filter ~f:(String.( <> ) ""))
      ~f:(fun a ->
        match String.split ~on:')' a with
        | [orbit; orbitter] ->
            update_hash orbit orbitter ; update_hash orbitter orbit
        | _ ->
            failwith "invalid input" ) ;
    (* show_hash_alist (hashtbl_to_alist graph) |> printf "hash -> %s" ; *)
    printf "result -> %d"
      (compute_closest graph ("YOU", 0) "SAN" |> Option.value ~default:(-1)) ;
    10

  let day = 6

  let year = 2019
end
