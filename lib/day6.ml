(* Find the core node *)
(* Run a function to find the height form the core node *)
(* Store the height fo each node in a hashtable *)
(* Return the sum of each height of each node from the hashtable *)
open Core

let ( =? ) = Poly.( = )

module Input = struct
  let crack_input _input = ()
end

module Graph = struct
  type 'a graph = {edges: ('a * 'a list) list} [@@deriving show]

  let node_exists node {edges= graph} =
    List.exists graph ~f:(fun (a, _) -> Poly.( = ) a node)

  let add_node graph node neighbors =
    if node_exists node graph then graph
    else {edges= (node, neighbors) :: graph.edges}

  let empty_graph = {edges= []}

  let init graph = {edges= graph}

  let traverse_graph _graph _node = ()

  let add_edge {edges= graph} node1 node2 =
    let rec add_edge' = function
      | [] ->
          [(node1, [node2])]
      | (n, neighbors) :: rest when n =? node1 ->
          (n, node2 :: neighbors) :: rest
      | x :: rest ->
          x :: add_edge' rest
    in
    {edges= add_edge' graph}
end

module Day6 = struct
  let input = Some {|COM)B
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

  (* let input = Some "12\n14\n1969\n100756" *)
  (* let heights_table = Hashtbl.create (module String) *)

  let part1_expected = 3412094

  type string_list_list = string list list [@@deriving show]

  let crack_input input =
    String.split input ~on:'\n'
    |> List.filter ~f:(fun a -> String.( <> ) a "")
    |> List.map ~f:(String.split ~on:')')
    |> List.map ~f:(List.filter ~f:(String.( <> ) ")"))

  (* We want the height of each node in the graph.  What is the best way to ge the height of each node. *)
  (* What should the data format be  *)
  let part1 input =
    let _example_graph =
      Graph.init [('a', ['b'; 'c'; 'd']); ('b', ['d']); ('c', ['e'; 'b'])]
    in
    (* crack_input input |> show_string_list_list |> printf "\n%s" ; *)
    let _graph =
      List.fold_left (crack_input input) ~init:Graph.empty_graph
        ~f:(fun graph orbit ->
          match orbit with
          | [source; orbiter] ->
              Graph.add_edge graph source orbiter
          | _ ->
              failwith "not properly formatted" )
    in
    let graph_str =
      Format.asprintf "%a" (Graph.pp_graph Format.pp_print_string) _graph
    in
    printf "%s\n" graph_str ; 0

  let part2_expected = 5115267

  let part2 _input = 10

  let day = 6

  let year = 2019
end
