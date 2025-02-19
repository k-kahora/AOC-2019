open Core

module Input = struct
  let crack_input _input = ()
end

module Graph = struct
  type 'a graph = {edges: ('a * 'a list) list} [@@deriving show]

  let add_node {edges= graph} node neighbors = (node, neighbors) :: graph

  let init graph = {edges= graph}

  let add_edge {edges= graph} node1 node2 =
    let rec add_edge' = function
      | [] ->
          [(node1, [node2])]
      | (n, neighbors) :: rest when n = node1 ->
          (n, node2 :: neighbors) :: rest
      | x :: rest ->
          x :: add_edge' rest
    in
    add_edge' graph
end

module Day6 = struct
  let input = None

  (* let input = Some "12\n14\n1969\n100756" *)

  let part1_expected = 3412094

  let part1 _input =
    let _example_graph =
      Graph.init [(10, [20; 30; 40]); (20, [30]); (40, [20; 30])]
    in
    let graph_str =
      Format.asprintf "%a" (Graph.pp_graph Format.pp_print_int) _example_graph
    in
    printf "%s\n" graph_str ; 0

  let part2_expected = 5115267

  let part2 _input = 10

  let day = 6

  let year = 2019
end
