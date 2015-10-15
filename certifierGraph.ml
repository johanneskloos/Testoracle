open CertifierData

module DOTGraph = struct
  module V = struct
    type t = { id: int; name: string; url: string }
  end;;
  module E = struct
    type t = { tsrc: int; tdst: int; op: string }
    let src { tsrc = src } = src
    let dst { tdst = dst } = dst
  end;;
  
  type t = TraceTree.t * TraceNodes.t * (int -> string)
  
  let iter_vertex f (graph, data, base) =
    TraceTree.iter_vertex (fun v ->
      f { id = v; name = "n" ^ (string_of_int v); url = base v })
      graph
  
  let iter_edges_e f (graph, data, base) = let open TraceTree in
    iter_edges_e (fun e ->
      f { src = E.src e; dst = E.dst e; op = short_op_string (E.label e) })
      graph
  
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  
  let vertex_name { name } = name
  let vertex_attributes { id; url } = let open Graph.Graphviz.DotAttributes in
    [ `Url url; `Label = string_of_int id  ]
  let edge_attributes { op } = let open Graph.Graphviz.DotAttributes in [ `Label op ]
end;;

module DOTWriter = Graphviz.Dot(DOTGraph)

let create_dot_graph { tree = tree; nodes = nodes } base channel =
  DOTWriter.output_graph channel (tree, nodes, base)

let spawn_format_task chan_dot name_png =
  let open Lwt_process in 
  exec ~stdin:chan_dot ("", [| "dot"; "-T"; "png"; "-o"; name_png |])

let format_task name_png base data =
  let (readend, writeend) = Unix.pipe () in
  let handle = spawn_format_task readend name_png in
  Unix.close readend;
  create_dot_graph data base (Unix.out_channel_of_descr writeend);
  Unix.close writeend;
  handle

let format name_png base data =
  try Unix.access name_png 