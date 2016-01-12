open CertifierData

let short_op_string =
  let open MatchTypes in function
    | MatchSimple -> "S"
    | MatchPush _ -> "S+"
    | MatchPop -> "S-"
    | WrapperSimple -> "W"
    | WrapperPush _ -> "W+"
    | WrapperPop -> "W-"
    | Initialization -> "I"
    | InitializationPush _ -> "I+"
    | InitializationPop -> "I-"
    | MatchDroppable -> "D"
    | MatchReplace _ -> "S="
    | WrapperReplace _ -> "W="

let create_dot_graph { tree = tree; nodes = nodes } base channel =
  let module AttrGraph = struct
    include CertifierData.TraceTree

    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let default_edge_attributes _ = []
    let get_subgraph _ = None

    let vertex_name v = "n" ^ (string_of_int v)
    let vertex_attributes v =
      let open Graph.Graphviz.DotAttributes in
      [ `Url (base v);
        `Label (string_of_int v)  ]
    let edge_attributes e =
      let open Graph.Graphviz.DotAttributes in
      [ `Label (short_op_string (E.label e)) ]
  end in
  let module DOTWriter = Graph.Graphviz.Dot(AttrGraph) in
  DOTWriter.output_graph channel tree

let spawn_format_task chan_dot name_png =
  let open Lwt_process in 
  exec ~stdin:(`FD_move chan_dot) ("", [| "dot"; "-T"; "png"; "-o"; name_png |])

let format_task name_png base data =
  let (readend, writeend) = Unix.pipe () in
  let handle = spawn_format_task readend name_png in
  Unix.close readend;
  create_dot_graph data base (Unix.out_channel_of_descr writeend);
  Unix.close writeend;
  handle

let format name_png base data =
  try
    Unix.access name_png [Unix.R_OK];
    Lwt.return name_png
  with _ ->
    format_task name_png base data
    |> Lwt.map (fun _ -> name_png)