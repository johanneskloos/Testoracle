open Lwt
open Cohttp
open Cohttp_lwt_unix
open Trace
open Richtrace
open MatchTraces
open MatchTypes
open Cleantrace
open Reference
open Graph

(** Part 1: Infrastructure. *)
let output_ref (ref, ver) =
  match get_fieldref ref, get_name ref, is_global ref with
    | Some (obj, field), None, _ -> <:html<obj#$int:obj$.$str:field$@$int:ver$>>
    | None, Some name, false -> <:html<$str:name$@$int:ver$>>
    | None, Some name, true -> <:html<global:$str:name$@$int:ver$>>
    | _ -> assert false

let output_val = function
  | OUndefined -> <:html<(undefined)>>
  | ONull -> <:html<(null)>>
  | OBoolean x -> <:html<$str:string_of_bool x$>>
  | ONumberInt x -> <:html<$int:x$>>
  | ONumberFloat x -> <:html<$flo:x$>>
  | OString x -> <:html<"$str:x$">>
  | OSymbol x -> <:html<symbol:$str:x$>>
  | OObject id -> <:html<obj#$int:id$>>
  | OFunction(id, fid) -> <:html<fun#$int:id$/$int:fid$>>
  | OOther (ty, id) -> <:html<$str:ty$#$int:id$>>

let output_op =
  let output_call_type = function
    | Function -> <:html<function>>
    | Method -> <:html<method>>
    | Constructor -> <:html<constructor>>
    | ConstructorMethod -> <:html<constructor/method>>
  and output_reason = function
    | Argument i -> <:html<it binds argument $int:i$>>
    | With ref -> <:html<it aliases $output_ref ref$>>  in 
  function
  | RFunPre { f; base; args; call_type } ->
    <:html< Calling $output_call_type call_type$
       $output_val f$ on $output_val base$ with
        argument object $output_val args$
     >> 
  | RFunPost { f; base; args; result } ->
    <:html< Called $output_val f$ on $output_val base$
      with argument object $output_val args$; result is $output_val result$
    >>
  | RLiteral { value; hasGetterSetter } ->
    if hasGetterSetter then
      <:html< Literal $output_val value$ with getter and/or setter >>
    else
      <:html< Literal $output_val value$>>
  | RForIn x ->
    <:html< for/in on $output_val x$>>
  | RLocal { name; ref } ->
    <:html< Declaring local variable $str:name$,
    mapped to reference $output_ref ref$>>
  | RAlias { name; source; ref } ->
    <:html< Declaring alias $str:name$ for
    reference $output_ref ref$ because
    $output_reason source$>>
  | RRead { ref; value } ->
    <:html< Reading reference $output_ref ref$ yields $output_val value$>>
  | RWrite { ref; oldref; value; success } ->
    if success then
      <:html< Updating $output_ref oldref$ to $output_ref ref$ with $output_val value$>>
    else
      <:html< Unsuccessful update from $output_ref oldref$ to $output_ref ref$ with $output_val value$>>
  | RReturn x -> <:html<Returning $output_val x>>
  | RThrow x -> <:html<Throwing $output_val x>>
  | RWith x -> <:html<with $output_val x>>
  | RFunEnter { f; this; args } ->
    <:html<Entering $output_val f$ on $output_val this$ with argument object $output_val args$>>
  | RFunExit { ret; exc } ->
    <:html<Exiting function with return value $output_val ret$ and exception $output_val exc$>>
  | RScriptEnter -> <:html<Entering script>>
  | RScriptExit -> <:html<Exiting script>>
  | RScriptExc x -> <:html<Existing script with exception $output_val x$>>
  | RBinary { op; left; right; result } ->
    <:html<$output_val left$ $str:op$ $output_val right$ gives $output_val result$>>
  | RUnary { op; arg; result } ->
    <:html<$str:op$ $output_val arg$ gives $output_val result$>>
  | REndExpression -> <:html<(discarding result)>>
  | RConditional x -> <:html<condition value: $output_val x$>>

let output_trace tr =
  let rowwrap x = <:html< <tr><td>$x$</td></tr> >> in
  <:html<
  <table class="trace">
  $list:List.map (fun x -> x |> output_op |> rowwrap) tr$
  </table>
  >>

let output_stack st =
  let mode_to_string = function
    |  Regular -> "R"
    | Init -> "I"
    | Wrapper -> "W"
    | External -> "E"
    | ToString -> "T"
    | RegularEnter -> "r" in
  let output_mode x = <:html< $str:mode_to_string x$ >> in
   <:html<$list:List.map output_mode st$>>

type output_type = HTML | JSON | CSS | SVG

let error_page e base query =
  <:html<
  <html><body><strong>Error</strong></body></html>
  >>

let match_entry_common = function
    | Pair(op1, op2) ->
      <:html<
          <td class="original">$output_op op1$</td>
          <td class="modified">$output_op op2$</td>
          <td class="classification"></td>
      >>
    | Wrap op ->
      <:html<
          <td class="original"></td>
          <td class="modified">$output_op op$</td>
          <td class="classification">Wrap</td>
      >>
    | Init op ->
      <:html<
          <td class="original"></td>
          <td class="modified">$output_op op$</td>
          <td class="classification">Init</td>
      >> 
let match_entry e = <:html< <tr>$match_entry_common e$</tr> >>
let match_entry_ext (link: int -> string) (id, st, e) =
  <:html< <tr><td class="num"><a href="$str:link id$">$int:id$</a></td><td class="stack">$output_stack st$</td>$match_entry_common e$</tr> >>

let shared_css = (CSS, <:css<
        th { text-align: left; }
        .num { width: "7em"; }
        .stack { width: "20em"; }
        .original { width: "80em"; }
        .modified {width: "80em"; }
        .classification { width: "20em"; }
        table.trace {
          table-layout: fixed;
        }
        table.partial_trace {
          display: block;
          table-layout: fixed;
          max-height: 30vh;
          overflow-y: scroll;
          position: relative;
          bottom: 0;
        }
>> |> Cow.Css.to_string)
 
(** Part 3: Trace output. *)
module IntSig = struct
  type t = int
  let compare: t -> t -> int = compare
  let hash (x: t) = x
  let equal: int -> int -> bool = (=)
end
module MatchOp = struct
  type t = match_operation
  let compare: t -> t -> int = compare
  let default: t = MatchSimple
end;;
module TraceTree = Graph.Persistent.Digraph.ConcreteLabeled(IntSig)(MatchOp);;
module TraceNodes = Map.Make(IntSig);;

type trace_inner_node_data = {
  op1: rich_operation;
  op2: rich_operation;
  stack: match_mode list;
  trace_trace: ((match_condition * MatchOperations.mismatch) list * match_operation) list
  }

type trace_node =
    FinalNodeData of trace_inner_node_data
  | NodeData of trace_inner_node_data
  | EndtraceData of rich_operation list
  | InitTailtraceData of rich_operation list * match_mode list
  | SuccessNode
  | BlockedData of int * int * match_mode list

type trace_data = { tree: TraceTree.t; nodes: trace_node TraceNodes.t }
  
let trace_main_page_trace self rtrace =
  let rec map_with_commas fmt = function
    | [] -> <:html< (empty)>>
    | [x] -> fmt x
    | x::l -> <:html<$fmt x$, $map_with_commas fmt l$>>
  and fmt_node_link node =
    let node' = string_of_int node in
    <:html< <a href="$str:self [("operation", "details"); ("index", node')]$">$str:node'$</a> >> in
  let fmt_node (ev, nodes) =
    <:html<
     <tr><td>$output_op ev$</td><td>$map_with_commas fmt_node_link (List.rev nodes)$</td></tr>
    >> in
  <:html<
  <table>
  <tr><th>Event</th><th>Encountered in</th></tr>
  $list:List.map fmt_node rtrace$
  </table> >>

let tree_visitor which fop { tree; nodes } =
  let rec find_follower_nodes v =
    TraceTree.fold_succ_e (fun e next ->
      let v' = TraceTree.E.dst e in
      if fop (TraceTree.E.label e) then
        v' :: next
      else
        find_follower_nodes v' @ next)
      tree v [] in
  let find_ops =
    List.fold_left (fun (maybe1, maybe2) v -> match TraceNodes.find v nodes with
      | FinalNodeData { op1; op2 } -> (Some op1, Some op2)
      | NodeData { op1; op2 } -> (Some op1, Some op2)
      | EndtraceData (op1 :: _) -> (Some op1, maybe2)
      | InitTailtraceData (op2 :: _, _) -> (maybe1, Some op2)
      | _ -> (maybe1, maybe2)) (None, None) in 
  let rec bfs layer =
    match which (find_ops layer) with
      | Some op ->
        let layer' = List.map find_follower_nodes layer |> List.flatten in
        (op, layer) :: bfs layer'
      | None -> []
  in bfs [0]
        
let reconstruct_first data =
  tree_visitor fst
   (function MatchSimple | MatchPush _ | MatchPop -> true | _ -> false)
   data
let reconstruct_second data = tree_visitor snd (fun _ -> true) data

let trace_main_page self base data =
  <:html< <html><head><title>Trace for $str:base$</title></head>
  <body>
  <a href="#trace2">To second trace</a>
  <h1><a id="trace1">Original trace</a></h1>
  $trace_main_page_trace self (reconstruct_first data)$
  <a href="#trace1">To first trace</a>
  <h1><a id="trace2">Modified trace</a></h1>
  $trace_main_page_trace self (reconstruct_second data)$  
  </body></html> >>

let trace_treedata { tree }: Yojson.Basic.json =
  let open Yojson.Basic in
  let out_label = Misc.to_string pp_match_operation in 
  let out_edge edge: json =
    `Assoc [ ("op", `String (TraceTree.E.label edge |> out_label)); ("node", `Int (TraceTree.E.dst edge)) ] in
  let out_succ_edges v: string * json =
    (string_of_int v, `List (TraceTree.fold_succ_e (fun edge lout -> out_edge edge :: lout) tree v [])) in
  `Assoc (TraceTree.fold_vertex (fun vertex l -> out_succ_edges vertex :: l) tree [])

let trace_details_summary = function
  | FinalNodeData _ -> <:html< <strong>No extensions at inner node available!</strong> >>
  | NodeData _ -> <:html< Regular inner node >>
  | EndtraceData _ -> <:html< <strong>Transformed trace has ended before original trace was exhausted!</strong> >>
  | InitTailtraceData _ -> <:html< <strong>Tail of transformed code cannot be classified as init code!</strong> >>
  | SuccessNode -> <:html< <strong>Match successful</strong> >>
  | BlockedData _ -> <:html< <strong>Shared block</strong> >> 

let output_mode m = <:html< $str:Misc.to_string pp_match_mode m$>>
 
let output_matchop = function
    | MatchSimple -> <:html< Simple match>>
    | MatchPush m -> <:html< Match and push $output_mode m$>>
    | MatchReplace m -> <:html< Match and replace $output_mode m$>>
    | MatchPop -> <:html<Match and pop>>
    | WrapperSimple -> <:html< Simple wrap >>
    | WrapperPush m -> <:html< Wrap and push $output_mode m$>>
    | WrapperPop -> <:html< Wrap and pop >>
    | Initialization -> <:html< Simple init >>
    | InitializationPush m -> <:html< Init and push $output_mode m$>>
    | InitializationPop -> <:html< Init and pop >>

let trace_details_reasons { op1; op2; stack; trace_trace } =
  let open MatchObjects in let open MatchTypes in let open MatchOperations in
  let output_path = function
    | path0 :: path -> List.fold_left (fun x y -> x ^ "," ^ y) path0 path
    | [] -> "the top"
  and output_cond = function
    | MatchSides -> <:html<$output_op op1$ and $output_op op2$ don't match>>
    | MayMatchSimple -> <:html<$output_op op2$ not admissible for a simple match>>
    | MatchCallInt -> <:html<$output_op op1$ and $output_op op2$  not matching internal calls>>
    | MatchCallExt -> <:html<$output_op op1$ and $output_op op2$ not matching external calls>>
    | MatchCallToString -> <:html<$output_op op1$ and $output_op op2$ not matching toString calls>>
    | MatchCallWrap -> <:html<$output_op op2$ cannot possible be a wrapper call>>
    | MayInit -> <:html<$output_op op2$ may not be inserted in simple init code>>
    | IsToplevel -> <:html<$output_op op2$ may not occur at the top level>>
    | IsNotFunction -> <:html<$output_op op2$ is not a call or return>>
    | IsExit -> <:html<$output_op op2$ is not a return>>
    | IsCallInt -> <:html<$output_op op2$ is not an internal call>>
    | IsUnobservable -> <:html<$output_op op2$ is an observable action>>
    | MayInsertInWrapSimple -> <:html<$output_op op2$ may not occur in simple wrapper code>>
    | IsPostExit -> <:html<$output_op op2$ is not a post-exit>>
    | IsEnter -> <:html<$output_op op2$ is not an entry>>
     in
  let output_obj_match_trace = function
    | NonMatching (path, val1, val2) ->
      <:html<At $str:output_path path$, $output_val val1$ does not match $output_val val2$>>
    | MissingOrig (fld, path) -> <:html< Missing field $str:fld$ at $str:output_path path$ in the set of objects for the original trace. >>
    | MissingXfrm (fld, path) -> <:html< Missing field $str:fld$ at $str:output_path path$ in the set of objects for the transformed trace. >>
    | Other reason -> <:html< $str:reason$ >> in
  let output_reason = function
    | DifferentType -> <:html<Different types>>
    | DifferentObjects (where, failure) ->
      <:html<Objects in $str:where$ don't match: $output_obj_match_trace failure$>>
    | DifferentArguments -> <:html<Referencing different arguments>>
    | DifferentValues name -> <:html<Values for $str:name$ differ>>
    | DifferentOperations -> <:html<Comparing incomparable events>>
    | OtherOperation -> <:html<Event of a non-appropriate type>>
    | NotToString -> <:html<Not a call to toString>>
    | NotInitData -> <:html<Variable does not contain init data>>
    | NotSimpleMatchable -> <:html<Cannot be matched by simple matching>>
    | NotWrapCode -> <:html<Cannot occur in wrapper code>>
    | NotToStringCode -> <:html<Cannot occur in toString code>>
    | ExternalCall -> <:html<The call is external>> 
    | InternalCall -> <:html<The call is internal>>
    | NotLiterallyEqual -> <:html<The function bodies are not literally equal>> 
    | LiterallyEqual -> <:html<The function bodies are literally equal>>
    | NotToplevel -> <:html<Not top level code>>
    | NotFunction -> <:html<Not a function entry>>
    | NotExit -> <:html<Not a function exit>>
    | Observable -> <:html<Event is observable>>
    | NotAtToplevel -> <:html<Not at top level>>
    | NotFunctionUpdate -> <:html<Not a function update>>
    | NotInitCode -> <:html<Not init code>>
    | NotEnter -> <:html<Not a function entry>> in
  let output_cond (cond, reason) =
    <:html<$output_cond cond$: $output_reason reason$>> in
  let output_nonempty = function
    | [] -> <:html<Non-empty list???>>
    | [x] -> <:html<$x$>>
    | l -> let l' = List.map (fun x -> <:html< <li>$x$</li> >>) l in
      <:html< <ul> $list:l'$ </ul> >> in
  let output_trace_entry (conds, op) =
    <:html<
      <dt>$output_matchop op$</dt>
      <dd>$output_nonempty (List.map output_cond conds)$</dd>
    >> in
  <:html<
     <p>Operations being matched: $output_op op1$ versus $output_op op2$</p>
     <p>Current stack: $output_stack stack$</p>
     <p>Failed matching rules:</p>
     <dl>
     $list:List.map output_trace_entry trace_trace$
     </dl>
    >>

(* ((match_condition * MatchOperations.mismatch) list * match_operation) *)

let trace_details_cases self tree idx =
  let output_follower e =
    <:html<
      <li><a href="$str:self [("operation", "details"); ("index", string_of_int (TraceTree.E.dst e))]$">$output_matchop (TraceTree.E.label e)$</a></li>
    >>
  in function
  | FinalNodeData data ->
    <:html< <h2>Reasons why no more matching is possible:</h2>$trace_details_reasons data$ >>
  | NodeData data ->
    <:html< <h2>Follower nodes</h2>
      <ul>$list:TraceTree.fold_succ_e (fun e l -> output_follower e :: l) tree idx []$</ul> 
      <h2>Reasons why other matchings have been ruled out:</h2>$trace_details_reasons data$ >>
  | EndtraceData tr ->
    <:html< <h2>Leftover trace</h2>$output_trace tr$>>
  |  InitTailtraceData (tr, st) ->
    <:html<
      <h2>Leftover trace</h2>$output_trace tr$
      <h2>Stack</h2>$output_stack st$
    >>
  | SuccessNode ->
    <:html< Match successful! >>
  | BlockedData (len1, len2, stack) ->
    <:html< Blocked: A suffix pair of lengths $int:len1$, $int:len2$ is known not to match for stack $output_stack stack$>> 

let collect_trace idx tree nodes =
  let rec collect_inner_node idx trace op =
    match TraceNodes.find idx nodes with
      | NodeData { op1; op2; stack } ->
        let matching = begin match op with
          | WrapperSimple | WrapperPush _ | WrapperPop -> Wrap op2
          | Initialization | InitializationPush _ | InitializationPop -> Init op2
          | MatchSimple | MatchPush _ | MatchPop | MatchReplace _ -> Pair (op1, op2)
        end in collect_edge idx ((idx, stack, matching) :: trace)
      | _ -> failwith "Bad tree structure"
  and collect_edge idx trace =
    match TraceTree.pred_e tree idx with
      | [] -> trace
      | [e] -> collect_inner_node (TraceTree.E.src e) trace (TraceTree.E.label e)
      | _ -> failwith "Bad tree structure"
  in collect_edge idx []

let trace_details self base { tree; nodes } idx =
  let node_class = TraceNodes.find idx nodes
  and link i = self [("operation", "details"); ("index", string_of_int i)] in
  let output_matchop = function
    | Pair(op1, op2) ->
      <:html< <td>$output_op op1$</td><td>$output_op op2$</td><td></td> >>
    | Init op ->
      <:html< <td></td><td>$output_op op$</td><td>Init</td> >>
    | Wrap op ->
      <:html< <td></td><td>$output_op op$</td><td>Wrap</td> >> in
  let build_partial_trace idx =
    collect_trace idx tree nodes
    |> List.map (fun (idx, stack, matchop) -> 
      <:html< <tr>
        <td><a href="$str:link idx$">$int:idx$</a></td>
        <td>$output_stack stack$</td>
        $output_matchop matchop$
        </tr> >>) in
  <:html<
  <html>
    <head>
      <title>Details for $str:base$, node $int:idx$</title>
      <link href="stylesheet.css" rel="stylesheet"/>
    </head>
    <body>
      <h1>Details for $str:base$, node $int:idx$</h1>
      <p>Executive summary: $trace_details_summary node_class$</p>
      <h2>Partial matching</h2>
      <table class="partial_trace">
        <tr>
          <th class="num"></th>
          <th class="stack">Stack</th>
          <th class="original">Original trace</th>
          <th class="modified">Modified trace</th>
          <th class="classification">Classification</th>
        </tr>
        $list:build_partial_trace idx$
      </table>
      $trace_details_cases self tree idx node_class$
    </body>
  </html> >>

let extend_pm idx stack pm (tr1: rich_trace) (tr2: rich_trace) op =
    let split = function (x, _):: l -> (x, l) | [] -> failwith "How can this be an empty list?" in
    let (op2, tr2) = split tr2 in
    let stack' = match op with
      | WrapperPush m | MatchPush m | InitializationPush m -> m :: stack
      | WrapperPop | MatchPop | InitializationPop -> List.tl stack
      | WrapperSimple | MatchSimple | Initialization -> stack
      | MatchReplace m -> m :: List.tl stack in 
    match op with
    | WrapperSimple | WrapperPush _ | WrapperPop ->
        (pm @ [idx, stack', Wrap op2], stack', tr1, tr2)
    | MatchSimple | MatchPush _ | MatchPop | MatchReplace _ ->
        let (op1, tr1) = split tr1 in
        (pm @ [idx, stack', Pair (op1, op2)], stack', tr1, tr2)
    | Initialization | InitializationPush _ | InitializationPop ->
        (pm @ [idx, stack', Init op2], stack', tr1, tr2)

let write_dot chan { tree; nodes } =
  let open Format in
  let fmt = formatter_of_out_channel chan in
  pp_open_vbox fmt 2;
  pp_print_string fmt "digraph searchtree {";
  pp_print_cut fmt ();
  TraceTree.iter_vertex (fun v ->
    match TraceNodes.find v nodes with
      | FinalNodeData _ ->
        fprintf fmt
          "v%d [label=\"%d\",shape=rectangle,fillstyle=filled,fillcolor=red]@ "
          v v
      | NodeData _ ->
        fprintf fmt "v%d [label=\"%d\",shape=ellipse]" v v
      | EndtraceData _ ->
        fprintf fmt
          "v%d [label=\"%d\",shape=rectangle,fillstyle=filled,fillcolor=yellow]@ "
          v v
      | InitTailtraceData _ ->
        fprintf fmt
          "v%d [label=\"%d\",shape=rectangle,fillstyle=filled,fillcolor=orange]@ "
          v v
      | SuccessNode ->
        fprintf fmt
          "v%d [label=\"%d\",shape=ellipse,fillstyle=filled,fillcolor=green]@ "
          v v
      | BlockedData(l1, l2, _) ->
        fprintf fmt
          "v%d [label=\"%d: %d,%d\",shape=rectangle,fillstyle=filled,fillcolor=blue]@ "
          v v l1 l2) tree;
  let pp_short_mode pp = function
    | Regular -> pp_print_string pp "R"
    | RegularEnter -> pp_print_string pp "r"
    | Wrapper -> pp_print_string pp "W"
    | External -> pp_print_string pp "E"
    | ToString -> pp_print_string pp "T"
    | Init -> pp_print_string pp "I" in
  let pp_short_op pp = function
    | WrapperSimple -> pp_print_string pp "W"
    | WrapperPush m -> fprintf pp "W, push %a" pp_short_mode m
    | WrapperPop -> pp_print_string pp "W, pop"
    | Initialization -> pp_print_string pp "I"
    | InitializationPush m -> fprintf pp "I, push %a" pp_short_mode m
    | InitializationPop -> pp_print_string pp "I, pop"
    | MatchSimple -> pp_print_string pp "M"
    | MatchPush m -> fprintf pp "M, push %a" pp_short_mode m
    | MatchReplace m -> fprintf pp "M, replace %a" pp_short_mode m
    | MatchPop -> pp_print_string pp "M, pop" in
    TraceTree.iter_edges_e (fun e ->
    let (src, dst, op) = (TraceTree.E.src e, TraceTree.E.dst e, TraceTree.E.label e) in
    fprintf fmt "%d -> %d [label=\"%a\"]@ " src dst pp_short_op op) tree;
  pp_close_box fmt ();
  pp_print_string fmt "}";
  Format.pp_print_flush fmt ()
  
let generate_treesvg filename data =
  let open Format in
  let outchan = open_out filename
  and (dot_in, dot_out) = Unix.open_process "/usr/bin/dot -T svg" in
  write_dot dot_out data;
  flush dot_out;
  let buf = Buffer.create 1048576 in
  let rec copy () =
    try
      let line = input_line dot_in in
      output_string outchan line;
      Buffer.add_string buf line;
      copy ()
    with End_of_file -> () in
  copy ();
  Unix.close_process (dot_in, dot_out) |> ignore;
  close_out outchan;
  Buffer.contents buf

let trace_treesvg base data =
  let filename = "."^ base ^ ".svg" in
  if not (Sys.file_exists filename) then
     generate_treesvg filename data
  else
    let chan = open_in filename in
    let svg = really_input_string chan (in_channel_length chan) in
    close_in chan; svg
    
let trace_multiplex self base data query =
  match query "operation" with
    | Some "treedata" -> (JSON, trace_treedata data |> Yojson.Basic.to_string)
    | Some "treesvg" -> (SVG, trace_treesvg base data)
    | Some "details" ->
        begin match query "index" with
          | Some idx -> (HTML, trace_details self base data (int_of_string idx) |> Cow.Html.to_string)
          | None -> raise (Invalid_argument "Needs exactly one index")
        end
    | None ->  (HTML, trace_main_page self base data |> Cow.Html.to_string)
    | _ -> raise (Invalid_argument "Unknown operation given")

let extract_data data =
  List.fold_left (fun { tree; nodes } -> function
      | MatchTracesObserver.RNode (id, op1, op2, stack) ->
        { tree = TraceTree.add_vertex tree id;
          nodes = TraceNodes.add id (FinalNodeData { op1; op2; stack; trace_trace = [] }) nodes }
      | MatchTracesObserver.REdge (src, tgt, op) ->
        { tree = TraceTree.add_edge_e tree (TraceTree.E.create src op tgt);
          nodes =
            let data = match TraceNodes.find src nodes with
              | FinalNodeData data ->  NodeData data
              | NodeData data ->  NodeData data
              | _ -> failwith "Inconsistent trace tree"
            in TraceNodes.add src data nodes }
      | MatchTracesObserver.RFailure (id, fail) ->
        { tree;
          nodes =
            let data = match TraceNodes.find id nodes with
              | FinalNodeData ({ trace_trace = [] } as node_class) ->
                  FinalNodeData { node_class with trace_trace = fail }
              | NodeData ({ trace_trace = [] } as node_class) ->
                  NodeData { node_class with trace_trace = fail }
              | _ -> failwith "Inconsistent trace tree"
            in TraceNodes.add id data nodes }
      | MatchTracesObserver.ROrigConsumedOk (id, trace, stack) ->
        { tree; nodes = TraceNodes.add id (SuccessNode) nodes }
      | MatchTracesObserver.ROrigConsumedFailure (id, trace ,stack) ->
        { tree; nodes = TraceNodes.add id (InitTailtraceData(trace, stack)) nodes }
      | MatchTracesObserver.RXfrmConsumed (id, trace) ->
        { tree; nodes = TraceNodes.add id (EndtraceData trace) nodes }
      | MatchTracesObserver.RBlockedShared (id, len1, len2, stack) ->
        { tree; nodes = TraceNodes.add id (BlockedData (len1, len2, stack)) nodes })
      { tree = TraceTree.empty; nodes = TraceNodes.empty } data
                    
          
(** Part 4: The server. *)
let read_result key =
  let data = MatchTracesObserver.read ("." ^ key ^ ".cert") in
  extract_data data

let get_certs () =
  let rec getdir handle list =
    try
      getdir handle (Unix.readdir handle :: list)
    with End_of_file ->
      Unix.closedir handle; list
  in
  getdir (Unix.opendir ".") []
  |> List.filter (fun name -> Filename.check_suffix name ".cert")
  |> List.map (fun name -> Filename.chop_suffix name ".cert")  
  |> List.sort String.compare
  
let list_certs self =
	  let print_result name =
			try
				let chan = open_in (name ^ ".result") in
				let res = int_of_string (input_line chan) in
				close_in chan;
				match res with
				| 0 -> <:html<(match)>>
				| 1 -> <:html<(no match)>>
				| 2 -> <:html<(failed>>
				| _ -> <:html<(unknown result $int:res$)>>
		  with _ -> <:html<(can't read state)>> in
    let link_cert name =
      <:html< <a href="$str:self name$">$str:name$</a> $print_result name$<br/> >>
    in
    (HTML, <:html<
    <html><head><title>List of certificates</title></head>
    <body>$list:List.map link_cert (get_certs ())$</body></html> >> |> Cow.Html.to_string)
    
let good_path = Str.regexp "^/[^/]*$"
let bad_path path =
  (HTML, <:html<
  <html>
    <head><title>Invalid request</title></head>
    <body>
    Request for invalid reqsource $str:path$
    </body>
  </html>
  >> |> Cow.Html.to_string)
  
let server_callback cache conn req body =
    let uri = req |> Request.uri in
    Format.eprintf "Handling %s@." (Uri.to_string uri);
    let path = Uri.path uri
    and query key = Uri.get_query_param uri key
    and self query' = Uri.with_query' uri query' |> Uri.to_string
    and page base = Uri.with_path uri base |> Uri.to_string in
    begin
        match path with
        | "/stylesheet.css" -> shared_css
        | "" | "/" -> list_certs page
        | _ when Str.string_match good_path path 0 ->
            let data = cache path in
            trace_multiplex self path data query
        | _ -> bad_path path
    end |> begin function
        | (HTML, body) -> ("text/html", body)
        | (JSON, body) -> ("application/json", body)
        | (CSS, body) -> ("text/css", body)
        | (SVG, body) -> ("image/svg+xml", body)
    end |> fun (ctype, body) ->
        Server.respond_string ~status:`OK ~headers: (Header.init_with "Content-type" ctype) ~body ()

let () =
  Format.eprintf "Starting...@.";
  let cache = BatCache.lru_cache ~gen:read_result ~cap:10 in 
  Server.create (Server.make ~callback:(server_callback cache) ())
  |> Lwt_main.run
