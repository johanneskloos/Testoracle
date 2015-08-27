open Trace
open Richtrace
open MatchTraces
open MatchTypes
open Cleantrace
open Reference
open Graph

type output_type = HTML | JSON | CSS | SVG
 
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

let collect_trace idx tree nodes =
  let rec collect_inner_node idx trace op =
    match TraceNodes.find idx nodes with
      | NodeData { op1; op2; stack } ->
        let matching = begin match op with
          | WrapperSimple | WrapperPush _ | WrapperPop | WrapperReplace _ -> Wrap op2
          | Initialization | InitializationPush _ | InitializationPop | MatchDroppable -> Init op2
          | MatchSimple | MatchPush _ | MatchPop | MatchReplace _ -> Pair (op1, op2)
        end in collect_edge idx ((idx, stack, matching) :: trace)
      | _ -> failwith "Bad tree structure"
  and collect_edge idx trace =
    match TraceTree.pred_e tree idx with
      | [] -> trace
      | [e] -> collect_inner_node (TraceTree.E.src e) trace (TraceTree.E.label e)
      | _ -> failwith "Bad tree structure"
  in collect_edge idx []

let extend_pm idx stack pm (tr1: rich_trace) (tr2: rich_trace) op =
    let split = function (x, _):: l -> (x, l) | [] -> failwith "How can this be an empty list?" in
    let (op2, tr2) = split tr2 in
    let stack' = match op with
      | WrapperPush m | MatchPush m | InitializationPush m -> m :: stack
      | WrapperPop | MatchPop | InitializationPop -> List.tl stack
      | WrapperSimple | MatchSimple | Initialization | MatchDroppable -> stack
      | MatchReplace m | WrapperReplace m -> m :: List.tl stack in 
    match op with
    | WrapperSimple | WrapperPush _ | WrapperPop | WrapperReplace _  ->
        (pm @ [idx, stack', Wrap op2], stack', tr1, tr2)
    | MatchSimple | MatchPush _ | MatchPop | MatchReplace _ ->
        let (op1, tr1) = split tr1 in
        (pm @ [idx, stack', Pair (op1, op2)], stack', tr1, tr2)
    | Initialization | InitializationPush _ | InitializationPop | MatchDroppable ->
        (pm @ [idx, stack', Init op2], stack', tr1, tr2)

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
