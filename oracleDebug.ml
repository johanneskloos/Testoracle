open MatchTraces
open Richtrace
open Trace
open Cleantrace

let trace_base = ref None

let pp_match pp = function
  | Pair(op1, op2) ->
    Format.fprintf pp "%a -> %a" pp_rich_operation op1 pp_rich_operation op2
  | Wrap op ->
    Format.fprintf pp "wrap %a" pp_rich_operation op
  | Init op ->
    Format.fprintf pp "init %a" pp_rich_operation op

type tree_data =
  | NodeData of matching_anti_tree_node
  | FinalNodeData of matching_anti_tree_node
  | EndFailureData of rich_trace
  | InitTailFailure of rich_trace * mode list
type tree = {
  nodes: (int * match_type list * tree_data) list;
  edges: (int * match_operation * int) list
  }
let extend_pm pm tr1 tr2 op =
  match op with
    | WrapperSimple | WrapperPush _ | WrapperPop ->
      (pm @ [Wrap (List.hd tr2)], tr1, List.tl tr2)
    | MatchSimple | MatchPush _ | MatchPop ->
      (pm @ [Pair (List.hd tr1, List.hd tr2)], List.tl tr1, List.tl tr2)
    | Initialization ->
      (pm @ [Init (List.hd tr2)], tr1, List.tl tr2)
      
let flatten_tree mat tr1 tr2 =
  let id = ref 0 in
  let rec flatten_mat pm tr1 tr2 { nodes; edges } =
    let here = !id in
    incr id;
    function
    | Node (dat, []) ->
      { nodes=(here, pm, FinalNodeData dat) :: nodes; edges }
    | Node (dat, sub) ->
      flatten_edges here pm tr1 tr2 { nodes=(here, pm, NodeData dat) :: nodes; edges } sub
    | EndFailure tr -> { nodes=(here, pm, EndFailureData tr) :: nodes; edges }
    | InitTailFailure (tr, st) -> { nodes=(here, pm, InitTailFailure (tr, st)) :: nodes; edges }
  and flatten_edges parent pm tr1 tr2 { nodes; edges } = function
    | (op, mat)::rest ->
      let (pm', tr1', tr2') = extend_pm pm tr1 tr2 op in
        flatten_edges parent pm tr1 tr2 (flatten_mat pm' tr1' tr2'
         { nodes; edges = (parent, op, !id) :: edges } mat) rest
    | [] -> { nodes; edges }
   in flatten_mat [] tr1 tr2 { nodes = []; edges = [] } mat

let pp_stack pp stack = FormatHelper.pp_print_list pp_print_mode pp stack

let dump_node_data_dot base pp = let open Format in function
  | (i, _, NodeData { op1; op2; stack }) ->
    fprintf pp "tooltip=\"%a\\n%a\\n%a\",URL=\"%s.html#node%d\""
      pp_rich_operation op1
      pp_rich_operation op2
      pp_stack stack
      base i
  | (i, _, FinalNodeData { op1; op2; stack }) ->
    fprintf pp "fillcolor=blue,tooltip=\"%a\\n%a\\n%a\",URL=\"%s.html#node%d\""
      pp_rich_operation op1
      pp_rich_operation op2
      pp_stack stack
      base i
  | (i, _, EndFailureData tr) ->
    fprintf pp "fillcolor=red,tooltip=\"%a\",URL=\"%s.html#node%d\""
      pp_rich_trace tr
      base i
  | (i, _, InitTailFailure (tr, stack)) ->
    fprintf pp "fillcolor=green,tooltip=\"%a\\n%a\",URL=\"%s.html#node%d\""
      pp_stack stack
      pp_rich_trace tr
      base i
      
let dump_dot { edges; nodes } base =
  let open Format in
  let chan = open_out (base ^ ".dot") in
  let fmt = formatter_of_out_channel chan in
  fprintf fmt "digraph searchtree {\n\tnode [shape=rectangle]\n";
  List.iter (fun (src, op, tgt) ->
     fprintf fmt "\t%d -> %d [label=\"%a\"]\n"
     src tgt pp_match_operation op)
      edges;
  List.iter (fun ((i, _, _) as d) -> fprintf fmt "\t%d [%a]\n" i (dump_node_data_dot base) d) nodes;
  fprintf fmt "}\n@.";
  close_out chan

let substs = List.map (fun (pat, subst) -> (Str.regexp_string pat, subst)) [
  ("&", "&amp;");
  ("<", "&lt;");
  (">", "&gt;")
  ]
let encode s =
  List.fold_right (fun (pat, subst) -> Str.global_replace pat subst) substs s
let dump_obj chan = function
  | OUndefined -> Printf.fprintf chan "undefined"
  | ONull -> Printf.fprintf chan "null"
  | OBoolean b -> Printf.fprintf chan "%B" b
  | ONumberInt n -> Printf.fprintf chan "%d" n
  | ONumberFloat n -> Printf.fprintf chan "%f" n
  | OString s -> Printf.fprintf chan "\"%s\"" (encode s) 
  | OSymbol s -> Printf.fprintf chan "sym:%s" (encode s)
  | OFunction(id, fid) -> Printf.fprintf chan "fun(%d, %d)" id fid
  | OObject id -> Printf.fprintf chan "obj(%d)" id
  | OOther (ty, id) -> Printf.fprintf chan "other(%s, %d)" (encode ty) id
let dump_call_type chan = function
  | Function -> output_string chan "Function"
  | Method -> output_string chan "Method"
  | Constructor -> output_string chan "Constructor"
  | ConstructorMethod -> output_string chan "ConstructorMethod"
let dump_ref chan (ref, ver) =
  match Reference.get_fieldref ref, Reference.get_name ref with
    | Some (obj, field), None -> Printf.fprintf chan "(%d:%s, %d)" obj (encode field) ver
    | None, Some name -> Printf.fprintf chan "(%s%s, %d)" (if Reference.is_global ref then "global:" else "") (encode name) ver
    | _ -> assert false
let dump_source chan = function
  | Argument i -> Printf.fprintf chan "arg:%d" i
  | With ref -> Printf.fprintf chan "with:%a" dump_ref ref

let dump_op chan = function
  | RFunPre { f; base; args; call_type } -> Printf.fprintf chan "FunPre(%a, %a, %a, %a)" dump_obj f dump_obj base dump_obj args dump_call_type call_type
  | RFunPost { f; base; args; result } -> Printf.fprintf chan "FunPost(%a, %a, %a, %a)" dump_obj f dump_obj base dump_obj args dump_obj result
  | RLiteral { value; hasGetterSetter } -> Printf.fprintf chan "Literal(%a, %B)" dump_obj value hasGetterSetter
  | RForIn obj -> Printf.fprintf chan "ForIn(%a)" dump_obj obj
  | RLocal { name; ref } -> Printf.fprintf chan "Local(%s, %a)" name dump_ref ref
  | RAlias { name; source; ref } -> Printf.fprintf chan "Alias(%s, %a, %a)" name dump_source source dump_ref ref
  | RRead { ref; value } -> Printf.fprintf chan "Read(%a, %a)" dump_ref ref dump_obj value
  | RWrite { ref; oldref; value; success } -> Printf.fprintf chan "Write(%a, %a, %a, %B)" dump_ref ref dump_ref oldref dump_obj value success
  | RReturn obj -> Printf.fprintf chan "Return %a" dump_obj obj
  | RThrow obj -> Printf.fprintf chan "Throw %a" dump_obj obj
  | RWith obj -> Printf.fprintf chan "With %a" dump_obj obj
  | RFunEnter { f; this; args } -> Printf.fprintf chan "FunEnter(%a, %a, %a)" dump_obj f dump_obj this dump_obj args
  | RFunExit { ret; exc } -> Printf.fprintf chan "FunExit(%a, %a)" dump_obj ret dump_obj exc
  | RScriptEnter -> Printf.fprintf chan "ScriptEnter"
  | RScriptExit -> Printf.fprintf chan "ScriptExit"
  | RScriptExc e -> Printf.fprintf chan "ScriptExc %a" dump_obj e
  | RBinary { op; left; right; result } -> Printf.fprintf chan "Binary(%s, %a, %a, %a)" op dump_obj left dump_obj right dump_obj result
  | RUnary { op; arg; result } -> Printf.fprintf chan "Unary(%s, %a, %a)" op dump_obj arg dump_obj result
  | REndExpression -> Printf.fprintf chan "EndExpression"
  | RConditional obj -> Printf.fprintf chan "Conditional %a" dump_obj obj

let dump_stack chan stack =
  match stack with
    | [] -> Printf.fprintf chan "(empty)"
    | _ -> List.iter (function
      | Regular -> Printf.fprintf chan "R"
      | Wrapper -> Printf.fprintf chan "W"
      | External -> Printf.fprintf chan "E"
      | ToString -> Printf.fprintf chan "S")
      stack
      
let dump_node chan (i, tracepair, data) =
  Printf.fprintf chan
    "<h1><a id=\"node%d\">Node %d</a></h1>\n" i i;
  output_string chan
    "<p>Trace to node:</p>\n\
     <table>\n\
    <tr><th>Original</th><th>Modified</th></tr>\n";
  List.iter (function
    | Pair(op1, op2) ->
      Printf.fprintf chan "<tr><td>%a</td><td>%a</td></tr>\n"
        dump_op op1 dump_op op2
    | Init op ->
      Printf.fprintf chan "<tr><td>(init)</td><td>%a</td></tr>\n"
        dump_op op
    | Wrap op ->
      Printf.fprintf chan "<tr><td>(wrap)</td><td>%a</td></tr>\n"
        dump_op op) tracepair;
   Printf.fprintf chan
      "</table>\n";
   match data with
    | NodeData { op1; op2; stack } ->
      Printf.fprintf chan "Match question: %a vs. %a in context %a."
        dump_op op1 dump_op op2 dump_stack stack
    | FinalNodeData { op1; op2; stack } ->
      Printf.fprintf chan "Match question: %a vs. %a in context %a.\n<strong>No matches found!</strong>\n"
        dump_op op1 dump_op op2 dump_stack stack
    | EndFailureData tr ->
      Printf.fprintf chan "<p>Modified trace completely consumed, but original trace remaining:</p><table>";
      List.iter (fun (op, _) -> Printf.fprintf chan "<tr><td>%a</td></tr>\n" dump_op op) tr;
      Printf.fprintf chan "</table>\n"
    | InitTailFailure (tr, stack) ->
      Printf.fprintf chan "<p>Remaining modified trace cannot be absorbed as initialization code:</p>\n";
      Printf.fprintf chan "<p>Stack: %a</p>\n" dump_stack stack;
      Printf.fprintf chan "<p>Trace:</p><table>";
      List.iter (fun (op, _) -> Printf.fprintf chan "<tr><td>%a</td></tr>\n" dump_op op) tr;
      Printf.fprintf chan "</table>\n"
      
let dump_data { nodes } base =
  let open Format in
  let chan = open_out (base ^ ".html") in
  List.iter (dump_node chan) (List.sort (fun (i1, _, _) (i2, _, _) -> compare i1 i2) nodes);
  close_out chan
        
let pp_matching pp = function
  | Success tr -> FormatHelper.pp_print_list_lines pp_match pp tr
  | Failure atr ->
    Format.fprintf pp "failure"

let dump_result { trace = tr1 } { trace = tr2 } res =
  match res, !trace_base with
    | Failure mat, Some base ->
      let data = flatten_tree mat (List.map fst tr1) (List.map fst tr2) in
      dump_dot data base;
      dump_data data base
    | _ -> ()
