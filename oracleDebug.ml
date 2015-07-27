open MatchTraces
open Richtrace
open Trace
open Cleantrace

(** Debugging functions for the oracle. *)
let trace_base = ref None

type tree_data =
    | NodeData of matching_anti_tree_node
    | FinalNodeData of matching_anti_tree_node
    | EndFailureData of rich_trace
    | InitTailFailureData of rich_trace * mode list
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
    | Initialization | InitializationPush _ | InitializationPop ->
        (pm @ [Init (List.hd tr2)], tr1, List.tl tr2)

(** Output of debugging data *)
let with_out_file name f =
    let chan = open_out name in
    try let res = f chan in close_out chan; res
    with e -> close_out chan; raise e

let pp_match pp = function
    | Pair(op1, op2) ->
        Format.fprintf pp "%a -> %a" pp_rich_operation op1 pp_rich_operation op2
    | Wrap op ->
        Format.fprintf pp "wrap %a" pp_rich_operation op
    | Init op ->
        Format.fprintf pp "init %a" pp_rich_operation op

let flatten_tree mat tr1 tr2 =
    let id = ref 0 in
    let rec flatten_mat pm tr1 tr2 { nodes; edges } =
        let here = !id in
        incr id;
        function
        | Node (dat, []) ->
            { nodes = (here, pm, FinalNodeData dat) :: nodes; edges }
        | Node (dat, sub) ->
            flatten_edges here pm tr1 tr2 { nodes = (here, pm, NodeData dat) :: nodes; edges } sub
        | EndFailure tr -> { nodes = (here, pm, EndFailureData tr) :: nodes; edges }
        | InitTailFailure (tr, st) -> { nodes = (here, pm, InitTailFailureData (tr, st)) :: nodes; edges }
    and flatten_edges parent pm tr1 tr2 { nodes; edges } = function
        | (op, mat):: rest ->
            let (pm', tr1', tr2') = extend_pm pm tr1 tr2 op in
            flatten_edges parent pm tr1 tr2 (flatten_mat pm' tr1' tr2'
                        { nodes; edges = (parent, op, !id) :: edges } mat) rest
        | [] -> { nodes; edges }
    in flatten_mat [] tr1 tr2 { nodes = []; edges = [] } mat

let pp_stack pp stack = FormatHelper.pp_print_list pp_print_mode pp stack

let dump_node_data_dot_color = function
    | NodeData _ -> "white"
    | FinalNodeData _ -> "blue"
    | EndFailureData _ -> "red"
    | InitTailFailureData _ -> "green"

let dump_node_data_dot_tooltip pp = let open Format in function
    | NodeData { op1; op2; stack }
    | FinalNodeData { op1; op2; stack } ->
        fprintf pp "%a\\n%a\\n%a"
            pp_rich_operation op1
            pp_rich_operation op2
            pp_stack stack
    | EndFailureData tr ->
        fprintf pp "%a" pp_rich_trace tr
    | InitTailFailureData (tr, stack) ->
        fprintf pp "%a\n%a" pp_rich_trace tr pp_stack stack

let dump_node_data_dot base pp (i, _, d) =
    let open Format in
    fprintf pp "tooltip=\"%a\",style=filled,fillcolor=%s,URL=\"step%d.html\""
        dump_node_data_dot_tooltip d
        (dump_node_data_dot_color d)
        i

let dump_matchgraph { edges; nodes } base trace =
    let open Format in
    with_out_file (base ^ "/tracematch.dot") (fun chan ->
                let fmt = formatter_of_out_channel chan in
                fprintf fmt "digraph searchtree {\n\tnode [shape=rectangle]\n";
                List.iter (fun (src, op, tgt) ->
                            fprintf fmt "\t%d -> %d [label=\"%a\"]\n"
                                src tgt pp_match_operation op)
                    edges;
                List.iter (fun ((i, _, _) as d) -> fprintf fmt "\t%d [%a]\n" i (dump_node_data_dot base) d) nodes;
                fprintf fmt "\tnode [shape=plaintext]\n\tedge [style=invis]\n";
                List.iteri (fun i (op, _) ->
                            fprintf fmt "\te%d [label=\"%a\"]\n" i pp_rich_operation op;
                            if (i > 0) then fprintf fmt "\te%d -> e%d\n" (i -1) i) trace;
                fprintf fmt "}\n@.")

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
    match Reference.get_fieldref ref, Reference.get_name ref, Reference.is_global ref with
    | Some (obj, field), None, _ -> Printf.fprintf chan "(%d:%s, %d)" obj (encode field) ver
    | None, Some name, true -> Printf.fprintf chan "(global:%s, %d)" (encode name) ver
    | None, Some name, false -> Printf.fprintf chan "(%s, %d)" (encode name) ver
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
                | ToString -> Printf.fprintf chan "S"
                | Init -> Printf.fprintf chan "I")
            stack

let dump_match_type chan = let open Printf in function
  | Pair(op1, op2) ->
    fprintf chan "<tr><td>%a</td><td>%a</td><td></td></tr>\n"
      dump_op op1 dump_op op2
  | Init op ->
    fprintf chan "<tr><td></td><td>%a</td><td>Init</td></tr>\n"
      dump_op op
  | Wrap op ->
    fprintf chan "<tr><td></td><td>%a</td><td>Wrap</td></tr>\n"
      dump_op op

let dump_mode chan = let open Printf in function
  | Regular -> fprintf chan "regular"
  | Wrapper -> fprintf chan "wrapper"
  | External -> fprintf chan "external"
  | ToString -> fprintf chan "toString"
  | Init -> fprintf chan "init"


let dump_mop chan = let open Printf in function
  | MatchSimple -> fprintf chan "match"
  | MatchPop -> fprintf chan "match and pop"
  | MatchPush m -> fprintf chan "match and push '%a'" dump_mode m
  | WrapperSimple -> fprintf chan "wrap"
  | WrapperPop -> fprintf chan "wrap and pop"
  | WrapperPush m -> fprintf chan "wrap and push '%a'" dump_mode m
  | Initialization -> fprintf chan "initialize"
  | InitializationPush m -> fprintf chan "initialize and push '%a'" dump_mode m
  | InitializationPop -> fprintf chan "initialize and pop"

let dump_path chan = let open Printf in function
  | [] -> fprintf chan "(top)"
  | top::path ->
    fprintf chan "%s" top;
    List.iter (fprintf chan ".%s") path
  
let dump_reason
  chan = 
    let dumpobj = dump_obj in
    let open Printf in let open MatchObjects in function
  | NonMatching (path, val1, val2) ->
    fprintf chan "Match failure at %a: %a vs. %a"
      dump_path path
      dumpobj val1
      dumpobj val2
  | MissingOrig path ->
    fprintf chan "Match failure: %a missing in orig"
      dump_path path
  | MissingXfrm path ->
    fprintf chan "Match failure: %a missing in xfrm"
      dump_path path
  | Other msg ->
    fprintf chan "Match failure: %s" msg

let dump_cond chan = let open Printf in function
  | MatchSides -> fprintf chan "op1 and op2 don't match"
  | MayMatchSimple -> fprintf chan "cannot be inserted into a simple match"
  | MatchCallInt -> fprintf chan "not a matching internal call"
  | MatchCallExt -> fprintf chan "not a matching external call"
  | MatchCallToString -> fprintf chan "not a toString special call"
  | MatchCallWrap -> fprintf chan "not a wrapper call"
  | MayInit -> fprintf chan "cannot occur in init"
  | IsToplevel -> fprintf chan "cannot occur at top level"
  | IsNotFunction -> fprintf chan "is a function event"
  | IsExit -> fprintf chan "not an exit event"
  | IsCallInt -> fprintf chan "not an internal call event"
  | IsUnobservable -> fprintf chan "not known to be unobservable"
  | MayInsertInWrapSimple -> fprintf chan "cannot be inserted in simple wrapper code"
 
let dump_conds chan = let open Printf in function
  | [] -> fprintf chan "(none)"
  | c::cs -> fprintf chan "%a" dump_cond c; List.iter (fprintf chan ", %a" dump_cond) cs

let dump_step base id pm td tr1 tr2 edges chan =
  let open Printf in
  fprintf chan "<html><head><title>Step %d</title></head><body>\n" id;
  fprintf chan "<h1>Step %d</h1>\n" id;
  fprintf chan "<p>Partial match:</p>\n";
  fprintf chan "<table><tr><th>Original trace</th><th>Modified trace</th><th></th></tr>\n";
  List.iter (dump_match_type chan) pm;
  fprintf chan "</table>\n";
  begin match td with
    | NodeData { op1; op2; stack; failure_trace } ->
        fprintf chan "<p>Context stack: %a</p>\n" dump_stack stack;
        fprintf chan "<p>Next unmodified operation: %a</p>\n" dump_op op1; 
        fprintf chan "<p>Next modified operation: %a</p>\n" dump_op op2;
        fprintf chan "<p>Matches for the next step:</p><ul>\n";
        List.iter (fun (_, mop, next) ->
          fprintf chan "<li><a href=\"step%d.html\">%a</a></li>\n" next
            dump_mop mop) edges;
        let (fail_cond, deep_failure) = failure_trace in
        fprintf chan "</ul>\n<p>Candidate matches that failed, with reasons: <ul>\n";
        List.iter (fun (reasons, what) ->
          fprintf chan "<li>%a: %a</li>\n" dump_mop what dump_conds reasons) fail_cond;
        fprintf chan "</ul>\n";
        begin match deep_failure with
          | Some (what_failed, reason) ->
            fprintf chan "<p>Explanation for matching failures: %s didn't match, reason: %a</p>\n"
              what_failed dump_reason reason
          | None -> ()
        end
    | FinalNodeData { op1; op2; stack; failure_trace } ->
      fprintf chan "<p>Context stack: %a</p>\n" dump_stack stack;
        fprintf chan "<p>Next unmodified operation: %a</p>\n" dump_op op1; 
        fprintf chan "<p>Next modified operation: %a</p>\n" dump_op op2;
        fprintf chan "<p><strong>No matches for next step</strong></p>\n";
        let (fail_cond, deep_failure) = failure_trace in
        fprintf chan "</ul>\n<p>Candidate matches that failed, with reasons: <ul>\n";
        List.iter (fun (reasons, what) ->
          fprintf chan "<li>%a: %a</li>\n" dump_mop what dump_conds reasons) fail_cond;
        fprintf chan "</ul>\n";
        begin match deep_failure with
          | Some (what_failed, reason) ->
            fprintf chan "<p>Explanation for matching failures: %s didn't match, reason: %a</p>\n"
              what_failed dump_reason reason
          | None -> ()
        end
    | EndFailureData tr ->
      fprintf chan "<p>Modified trace has been completely consumed, but there is something left in the unmodified trace:<table>\n";
      List.iter (fun (op, _) -> fprintf chan "<tr><td>%a</td></tr>\n" dump_op op) tr;
      fprintf chan "</table></p>\n"
    | InitTailFailureData (tr, _) ->
      fprintf chan "<p>Unmodified trace has been completely consumed, but the remaining modified trace cannot be classified as initial:<table>\n";
      List.iter (fun (op, _) -> fprintf chan "<tr><td>%a</td></tr>\n" dump_op op) tr;
      fprintf chan "</table></p>\n"
  end;
  fprintf chan "</body></html>"
  
let dump_steps { nodes; edges } base tr1 tr2 =
  List.iter (fun (id, pm, td) ->
    let oedges = List.filter (fun (id', _, _) -> id = id') edges in
    with_out_file
      (base ^ "/step" ^ string_of_int id ^ ".html")
      (dump_step base id pm td tr1 tr2 oedges )) nodes
        
let dump_result tf1 tf2 res =
    let { trace = tr1 } = tf1 and { trace = tr2 } = tf2 in
    match res, !trace_base with
    | Failure mat, Some base ->
        let data = flatten_tree mat (List.map fst tr1) (List.map fst tr2) in
        (* Ignore any error - if it's EEXIST, we don't care, and otherwise,
         * we'll get another error from all the open_out calls anyways.
         * Since this is just debugging code, there's no point in trying
         * to recover anyway. *)
        begin try Unix.mkdir base 0o755 with Unix.Unix_error _ -> () end;
        dump_matchgraph data base tr2;
        dump_steps data base tr1 tr2
    | Success matching, Some base ->
      with_out_file (base ^ ".matching.html")
        (fun chan ->
          let open Printf in
          fprintf chan "<html><head><title>Matching for %s</title></head>\n" base;
          fprintf chan "<body><h1>Matching for %s</h1>\n" base;
          fprintf chan "<table><tr><th>Original</th><th>Modified</th><th></th>\n";
          List.iter (fprintf chan "%a" dump_match_type) matching;
          fprintf chan "</table></body></html>\n")
     | _, None -> () 
        
