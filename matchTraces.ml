open Richtrace
open Trace
open Reference
open LocalFacts
open Misc
open MatchObjects

(**
* Basic predicates.
*
* The following properties of operations are decidable using any extra
* information.
*
* A function is unobservable if it neither changes global state nor
* calls external functions.
*)
let is_unobservable = function
    | RForIn _ | RLocal _ | RAlias _ | RRead _ | RReturn _
    | RWith _ | RScriptEnter | RScriptExit | RScriptExc _ | RBinary _
    | RUnary _ | REndExpression | RConditional _ | RLiteral _
    | RFunEnter _ | RFunExit _ ->
        true
    | RFunPre _ | RWrite _ | RFunPost _ | RThrow _ ->
        false

(**
* Some operations just don't make sense at the top level.
*)
let is_toplevel = function
    | RFunEnter _ | RFunExit _ | RReturn _ -> false
    | _ -> true

let is_throw = function
  | RThrow _ -> true
  | _ -> false
  
(**
* Some obvious classifications.
*)
let is_write = function RWrite _ -> true | _ -> false
let is_exit = function RFunExit _ -> true | _ -> false
let is_post_exit = function RFunPost _ -> true | _ -> false
(**
* Functions dependening on the current matching state.
*
* We summarize the matching state in a record.
*)
type matching_state = {
    rt1: rich_tracefile;
    rt2: rich_tracefile;
    facts1: local_facts;
    facts2: local_facts;
    objeq: bool IntIntMap.t;
    initialisation_data: VersionReferenceSet.t;
    toString_data: jsval list
}

(**
* Check if two alias sources match.
*)
let match_source { rt1; rt2; facts1; facts2; objeq } src1 src2 =
    match src1, src2 with
    | Argument i1, Argument i2 -> (i1 = i2, objeq)
    | With r1, With r2 ->
        MatchObjects.match_refs rt1 rt2 facts1 facts2 r1 r2 objeq
    | _ -> (false, objeq)

(**
* Check if two operations match. This does not take
* any stack state into account; it purely matches the arguments.
*)
let match_operations matching_state op1 op2 =
    let { rt1; rt2; facts1; facts2; objeq } = matching_state in
    let check = MatchObjects.match_values rt1 rt2 facts1 facts2
    and check_ref = MatchObjects.match_refs rt1 rt2 facts1 facts2 in
    match op1, op2 with
    | RFunPre { f = f1; base = base1; args = args1; call_type = ct1 },
    RFunPre { f = f2; base = base2; args = args2; call_type = ct2 } ->
        let (eqf, objeq) = check f1 f2 objeq in
        let (eqbase, objeq) = check base1 base2 objeq in
        let (eqargs, objeq) = check args1 args2 objeq in
        (eqf && eqbase && eqargs && ct1 = ct2, objeq)
    | RFunPost { f = f1; base = base1; args = args1; result = res1 },
    RFunPost { f = f2; base = base2; args = args2; result = res2 } ->
        let (eqf, objeq) = check f1 f2 objeq in
        let (eqbase, objeq) = check base1 base2 objeq in
        let (eqargs, objeq) = check args1 args2 objeq in
        let (eqres, objeq) = check res1 res2 objeq in
        (eqf && eqbase && eqargs && eqres, objeq)
    | RLiteral { value = val1; hasGetterSetter = hgs1 },
    RLiteral { value = val2; hasGetterSetter = hgs2 } ->
        let (eqval, objeq) = check val1 val2 objeq in
        (eqval && hgs1 = hgs2, objeq)
    | RLocal { name = name1; ref = ref1 },
    RLocal { name = name2; ref = ref2 } ->
        let (eqref, objeq) = check_ref ref1 ref2 objeq in
        (eqref && name1 = name2, objeq)
    | RAlias { name = name1; ref = ref1; source = src1 },
    RAlias { name = name2; ref = ref2; source = src2 } ->
        let (eqsrc, objeq) = match_source matching_state src1 src2 in
        let (eqref, objeq) = check_ref ref1 ref2 objeq in
        (name1 = name2 && eqsrc && eqref, objeq)
    | RRead { ref = ref1; value = val1 },
    RRead { ref = ref2; value = val2 } ->
        let (eqref, objeq) = check_ref ref1 ref2 objeq in
        let (eqval, objeq) = check val1 val2 objeq in
        (eqref && eqval, objeq)
    | RWrite { ref = ref1; oldref = oref1; value = val1; success = succ1 },
    RWrite { ref = ref2; oldref = oref2; value = val2; success = succ2 } ->
        let (eqref, objeq) = check_ref ref1 ref2 objeq in
        let (eqval, objeq) = check val1 val2 objeq in
        let (eqoref, objeq) = check_ref oref1 oref2 objeq in
        (eqref && eqval && eqoref && succ1 = succ2, objeq)
    | RForIn val1, RForIn val2 -> check val1 val2 objeq
    | RReturn val1, RReturn val2 -> check val1 val2 objeq
    | RThrow val1, RThrow val2 -> check val1 val2 objeq
    | RWith val1, RWith val2 -> check val1 val2 objeq
    | RFunEnter { f = f1; this = this1; args = args1 },
    RFunEnter { f = f2; this = this2; args = args2 } ->
        let (eqf, objeq) = check f1 f2 objeq in
        let (eqthis, objeq) = check this1 this2 objeq in
        let (eqargs, objeq) = check args1 args2 objeq in
        (eqf && eqthis && eqargs, objeq)
    | RFunExit { ret = ret1; exc = exc1 },
    RFunExit { ret = ret2; exc = exc2 } ->
        let (eqret, objeq) = check ret1 ret2 objeq in
        let (eqexc, objeq) = check exc1 exc2 objeq in
        (eqret && eqexc, objeq)
    | RScriptEnter, RScriptEnter -> (true, objeq)
    | RScriptExit, RScriptExit -> (true, objeq)
    | RScriptExc val1, RScriptExc val2 -> check val1 val2 objeq
    | RBinary { op = op1; left = left1; right = right1; result = result1 },
    RBinary { op = op2; left = left2; right = right2; result = result2 } ->
        let (eqleft, objeq) = check left1 left2 objeq in
        let (eqright, objeq) = check right1 right2 objeq in
        let (eqresult, objeq) = check result1 result2 objeq in
        (eqleft && eqright && eqresult && op1 = op2, objeq)
    | RUnary { op = op1; arg = arg1; result = result1 },
    RUnary { op = op2; arg = arg2; result = result2 } ->
        let (eqarg, objeq) = check arg1 arg2 objeq in
        let (eqresult, objeq) = check result1 result2 objeq in
        (eqarg && eqresult && op1 = op2, objeq)
    | REndExpression, REndExpression -> (true, objeq)
    | RConditional val1, RConditional val2 -> check val1 val2 objeq
    | _, _ -> (false, objeq)

let match_operations matching_state op1 op2 =
  try match_operations matching_state op1 op2 with Not_found -> failwith "match_operations failed"
  
(**
* Predicates that check whether a function can be used in a specific
* context.
*
* These two predicates classify certain types of writes.
*)
let is_instrumentation_write { initialisation_data } = function
    | RWrite { oldref } -> VersionReferenceSet.mem oldref initialisation_data
    | _ -> false

let is_function_update { rt2 } = function
    | RWrite { ref } ->
        begin match VersionReferenceMap.find ref rt2.points_to with
            | OFunction _ -> true
            | _ -> false
        end
    | _ -> false

let is_function_update func =
  try is_function_update func with Not_found -> failwith "is_function_update failed"
  
(**
* The folloing three predicates detect operations that can
* be used without any special handling in various contexts. *)
let may_insert_in_init matching_state op =
    (is_unobservable op || is_instrumentation_write matching_state op ||
    is_function_update matching_state op)

let may_insert_in_matching_simple op =
    is_unobservable op || is_write op || is_throw op

let may_insert_in_wrap_simple matching_state op =
    may_insert_in_init matching_state op (* for now *)

let may_insert_in_toString_simple = is_unobservable

(**
* Predicates for call classification.
* All these predicates presume that standard object equality has
* already been checked. Thus, only the function arguments are
* considered.
*
* First come some helper functions.
*)
let convert
    {
        rt1 ={ funcs = funs1; points_to = pt1 };
        rt2 ={ funcs = funs2; points_to = pt2 };
        facts1; facts2
    } = { funs1; funs2; pt1; pt2; facts1; facts2 }

let is_internal_call_impl { funcs } f =
    try
      begin match funcs.(f) with Local _ -> true | External _ -> false end
   with
    | e -> Format.eprintf "trying to get %d from %a@." f
    (FormatHelper.pp_print_array pp_funcspec) funcs; raise e

let is_internal_call rt = function
    | RFunPre { f = OFunction(_, f) } -> is_internal_call_impl rt f
    | _ -> false

(**
* A generic matcher that wraps the annoying details.
* The concrete matchers are implemented below.
*
* Note that the property of functions being local is checked
* * only on the first function *!
*)
let is_matching_call literally_equal local matching_data op1 op2 =
    match op1, op2 with
    | RFunPre { f = OFunction(_, f1) },
    RFunPre { f = OFunction(_, f2) } ->
        match_functions (convert matching_data) f1 f2 = literally_equal &&
        is_internal_call_impl matching_data.rt1 f1 = local
    | _, _ -> false

let is_matching_call literally_equal local matching_data op1 op2 =
  try is_matching_call literally_equal local matching_data op1 op2 with Not_found -> failwith "is_matching_call failed"
(**
* Check if both calls go to an identical internal function.
*)
let is_matching_internal_call = is_matching_call true true

(**
* Check if both calls go to the same external function.
*)
let is_matching_external_call = is_matching_call true false

(**
* Check if we are looking at a toString pair.
* This needs special attention: Since the function
* will obviously not match, we need to handle matching
* in a special way.
*
* We ignore the argument array - the original string
* doesn't use it, and we don't case what the other version does with
* it.
*)
let is_matching_toString_call matching_data op1 op2 =
    match op1, op2 with
    | RFunPre { f = f1; base = this1 },
      RFunPre { f = f2; base = this2 } ->
        let { rt1; rt2; facts1; facts2; objeq; toString_data } = matching_data in
        begin match
            MatchObjects.match_values rt1 rt2 facts1 facts2 this1 this2 objeq
            with
            | (false, _) -> false
            | (true, _) ->  List.mem f1 toString_data
        end
    | _ -> false

let is_matching_toString_call matching_data op1 op2 =
  try is_matching_toString_call matching_data op1 op2 with
    | Not_found -> failwith "is_matching_toString_call failed"
(**
* Check if we are looking at a potential wrapper.
*
* Do it in a simple way: internal calls that don't match
* literally.
*)
let may_be_wrapper_entry = is_matching_call false true

(** Check if the event is some kind of function event.
 *
 *)
let is_not_function = function
  | RFunPre _ | RFunPost _ | RFunEnter _ | RFunExit _ | RReturn _ -> false
  | _ -> true


(** The trace matcher is built in two parts,
* the match candidate finder and the matching engine.
* They communicate using the following protocol:
* The matching engine calls the appropriate candidate finder
* for its current state with the head operations of both traces,
* and the candidate finder returns a list of matching operations
* that describe how the traces can be matched at this point.
*
* Example:
* Suppose we are in top - level mode, and the head operations
* are Read(ref1, "x") and Read(ref2, "x").
* If ref1 and ref2 match, then two operations are possible,
* namely MatchSimple, matching both operations together and continuing
* with the trail of the traces on both said, and
* Init, which treats the second operation as initialization code,
* and continues matching with the full trace on the left and the tail
* on the right.
*)
(** The mode to switch to, on a push. *)
type mode = Wrapper | Regular | External | ToString
type match_operation =
    (* Both operations match, no stack change *)
    | MatchSimple
    (* Both operations match, and a push is required. *)
    | MatchPush of mode
    (* Both operations match, and a pop is required. *)
    | MatchPop
    (* The second operation may be initialization code. *)
    | Initialization
    (* The second operation may be wrapper code, no stack change. *)
    | WrapperSimple
    (* The second operation may be wrapper code, push required. *)
    | WrapperPush of mode
    (* The second operation may be wrapper code, pop required. *)
    | WrapperPop

(**
* A helper for candidate generators.
*)
let add_objeq op objeq cands =
    (cands, if is_write op then IntIntMap.empty else objeq)

(**
* The first half of the trace matcher: The candidate generators.
* For each operating mode of the trace matcher, there is
* one generator. *)
let toplevel_candidates matching_state op1 op2 =
    let (match_op1_op2, objeq) = match_operations matching_state op1 op2 in
    [ (match_op1_op2 && may_insert_in_matching_simple op2 && is_toplevel op2, MatchSimple);
    (match_op1_op2 && is_matching_internal_call matching_state op1 op2, MatchPush Regular);
    (match_op1_op2 && is_matching_external_call matching_state op1 op2, MatchPush External);
    (is_matching_toString_call matching_state op1 op2, MatchPush ToString);
    (match_op1_op2 && may_be_wrapper_entry matching_state op1 op2, MatchPush Wrapper);
    (may_insert_in_init matching_state op2 && is_toplevel op2 && is_not_function op2, Initialization) ]
    |> List.filter fst |> List.map snd |> add_objeq op2 objeq

let regular_candidates matching_state op1 op2 =
    let (match_op1_op2, objeq) = match_operations matching_state op1 op2 in
    [ (match_op1_op2 && may_insert_in_matching_simple op2, MatchSimple);
    (match_op1_op2 && is_matching_internal_call matching_state op1 op2, MatchPush Regular);
    (match_op1_op2 && is_matching_external_call matching_state op1 op2, MatchPush External);
    (is_matching_toString_call matching_state op1 op2, MatchPush ToString);
    (match_op1_op2 && may_be_wrapper_entry matching_state op1 op2, MatchPush Wrapper);
    (match_op1_op2 && is_post_exit op2, MatchPop) ]
    |> List.filter fst |> List.map snd |> add_objeq op2 objeq

let wrap_candidates matching_state op1 op2 =
    [ (is_internal_call matching_state.rt2 op2, WrapperPush Regular);
    (is_internal_call matching_state.rt2 op2, WrapperPush Wrapper);
    (is_post_exit op2, WrapperPop);
    (may_insert_in_wrap_simple matching_state op2, WrapperSimple) ]
    |> List.filter fst |> List.map snd |> add_objeq op2 matching_state.objeq

let toString_candidates matching_state op1 op2 =
    [ (is_internal_call matching_state.rt2 op2, WrapperPush ToString);
    (is_post_exit op2, WrapperPop);
    (is_unobservable op2, WrapperSimple) ]
    |> List.filter fst |> List.map snd |> add_objeq op2 matching_state.objeq

let external_candidates matching_state op1 op2 =
    let (match_op1_op2, objeq) = match_operations matching_state op1 op2 in
    (if match_op1_op2 && is_post_exit op2 then [ MatchPop ] else [])
    |> add_objeq op2 objeq

(**
* The operating states, and the general candidate generator. *)
type state = InToplevel | InRegular | InWrap | InToString | InExternal

let build_candidates matching_state op1 op2 = function
    | InToplevel -> toplevel_candidates matching_state op1 op2
    | InRegular -> regular_candidates matching_state op1 op2
    | InWrap -> wrap_candidates matching_state op1 op2
    | InToString -> toString_candidates matching_state op1 op2
    | InExternal -> external_candidates matching_state op1 op2

(**
* The entries of the matching certificate.
*
* Pair indicates paired operations, describing the subword
* relationship. All other operations get classified as either wrapper
* or initialisation.
*)
type match_type =
    | Pair of rich_operation * rich_operation
    | Wrap of rich_operation
    | Init of rich_operation

(** Pretty-printers for matching state and match operations *)
let pp_matching_state pp { rt1; rt2; facts1; facts2; objeq; initialisation_data; toString_data } =
  Format.fprintf pp "..."
let pp_print_mode pp = function
  | Regular -> Format.pp_print_string pp "regular"
  | Wrapper -> Format.pp_print_string pp "wrap"
  | External -> Format.pp_print_string pp "external"
  | ToString ->  Format.pp_print_string pp "toString"
  
let pp_match_operation pp = function
  | Initialization -> Format.pp_print_string pp "init"
  | WrapperSimple -> Format.pp_print_string pp "wrap"
  | WrapperPop -> Format.pp_print_string pp "wrap, pop"
  | WrapperPush m -> Format.fprintf pp "wrap, push %a" pp_print_mode m
  | MatchSimple -> Format.pp_print_string pp "match"
  | MatchPop -> Format.pp_print_string pp "match, pop"
  | MatchPush m -> Format.fprintf pp "match, push %a" pp_print_mode m 

(**
* Helpers for the matching engine.
*
* Get the operating state by examining the stack.
*)
let get_state = function
    | Wrapper :: _ -> InWrap
    | Regular :: _ -> InRegular
    | External :: _ -> InExternal
    | ToString :: _ -> InToString
    | [] -> InToplevel

(**
* Special-case handling for a trace ending in initialisation code.
* This is legal, but probably not very useful, except in the degenerate
* case of empty code.
*)
let can_be_added_as_initialisation matching_state trace stack =
    get_state stack = InToplevel &&
    List.for_all
        (fun (op, facts) ->
                may_insert_in_init { matching_state with facts2 = facts } op)
        trace

(**
* Effects of the matching operations on various bits of state.
*
* [adapt_first op op1 facts1 trace1]
* computes the original code trace to use for further matching,
* given a matching operation [op].
*)
let adapt_first op op1 facts1 trace1 =
    match op with
    | MatchSimple | MatchPush _ | MatchPop -> trace1
    | _ -> (op1, facts1) :: trace1

(** [adapt_stack op stack]
* applies the required stack manipulation for [op]. *)
let adapt_stack op stack =
    match op with
    | MatchPush mode | WrapperPush mode -> mode :: stack
    | MatchPop | WrapperPop -> List.tl stack
    | _ -> stack

(** [extend_matching op op1 op2 matching]
* extends the given matching according to [op]. *)
let extend_matching op op1 op2 matching =
    match op with
    | MatchSimple | MatchPush _ | MatchPop -> Pair(op1, op2) :: matching
    | WrapperSimple | WrapperPush _ | WrapperPop -> Wrap op2 :: matching
    | Initialization -> Init op2 :: matching

(** Collect the references belonging to a value. *)
let collect_object_references { facts2 = facts; rt2 = { objs } } id =
  objs.(id)
  |> StringMap.bindings
  |> List.map (fun (field, _) -> reference_of_fieldref (id, field) |> make_versioned facts)

let collect_references matching_state = function
  | OObject id -> collect_object_references matching_state id
  | OFunction(id, _) -> collect_object_references matching_state id
  | OOther(_, id) -> collect_object_references matching_state id
  | _ -> []

(** Perpetuate initialisation-produced data. *)
let perpetuate_initialisation_data matching_state op =
    let { initialisation_data = init_old } = matching_state in
    let initialisation_data =
        match op with
        | RWrite { ref; oldref }
        when VersionReferenceSet.mem oldref init_old ->
            VersionReferenceSet.add ref init_old
        | RLiteral { value } ->
          List.fold_left (fun init ref -> VersionReferenceSet.add ref init)
            init_old (collect_references matching_state value)
        | RLocal { ref } ->
            VersionReferenceSet.add ref init_old
        | _ -> init_old
    in
    { matching_state with initialisation_data }

let detect_toString op1 matching_state = match op1 with
  | RRead { ref; value } ->
     begin match get_fieldref (fst ref) with
      | Some (_, name) when name = "toString" ->
         { matching_state with
           toString_data = value :: matching_state.toString_data }
      | _ -> matching_state
      end
   | _ -> matching_state
  
let adapt_matching_state op op1 op2 matching_state =
    begin match op with
    | MatchSimple | MatchPush _ | MatchPop -> matching_state
    | _ -> perpetuate_initialisation_data matching_state op2
    end |> detect_toString op1


type matching_anti_tree_node = {
  op1: rich_operation;
  op2: rich_operation;
  stack: mode list
  }
type matching_anti_tree =
  | Node of matching_anti_tree_node * (match_operation * matching_anti_tree) list
  | EndFailure of rich_trace
  | InitTailFailure of rich_trace * mode list

type result =
  | Success of match_type list
  | Failure of matching_anti_tree

type apply_one_result =
  | A1Success of match_type list
  | A1Failure of matching_anti_tree list
   
(** The matching engine.
*
* It consists of two mutually - recursive functions:
* [matching_engine matching_state trace1 trace2 stack]
* handles the various cases of empty and non - empty traces
* [trace1] and [trace2], using
* [apply_first_working matching_state op1 op2 trace1 trace2 stack]
* to handle the list of operations generated by the candidate
* generator when there is an operation to match on both sides. *)
let rec matching_engine matching_state trace1 trace2 stack =
    match trace1, trace2 with
    | (op1, facts1) :: trace1, (op2, facts2) :: trace2 ->
        let matching_state' = { matching_state with facts1; facts2 } in
        let (ops, objeq) =
            build_candidates matching_state' op1 op2 (get_state stack) in
        begin match apply_first_working
            { matching_state' with objeq }
            op1 op2 trace1 trace2 stack ops
        with
          | A1Success tr -> Success tr
          | A1Failure l -> Failure (Node ({ op1; op2; stack}, List.map2 (fun op mat -> (op, mat)) ops l))
        end
    | _ :: _, [] ->
       Failure (EndFailure trace1)
    | [], trace2 ->
        if can_be_added_as_initialisation matching_state trace2 stack then begin
            Success (List.map (fun (op, _) -> Init op) trace2)
        end else begin
            Failure (InitTailFailure (trace2, stack))
        end
and apply_first_working matching_state op1 op2 trace1 trace2 stack =
    function
    | [] ->
      A1Failure []
    | op :: ops ->
        match
        matching_engine
            (adapt_matching_state op op1 op2 matching_state)
            (adapt_first op op1 matching_state.facts1 trace1)
            trace2
            (adapt_stack op stack)
        with
        | Success matching ->
            A1Success (extend_matching op op1 op2 matching)
        | Failure ft ->
          match
            apply_first_working matching_state op1 op2 trace1 trace2 stack ops
            with
              | A1Success s -> A1Success s
              | A1Failure l -> A1Failure (ft :: l)

(** The main entry point for trace matching. *)
let match_traces rt1 rt2 =
    matching_engine
        { rt1; rt2;
            facts1 = empty_local_facts;
            facts2 = empty_local_facts;
            objeq = IntIntMap.empty;
            initialisation_data = VersionReferenceSet.empty;
            toString_data = []
        } rt1.trace rt2.trace []

let match_traces rt1 rt2 = Printexc.print (match_traces rt1) rt2
