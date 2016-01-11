open MatchTypes
module VersionReferenceSet = Reference.VersionReferenceSet
module IntIntMap = Misc.IntIntMap

let interpret_conds matching_state op1 op2 conds =
    Misc.List.filtermap (fun cond ->
                match MatchOperations.interpret_cond matching_state op1 op2 cond with
                | Some reason -> Some (cond, reason)
                | None -> None)
        conds

let interpret_rule matching_state op1 op2 (conds, matchop) =
  (interpret_conds matching_state op1 op2 conds, matchop)
    
let interpret_rules (rules: MatchRules.match_rules) matching_state op1 op2 =
    let split = List.partition (function ([], _) -> true | _ -> false) in
    rules
    |> List.map (interpret_rule matching_state op1 op2)
    |> split
    |> fun (applicable, not_applicable) -> (List.map snd applicable, not_applicable)

let build_candidates matching_state op1 op2 state =
    interpret_rules (MatchRules.find_rules state) matching_state op1 op2

(**
* Helpers for the matching engine.
*)
let can_be_added_as_initialisation matching_state trace stack =
    if get_state stack <> InToplevel then Some NotAtToplevel
    else if List.for_all (fun ev ->
                MatchOperations.may_insert_in_init matching_state ev = None) trace
    then None else Some NotInitCode

let adapt_first op (op1, facts1) trace1 =
    match op with
    | MatchSimple | MatchPush _ | MatchPop | MatchReplace _ -> trace1
    | WrapperPush _ | WrapperPop | WrapperSimple | MatchDroppable | WrapperReplace _
    | InitializationPush _ | InitializationPop | Initialization -> (op1, facts1) :: trace1

let adapt_stack op stack =
    match op with
    | MatchPush mode | WrapperPush mode | InitializationPush mode -> mode :: stack
    | MatchReplace mode | WrapperReplace mode -> mode :: List.tl stack
    | MatchPop | WrapperPop | InitializationPop -> List.tl stack
    | MatchSimple | WrapperSimple | Initialization | MatchDroppable -> stack

let extend_matching op op1 op2 matching =
    match op with
    | MatchSimple | MatchPush _ | MatchReplace _ | MatchPop -> Pair(op1, op2) :: matching
    | WrapperSimple | WrapperPush _ | WrapperPop | WrapperReplace _ -> Wrap op2 :: matching
    | Initialization | InitializationPush _ | InitializationPop | MatchDroppable -> Init op2 :: matching

let collect_object_references { rt2 = { TraceTypes.objs } } facts id =
    ExtArray.get objs (Types.get_object_id id)
    |> Misc.StringMap.bindings
    |> List.map (fun (field, _) -> Reference.reference_of_fieldref (id, field) |> LocalFacts.make_versioned facts)

let collect_references matching_state facts obj = match obj with
        Types.OObject _ | Types.OFunction _ | Types.OOther _ ->
        collect_object_references matching_state facts (Types.objectid_of_jsval obj)
    | _ -> []

let perpetuate_initialisation_data matching_state (op, facts) =
    let open TraceTypes in
    let { initialisation_data = init_old } = matching_state in
    let init_new =
        match op with
        | RWrite { ref; oldref }
        when VersionReferenceSet.mem oldref init_old ->
            VersionReferenceSet.add ref init_old
        | RLiteral { value } ->
            List.fold_left (fun init ref -> VersionReferenceSet.add ref init)
                init_old (collect_references matching_state facts value)
        | RLocal { ref } ->
            VersionReferenceSet.add ref init_old
        | _ -> init_old
    in
    { matching_state with initialisation_data = init_new }

let detect_toString op1 matching_state = let open TraceTypes in match op1 with
    | RRead { ref; value } ->
        begin match (fst ref) with
            | Reference.Field (_, name) when name = "toString" ->
                { matching_state with
                    toString_data = value :: matching_state.toString_data }
            | _ -> matching_state
        end
    | _ -> matching_state

let performs_write = function
  | TraceTypes.RWrite _ -> true
  | _ -> false

let invalidate_cache op1 op2 matching_state =
  if performs_write op1 || performs_write op2 then
    { matching_state with objeq = ref IntIntMap.empty }
  else
    matching_state

let adapt_matching_state op op1 op2 matching_state =
    begin match op with
        | MatchSimple | MatchPush _ | MatchPop | MatchDroppable -> matching_state
        | _ -> perpetuate_initialisation_data matching_state op2
    end |> detect_toString (fst op1) |> invalidate_cache (fst op1) (fst op2)

let mark_blocked ({ known_blocked } as matching_state) trace1 trace2 stack =
    let key = (List.length trace1, List.length trace2) in
    let known_blocked_here =
        if IntIntMap.mem key known_blocked then IntIntMap.find key known_blocked else [] in
    let known_blocked_new =
        if List.mem stack known_blocked_here then known_blocked_here else stack :: known_blocked_here in
    matching_state.known_blocked <- IntIntMap.add key known_blocked_new known_blocked

let is_blocked { known_blocked } trace1 trace2 stack =
    let key = (List.length trace1, List.length trace2) in
    if IntIntMap.mem key known_blocked then List.mem stack (IntIntMap.find key known_blocked) else false

(** The matching engine itself. *)
let rec matching_engine matching_state trace1 trace2 stack =
    (* Short-circuit matching if we have shown this case to be blocked. *)
    if is_blocked matching_state trace1 trace2 stack then begin
        MatchTracesObserver.log_blocked_shared (List.length trace1) (List.length trace2) stack;
        None
    end else match trace1, trace2 with
        (* Consider the different cases of how much of each trace remains *)
        | ev1 :: trace1, ev2 :: trace2 ->
            (* Neither trace is exhausted - match one step using the main rules *)
            let id = MatchTracesObserver.log_node (fst ev1) (fst ev2) stack in
            let (ops, failure_details) =
                build_candidates matching_state ev1 ev2 (get_state stack) in
            MatchTracesObserver.log_failure id failure_details ;
            apply_first_working id matching_state ev1 ev2 trace1 trace2 stack ops
        | _ :: _, [] ->
            (* The original trace has events, while the transformed trace is exhausted -
             * this means there is no match, because this is clearly not a subtrace. *)
            MatchTracesObserver.log_xfrm_consumed (List.map fst trace1);
            None
        | [], trace2 ->
            (* The original trace is exhausted. For a successful matching,
             * the modified trace must consist of initialisation events and
             * leave an empty stack. *)
            match can_be_added_as_initialisation matching_state trace2 stack with
            | None ->
                MatchTracesObserver.log_orig_consumed_ok (List.map fst trace2) stack;
                Some (List.map (fun (op, _) -> Init op) trace2)
            | Some err ->
                MatchTracesObserver.log_orig_consumed_failed (List.map fst trace2) stack;
                None
and apply_first_working parent matching_state op1 op2 trace1 trace2 stack =
    (* Try to continue the match using one of the applicable matching rules. *)
    function
    | [] ->
        None
    | matchop :: matchops ->
        MatchTracesObserver.log_edge parent matchop;
        let matching_state_adapted = adapt_matching_state matchop op1 op2 matching_state
        and trace1_adapted = adapt_first matchop op1 trace1
        and stack_adapted = adapt_stack matchop stack in
        match matching_engine matching_state_adapted trace1_adapted trace2 stack_adapted with
        | Some matching ->
            Some (extend_matching matchop (fst op1) (fst op2) matching)
        | None ->
            mark_blocked matching_state trace1_adapted trace2 stack_adapted;
            apply_first_working parent matching_state op1 op2 trace1 trace2 stack matchops

let match_traces rt1 rt2 =
    let open TraceTypes in
    matching_engine
        { rt1; rt2;
            objeq = ref IntIntMap.empty;
            initialisation_data = VersionReferenceSet.empty;
            toString_data = [];
            nonequivalent_functions = Misc.IntIntSet.empty;
            known_blocked = IntIntMap.empty
        } rt1.trace rt2.trace []
