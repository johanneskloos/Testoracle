open Richtrace
open Types
open Reference
open LocalFacts
open Misc
open MatchObjects
open MatchOperations
open MatchTypes

(**
* A helper for candidate generators.
*)


let interpret_rules (rules: MatchRules.match_rules) matching_state op1 op2 =
    let interpret_conds conds =
        conds
        |> List.map (fun c -> match interpret_cond matching_state op1 op2 c with Some reason -> [(c, reason)] | None -> [])
        |> List.flatten
    and split = List.partition (function ([], _) -> true | _ -> false) in
    rules
    |> List.map (fun (cond, res) -> (interpret_conds cond, res))
    |> split
    |> fun (applicable, not_applicable) ->
        (applicable |> List.map snd, not_applicable)

let build_candidates matching_state op1 op2 state =
    interpret_rules (MatchRules.find_rules state) matching_state op1 op2

(**
* Helpers for the matching engine.
*)
let can_be_added_as_initialisation matching_state trace stack =
    if get_state stack <> InToplevel then Some NotAtToplevel
    else if List.for_all (fun ev ->
                may_insert_in_init matching_state ev = None) trace
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

let collect_object_references { rt2 = { objs } } facts id =
    objs.(get_object_id id)
    |> StringMap.bindings
    |> List.map (fun (field, _) -> reference_of_fieldref (id, field) |> make_versioned facts)

let collect_references matching_state facts obj = match obj with
        OObject _ | OFunction _ | OOther _ ->
        collect_object_references matching_state facts (objectid_of_jsval obj)
    | _ -> []

let perpetuate_initialisation_data matching_state (op, facts) =
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

let detect_toString op1 matching_state = match op1 with
    | RRead { ref; value } ->
        begin match (fst ref) with
            | Field (_, name) when name = "toString" ->
                { matching_state with
                    toString_data = value :: matching_state.toString_data }
            | _ -> matching_state
        end
    | _ -> matching_state

let performs_write = function
  | RWrite _ -> true
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

(** Merge back results that need to be propagated. *)
let merge
    { rt1; rt2; objeq; initialisation_data; toString_data }
    { nonequivalent_functions; known_blocked } =
    { rt1; rt2; objeq; initialisation_data; toString_data; nonequivalent_functions; known_blocked }

let mark_blocked ({ known_blocked } as matching_state) trace1 trace2 stack =
    let key = (List.length trace1, List.length trace2) in
    let known_blocked_here =
        if IntIntMap.mem key known_blocked then IntIntMap.find key known_blocked else [] in
    let known_blocked_new =
        if List.mem stack known_blocked_here then known_blocked_here else stack :: known_blocked_here in
    { matching_state with known_blocked = IntIntMap.add key known_blocked_new known_blocked }

let is_blocked { known_blocked } trace1 trace2 stack =
    let key = (List.length trace1, List.length trace2) in
    if IntIntMap.mem key known_blocked then List.mem stack (IntIntMap.find key known_blocked) else false

(** The matching engine itself. *)
let rec matching_engine matching_state trace1 trace2 stack =
    (* Short-circuit matching if we have shown this case to be blocked. *)
    if is_blocked matching_state trace1 trace2 stack then begin
        MatchTracesObserver.log_blocked_shared (List.length trace1) (List.length trace2) stack;
        (None, matching_state)
    end else match trace1, trace2 with
        | ev1 :: trace1, ev2 :: trace2 ->
            let id = MatchTracesObserver.log_node (fst ev1) (fst ev2) stack in
            let (ops, failure_details) =
                build_candidates matching_state ev1 ev2 (get_state stack) in
            MatchTracesObserver.log_failure id failure_details ;
            apply_first_working id matching_state ev1 ev2 trace1 trace2 stack ops
        | _ :: _, [] ->
            MatchTracesObserver.log_xfrm_consumed (List.map fst trace1);
            (None, matching_state)
        | [], trace2 ->
            match can_be_added_as_initialisation matching_state trace2 stack with
            | None ->
                MatchTracesObserver.log_orig_consumed_ok (List.map fst trace2) stack;
                (Some (List.map (fun (op, _) -> Init op) trace2), matching_state)
            | Some err ->
                MatchTracesObserver.log_orig_consumed_failed (List.map fst trace2) stack;
                (None, matching_state)
and apply_first_working parent matching_state op1 op2 trace1 trace2 stack =
    function
    | [] ->
        (None, matching_state)
    | op :: ops ->
        MatchTracesObserver.log_edge parent op;
        let matching_state_adapted = adapt_matching_state op op1 op2 matching_state
        and trace1_adapted = adapt_first op op1 trace1
        and stack_adapted = adapt_stack op stack in
        let (result, matching_state') =
            matching_engine matching_state_adapted
                trace1_adapted trace2 stack_adapted in
        let matching_state_merged = merge matching_state matching_state' in match result with
        | Some matching ->
            (Some (extend_matching op (fst op1) (fst op2) matching), matching_state_merged)
        | None ->
            apply_first_working parent
                (mark_blocked matching_state_merged trace1_adapted trace2 stack_adapted)
                op1 op2 trace1 trace2 stack ops

let match_traces rt1 rt2 =
    matching_engine
        { rt1; rt2;
            objeq = ref IntIntMap.empty;
            initialisation_data = VersionReferenceSet.empty;
            toString_data = [];
            nonequivalent_functions = Misc.IntIntSet.empty;
            known_blocked = Misc.IntIntMap.empty
        } rt1.trace rt2.trace []
    |> fst
