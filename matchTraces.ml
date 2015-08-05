open Richtrace
open Trace
open Reference
open LocalFacts
open Misc
open MatchObjects
open MatchOperations
open MatchTypes

(**
* A helper for candidate generators.
*)
let add_objeq op objeq cands =
    (cands, if is_write op then IntIntMap.empty else objeq )

let rules_toplevel =
    [
    ([MatchSides; MayMatchSimple; IsToplevel], MatchSimple);
    ([MatchSides; MatchCallInt], MatchPush Regular);
    ([MatchSides; MatchCallExt], MatchPush External);
    ([MatchCallToString], MatchPush ToString);
    ([MatchSides; MatchCallWrap], MatchPush Wrapper);
    ([MayInit; IsToplevel; IsNotFunction], Initialization);
    ([IsCallInt], InitializationPush Init)
    ]

let rules_regular =
    [
    ([MatchSides; MayMatchSimple], MatchSimple);
    ([MatchSides; MatchCallInt], MatchPush Regular);
    ([MatchSides; MatchCallExt], MatchPush External);
    ([MatchCallToString], MatchPush ToString);
    ([MatchSides; MatchCallWrap], MatchPush Wrapper);
    ([MatchSides; IsExit], MatchPop)
    ]

let rules_wrap =
    [
    ([IsCallInt], WrapperPush Regular);
    ([IsCallInt], WrapperPush Wrapper);
    ([IsExit], WrapperPop);
    ([MayInsertInWrapSimple], WrapperSimple)
    ]

let rules_toString =
    [
    ([IsCallInt], WrapperPush ToString);
    ([IsExit], WrapperPop);
    ([IsUnobservable], WrapperSimple)
    ]

let rules_external =
    [ ([MatchSides; IsExit], MatchPop) ]

let rules_init =
    [ ([MayInit], Initialization);
    ([IsCallInt], InitializationPush Init);
    ([IsExit], InitializationPop) ]

let interpret_rules (rules: (condition list * match_operation) list) matching_state op1 op2 =
    let (objeq, match12) = match_operations matching_state op1 op2 in
    let interpret_cond = function
        | MatchSides -> match12
        | MayMatchSimple -> may_insert_in_matching_simple op2
        | MatchCallInt -> is_matching_internal_call matching_state op1 op2
        | MatchCallExt -> is_matching_external_call matching_state op1 op2
        | MatchCallToString -> is_matching_toString_call matching_state op1 op2
        | MatchCallWrap -> may_be_wrapper_entry matching_state op1 op2
        | MayInit -> may_insert_in_init matching_state op2
        | IsToplevel -> is_toplevel op2 |> explain NotToplevel
        | IsNotFunction -> is_not_function op2 |> explain NotFunction
        | IsExit -> is_post_exit op2 |> explain NotExit
        | IsCallInt -> is_internal_call matching_state.rt2 op2
        | IsUnobservable -> is_unobservable op2 |> explain Observable
        | MayInsertInWrapSimple -> may_insert_in_wrap_simple matching_state op2 in
    let interpret_conds conds =
      conds
      |> List.map (fun c -> match interpret_cond c with Some reason -> [(c, reason)] | None -> [])
      |> List.flatten
    and split = List.partition (function ([], _) -> true | _ -> false) in
    rules
    |> List.map (fun (cond, res) -> (interpret_conds cond, res))
    |> split
    |> fun (applicable, not_applicable) ->
       (applicable |> List.map snd |> add_objeq op2 objeq, not_applicable)

let build_candidates matching_state op1 op2 state =
    let find_rules = function
        | InToplevel -> rules_toplevel
        | InRegular -> rules_regular
        | InWrap -> rules_wrap
        | InToString -> rules_toString
        | InExternal -> rules_external
        | InInit -> rules_init
    in
    interpret_rules (find_rules state) matching_state op1 op2

(** Pretty-printers for matching state and match operations *)
let pp_matching_state pp { rt1; rt2; facts1; facts2; objeq; initialisation_data; toString_data } =
    Format.fprintf pp "..."
let pp_print_mode pp = function
    | Regular -> Format.pp_print_string pp "regular"
    | Wrapper -> Format.pp_print_string pp "wrap"
    | External -> Format.pp_print_string pp "external"
    | ToString -> Format.pp_print_string pp "toString"
    | Init -> Format.pp_print_string pp "init"

let pp_match_operation pp = function
    | Initialization -> Format.pp_print_string pp "init"
    | WrapperSimple -> Format.pp_print_string pp "wrap"
    | WrapperPop -> Format.pp_print_string pp "wrap, pop"
    | WrapperPush m -> Format.fprintf pp "wrap, push %a" pp_print_mode m
    | MatchSimple -> Format.pp_print_string pp "match"
    | MatchPop -> Format.pp_print_string pp "match, pop"
    | MatchPush m -> Format.fprintf pp "match, push %a" pp_print_mode m
    | InitializationPush m -> Format.fprintf pp "init, push %a" pp_print_mode m
    | InitializationPop -> Format.pp_print_string pp "init, pop"

(**
* Helpers for the matching engine.
*)
let get_state = function
    | Wrapper :: _ -> InWrap
    | Regular :: _ -> InRegular
    | External :: _ -> InExternal
    | ToString :: _ -> InToString
    | Init :: _ -> InInit
    | [] -> InToplevel

let can_be_added_as_initialisation matching_state trace stack =
  if get_state stack <> InToplevel then Some NotAtToplevel
  else if List.for_all (fun (op, facts) ->
      may_insert_in_init { matching_state with facts2 = facts } op = None) trace
  then None else Some NotInitCode

let adapt_first op op1 facts1 trace1 =
    match op with
    | MatchSimple | MatchPush _ | MatchPop -> trace1
    | _ -> (op1, facts1) :: trace1

let adapt_stack op stack =
    match op with
    | MatchPush mode | WrapperPush mode | InitializationPush mode -> mode :: stack
    | MatchPop | WrapperPop | InitializationPop -> List.tl stack
    | MatchSimple | WrapperSimple | Initialization -> stack

let extend_matching op op1 op2 matching =
    match op with
    | MatchSimple | MatchPush _ | MatchPop -> Pair(op1, op2) :: matching
    | WrapperSimple | WrapperPush _ | WrapperPop -> Wrap op2 :: matching
    | Initialization | InitializationPush _ | InitializationPop -> Init op2 :: matching

let collect_object_references { facts2 = facts; rt2 = { objs } } id =
    objs.(id)
    |> StringMap.bindings
    |> List.map (fun (field, _) -> reference_of_fieldref (id, field) |> make_versioned facts)

let collect_references matching_state = function
    | OObject id -> collect_object_references matching_state id
    | OFunction(id, _) -> collect_object_references matching_state id
    | OOther(_, id) -> collect_object_references matching_state id
    | _ -> []

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

let rec matching_engine matching_state trace1 trace2 stack =
    match trace1, trace2 with
    | (op1, facts1) :: trace1, (op2, facts2) :: trace2 ->
        let id = MatchTracesObserver.log_node op1 op2 stack in
        let matching_state' = { matching_state with facts1; facts2 } in
        let ((ops, objeq), failure_details) =
            build_candidates matching_state' op1 op2 (get_state stack) in
        MatchTracesObserver.log_failure id failure_details;
        apply_first_working id { matching_state' with objeq } op1 op2 trace1 trace2 stack ops
    | _ :: _, [] ->
        MatchTracesObserver.log_xfrm_consumed (List.map fst trace1);
        None
    | [], trace2 ->
        match can_be_added_as_initialisation matching_state trace2 stack with
            | None ->
              MatchTracesObserver.log_orig_consumed_ok (List.map fst trace2) stack;
              Some (List.map (fun (op, _) -> Init op) trace2)
            | Some err ->
              MatchTracesObserver.log_orig_consumed_failed (List.map fst trace2) stack;
              None
and apply_first_working parent matching_state op1 op2 trace1 trace2 stack =
    function
    | [] ->
      None
    | op :: ops ->
        MatchTracesObserver.log_edge parent op;
        match
        matching_engine
            (adapt_matching_state op op1 op2 matching_state)
            (adapt_first op op1 matching_state.facts1 trace1)
            trace2
            (adapt_stack op stack)
        with
        | Some matching ->
            Some (extend_matching op op1 op2 matching)
        | None ->
            apply_first_working parent matching_state op1 op2 trace1 trace2 stack ops

let match_traces rt1 rt2 =
    matching_engine
        { rt1; rt2;
            facts1 = empty_local_facts;
            facts2 = empty_local_facts;
            objeq = IntIntMap.empty;
            initialisation_data = VersionReferenceSet.empty;
            toString_data = []
        } rt1.trace rt2.trace []
