open TraceTypes
open Types
open MatchTypes

(**
 * Basic predicates.
 *
 * A small wrapper provides a convenient interface. *)
let explain_wrapper reason pred (op, _): MatchTypes.mismatch option = if pred op then None else Some reason
let explain_otherop = explain_wrapper OtherOperation

(**
 * The following properties of operations are decidable without using any extra
 * information.
 *
 * A function is unobservable if it neither changes global state nor
 * calls external functions.
*)

let is_unobservable = explain_wrapper Observable (function
    | RForIn _ | RLocal _ | RAlias _ | RRead _ | RReturn _
    | RWith _ | RScriptEnter | RScriptExit | RScriptExc _ | RBinary _
    | RUnary _ | REndExpression | RConditional _ | RLiteral _
    | RFunEnter _ | RFunPost _ | RCatch _ ->
      true
    | RFunPre _ | RWrite _ | RFunExit _ | RThrow _ ->
      false)

(**
 * Some operations just don't make sense at the top level.
*)
let is_toplevel = explain_wrapper NotAtToplevel (function
    | RFunEnter _ | RFunExit _ | RReturn _ -> false
    | _ -> true)

let is_throw = explain_otherop (function
    | RThrow _ -> true
    | _ -> false)

(**
 * Some obvious classifications.
*)
let is_write = explain_otherop (function RWrite _ -> true | _ -> false)
let is_exit = explain_otherop (function RFunExit _ -> true | _ -> false)
let is_post_exit = explain_otherop (function RFunPost _ -> true | _ -> false)
let is_enter = explain_otherop (function RFunEnter _ -> true | _ -> false)
let is_use_strict = explain_otherop (function RLiteral { value = OString "use strict" } -> true | REndExpression -> true | _ -> false)
let is_catch = explain_otherop (function RCatch _ -> true | _ -> false)

(**
 * Functions dependening on the current matching state.
 *
 * We summarize the matching state in a record.
*)

type 'a comparator = matching_state -> 'a -> 'a -> mismatch option
type predicate = matching_state -> rich_event -> mismatch option
type call_comparator = matching_state -> rich_event -> rich_event -> mismatch option
type simple_predicate = rich_event -> mismatch option

let match_source { rt1; rt2; objeq; nonequivalent_functions } facts1 src1 facts2 src2 =
  match src1, src2 with
  | Argument i1, Argument i2 -> (if i1 = i2 then None else Some DifferentArguments)
  | With r1, With r2 ->
    begin match
        MatchObjects.match_refs "source" rt1 rt2 facts1 facts2 nonequivalent_functions r1 r2 objeq
      with
      | None -> None
      | Some (which, reason) -> Some (DifferentObjects (which, reason))
    end
  | _ -> Some DifferentType

let (&&&) cond check =
  match cond with
  | Some err -> Some err
  | None -> check
let (|||) res1 res2 =
  match res1, res2 with
  | None, _ -> None
  | _, None -> None
  | Some res1, Some res2 -> Some (And (res1, res2))

let wrap_reason = function
  | Some (name, reason) -> Some (DifferentObjects (name, reason))
  | None -> None

let is_arguments = function
  | (Reference.Variable (_, "arguments"), _) -> true
  | _ -> false

let is_this = function
  | (Reference.Variable (_, "this"), _) -> true
  | _ -> false

(**
 * Check if two operations match. This does not take
 * any stack state into account; it purely matches the arguments.
*)
let match_operations matching_state (op1, facts1) (op2, facts2) =
  let { rt1; rt2; objeq; nonequivalent_functions } = matching_state in
  let check name v1 v2 =
    MatchObjects.match_values name rt1 rt2 facts1 facts2 nonequivalent_functions v1 v2 objeq |> wrap_reason
  and check_ref name r1 r2 =
    MatchObjects.match_refs name rt1 rt2 facts1 facts2 nonequivalent_functions r1 r2 objeq |> wrap_reason
  and check_eq name x1 x2 =
    if x1 = x2 then None else Some (DifferentValues name) in
  begin match op1, op2 with
    | RFunPre { f = f1; base = base1; args = args1; call_type = ct1 }, RFunPre { f = f2; base = base2; args = args2; call_type = ct2 } ->
      check "f" f1 f2 &&& check "base" base1 base2 &&& check "args" args1 args2 &&& check_eq "call type" ct1 ct2
    | RFunPost { f = f1; base = base1; args = args1; result = res1 }, RFunPost { f = f2; base = base2; args = args2; result = res2 } ->
      check "f" f1 f2 &&& check "base" base1 base2 &&& check "args" args1 args2 &&& check "res" res1 res2
    | RLiteral { value = val1; hasGetterSetter = hgs1 }, RLiteral { value = val2; hasGetterSetter = hgs2 } ->
      check "val" val1 val2 &&& check_eq "hgs" hgs1 hgs2
    | RLocal { name = name1; ref = ref1 }, RLocal { name = name2; ref = ref2 } ->
      (* Not checking ref equivalence; the initial value is provided by  *)
      (* a write later, so we don't need to handle it here, and it       *)
      (* breaks the function-declaring-function pattern.                 *)
      check_eq "name" name1 name2
    | RCatch { name = name1; ref = ref1 }, RCatch { name = name2; ref = ref2 } ->
      (* Not checking ref equivalence; the initial value is provided by  *)
      (* a write later, so we don't need to handle it here, and it       *)
      (* breaks the function-declaring-function pattern.                 *)
      check_eq "name" name1 name2
    | RAlias { name = name1; ref = ref1; source = src1 }, RAlias { name = name2; ref = ref2; source = src2 } ->
      match_source matching_state facts1 src1 facts2 src2 &&& check_ref "ref" ref1 ref2 &&& check_eq "name" name1 name2
    | RRead { ref = ref1; value = val1 }, RRead { ref = ref2; value = val2 } ->
      check_ref "ref" ref1 ref2 &&& check "val" val1 val2
    | RWrite { ref = ref1; oldref = oref1; value = val1; success = succ1 }, RWrite { ref = ref2; oldref = oref2; value = val2; success = succ2 } ->
      (* Don't check oref; we probably need to check a proper match      *)
      (* between ref.                                                    *)
        if !MatchFlags.lax_args && is_arguments ref1 && is_arguments ref2 then
          None
        else if !MatchFlags.lax_this && is_this ref1 && is_this ref2 then
          None
        else
          check_ref "ref" ref1 ref2 &&& check "val" val1 val2
    | RForIn val1, RForIn val2 -> check "val" val1 val2
    | RReturn val1, RReturn val2 -> check "val" val1 val2
    | RThrow val1, RThrow val2 -> check "val" val1 val2
    | RWith val1, RWith val2 -> check "val" val1 val2
    | RFunExit { ret = ret1; exc = exc1 }, RFunExit { ret = ret2; exc = exc2 } ->
      check "ret" ret1 ret2 &&& check "ext" exc1 exc2
    | RScriptEnter, RScriptEnter -> None
    | RScriptExit, RScriptExit -> None
    | RScriptExc val1, RScriptExc val2 -> check "val" val1 val2
    | RBinary { op = op1; left = left1; right = right1; result = result1 }, RBinary { op = op2; left = left2; right = right2; result = result2 } ->
      check "left" left1 left2 &&& check "right" right1 right2 &&& check "result" result1 result2 &&& check_eq "op" op1 op2
    | RUnary { op = op1; arg = arg1; result = result1 }, RUnary { op = op2; arg = arg2; result = result2 } ->
      check "arg" arg1 arg2 &&& check "result" result1 result2 &&& check_eq "op" op1 op2
    | REndExpression, REndExpression -> None
    | RConditional val1, RConditional val2 -> check "val" val1 val2
    | _, _ -> Some DifferentOperations
  end

(**
 * Predicates that check whether a function can be used in a specific
 * context.
 *
 * These two predicates classify certain types of writes.
*)
let is_instrumentation_write { initialisation_data } = explain_wrapper NotInitData (function
    | RWrite { oldref } -> Reference.VersionedReferenceSet.mem oldref initialisation_data
    | _ -> false)

let is_function_update { rt2 } = explain_wrapper NotFunctionUpdate (function
    | RWrite { ref } ->
      begin try match Reference.VersionedReferenceMap.find ref rt2.points_to with
        | OFunction _ -> true
        | _ -> false
        with Not_found ->
          Format.eprintf "%a not found in is_function_update@." Reference.pp_versioned_reference ref;
          Format.eprintf "@[<v 2>points-to map contains:@ %a@]@." Reference.pp_points_to_map rt2.points_to;
          failwith "is_function_update failed"
      end
    | _ -> false)

let is_function_property_update = explain_wrapper NotFunctionUpdate (function
    | RWrite { ref = (Reference.Field (Function _, _), _) } -> true
    | _ -> false)
let is_uninitialized_dummy_write = explain_wrapper OtherOperation (function
    | RWrite { ref; oldref; value = OUndefined } when ref = oldref -> true
    | _ -> false)

(**
 * The folloing three predicates detect operations that can
 * be used without any special handling in various contexts. *)
let may_insert_in_init matching_state op =
  (is_unobservable op |||
   is_instrumentation_write matching_state op |||
   is_function_update matching_state op |||
   is_uninitialized_dummy_write op |||
   is_function_property_update op)
(*|> better_explanation NotInitCode*)

let may_insert_in_matching_simple op =
  (is_unobservable op |||
   is_write op |||
   is_throw op)
(*|> better_explanation NotSimpleMatchable*)

let may_insert_in_wrap_simple matching_state op =
  may_insert_in_init matching_state op (* for now *)

let may_insert_in_toString_simple op = is_unobservable op

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
      nonequivalent_functions = noneq
    } facts1 facts2 = let open MatchObjects in { funs1; funs2; pt1; pt2; facts1; facts2; noneq }

let is_internal_call_impl { funcs } f =
  try
    begin match BatDynArray.get funcs f with External _ -> Some ExternalCall | _ -> None end
  with
  | e -> Format.eprintf "trying to get %d from %a@." f
           pp_functions funcs; raise e

let is_internal_call rt = function
  | (RFunPre { f = OFunction(_, f) }, _) -> is_internal_call_impl rt f
  | _ -> Some OtherOperation

(**
 * A generic matcher that wraps the annoying details.
 * The concrete matchers are implemented below.
 *
 * Note that the property of functions being local is checked
 * * only on the first function *!
*)
let is_matching_call may_be_literally_equal local matching_data op1 op2 =
  match op1, op2 with
  | (RFunPre { f = OFunction(_, f1) }, facts1),
    (RFunPre { f = OFunction(_, f2) }, facts2) ->
    let is_matching = match MatchObjects.match_functions (convert matching_data facts1 facts2) f1 f2 with
      | Some _ -> false | None -> true in
    if (not is_matching) || may_be_literally_equal then
      match is_internal_call_impl matching_data.rt1 f1, local with
      | Some _, false -> None
      | None, true -> None
      | Some _, true -> Some ExternalCall
      | None, false -> Some InternalCall
    else
      Some LiterallyEqual
  | _, _ -> Some OtherOperation

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
let is_matching_toString_call matching_data (op1, facts1) (op2, facts2) =
  match op1, op2 with
  | RFunPre { f = f1; base = this1 },
    RFunPre { f = f2; base = this2 } ->
    let { rt1; rt2; objeq; toString_data; nonequivalent_functions } = matching_data in
    begin match
        MatchObjects.match_values "this" rt1 rt2 facts1 facts2 nonequivalent_functions this1 this2 objeq
      with
      | Some (name, failure) -> Some (DifferentObjects (name, failure))
      |  None -> if List.mem f1 toString_data then None else Some NotToString
    end
  | _ -> Some OtherOperation

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
 * Note that RFunPost is fine.
*)
let is_not_function = explain_otherop (function
    | RFunPre _ | RFunEnter _ | RFunExit _ | RReturn _ -> false
    | _ -> true)

let is_matching_entry matching_data (op1, facts1) (op2, facts2) =
  let { rt1; rt2; objeq; toString_data; nonequivalent_functions } = matching_data in
  let match_val key obj1 obj2 =
    MatchObjects.match_values key rt1 rt2 facts1 facts2 nonequivalent_functions obj1 obj2 objeq |> wrap_reason in
  match op1, op2 with
  | RFunEnter { f = OFunction(_, f1); args = args1; this = this1 },
    RFunEnter { f = OFunction(_, f2); args = args2; this = this2 } ->
      begin match !MatchFlags.lax_args, !MatchFlags.lax_this with
        | true, true -> None
        | true, false -> match_val "this" this1 this2
        | false, true -> match_val "args" args1 args2
        | false, false -> match_val "args" args1 args2 &&& match_val "this" this1 this2
      end
  | _ -> Some NotEnter

(** Check if a call goes to a known higher-order function. *)
let is_call_to { funcs; objs; points_to } name: funpre -> bool = function
  | { f = OFunction(_, id); base } (*as rt*) ->
    begin
      let rec lookup base name = match name with
        | component :: rest ->
          lookup (StringMap.find component (BatDynArray.get objs (get_object base))).value rest
        | [] -> base
      in try
        let get path = lookup (OObject 0) path in
        let called_via path id' =
          match get path, base with
          | OFunction(_, id''), OFunction(_, id''') when id = id'' ->
            (*Format.eprintf "%d vs. %d@." id'' id';*) id''' = id'
          | OFunction _, _ -> (*Format.eprintf "Not an indirect call.@.";*) false
          | (v, _) -> Format.eprintf "Not a function?! Got %a@." pp_jsval v; false
        in match get name with
        | OFunction(_, id') ->
          (*Format.eprintf "Got implementation, id=%d@." id;*)
          id = id' ||
          called_via ["Function"; "prototype"; "call"] id' ||
          called_via ["Function"; "prototype"; "apply"] id'
        | _ -> Format.eprintf "No implementation found@."; false
      with Not_found -> Format.eprintf "Something not found@."; false
    end
  | _ -> Format.eprintf "Calling a non-function@."; false

let known_higher_order = [
  ["Array"; "from"];
  ["Array"; "prototype"; "each"];
  ["Array"; "prototype"; "filter"];
  ["Array"; "prototype"; "find"];
  ["Array"; "prototype"; "findIndex"];
  ["Array"; "prototype"; "forEach"];
  ["Array"; "prototype"; "map"];
  ["Array"; "prototype"; "reduce"];
  ["Array"; "prototype"; "reduceRight"];
  ["Array"; "prototype"; "some"];
  ["Array"; "prototype"; "sort"]
]

let match_higher_order { rt1; rt2 } op1 op2 =
  match op1, op2 with
  | RFunPre fp1, RFunPre fp2 ->
    if List.exists
        (fun name -> is_call_to rt1 name fp1 && is_call_to rt2 name fp2)
        known_higher_order then
      None
    else
      Some NotFunction (* Give better error later *)
  | _ -> Some OtherOperation

let is_fun_literal = explain_wrapper NotFunction (function
    | RLiteral { value = OFunction _ } -> true
    | _ -> false)

let is_local_decl = function
  | (RLocal _, _) -> None
  | _ -> Some OtherOperation

let is_fun_read = function
  | (RRead { value = OFunction _ }, _) -> None
  | _ -> Some OtherOperation

let is_end_of_expr = function
  | (REndExpression, _) -> None
  | _ -> Some OtherOperation

let is_alias_match (op1, _) (op2, _) =
  if !MatchFlags.arg_undef then begin
    match (op1, op2) with
      | (RAlias { name = name1; source = Argument _ },
         RLocal { name = name2 })
          when name1 = name2 -> None
      | (RLocal { name = name1 },
         RAlias { name = name2; source = Argument _ })
          when name1 = name2 -> None
      | _ -> Some DifferentOperations
  end else Some DifferentOperations

let is_alias_write_match (op1, _) (op2, _) =
  match op1, op2 with
    | RWrite { value = OUndefined; success = true; ref=ref1; oldref=oldref1 },
      RWrite { value = OUndefined; success = true; ref=ref2; oldref=oldref2 }
        when ref1 = oldref1 && ref2 = oldref2 ->
        None
    | _ -> Some DifferentOperations

(** DSL interface to matching functionality *)
let interpret_cond matching_state op1 op2 = function
  | MatchSides -> match_operations matching_state op1 op2
  | MayMatchSimple -> may_insert_in_matching_simple op2
  | MatchCallInt -> is_matching_internal_call matching_state op1 op2
  | MatchCallExt -> is_matching_external_call matching_state op1 op2
  | MatchCallToString -> is_matching_toString_call matching_state op1 op2
  | MatchCallWrap -> may_be_wrapper_entry matching_state op1 op2
  | MayInit -> may_insert_in_init matching_state op2
  | IsToplevel -> is_toplevel op2
  | IsNotFunction -> is_not_function op2
  | IsExit -> is_exit op2
  | IsPostExit -> is_post_exit op2
  | IsEnter -> is_enter op2
  | IsCallInt -> is_internal_call matching_state.rt2 op2
  | IsUnobservable -> is_unobservable op2
  | MayInsertInWrapSimple -> may_insert_in_wrap_simple matching_state op2
  | MatchEnter -> is_matching_entry matching_state op1 op2
  | UseStrictRHS -> is_use_strict op2
  | IsCatch -> is_catch op2
  | IsFunLiteral -> is_fun_literal op2
  | IsLocalDecl -> is_local_decl op2
  | IsFunRead -> is_fun_read op2
  | IsEndOfExpr -> is_end_of_expr op2
  | IsAliasMatch -> is_alias_match op1 op2
  | MatchAliasWrites -> is_alias_write_match op1 op2
