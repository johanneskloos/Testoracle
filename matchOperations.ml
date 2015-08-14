open Richtrace
open Trace
open Reference
open LocalFacts
open Misc
open MatchObjects
open MatchTypes

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
    objeq: objeq;
    initialisation_data: VersionReferenceSet.t;
    toString_data: jsval list;
    nonequivalent_functions: IntIntSet.t;
    known_blocked: match_mode list list IntIntMap.t
}


(**
* Check if two alias sources match.
*)
type mismatch =
  | DifferentType
  | DifferentObjects of string * obj_match_failure
  | DifferentArguments
  | DifferentValues of string
  | DifferentOperations
  | OtherOperation
  | NotToString
  | NotInitData
  | NotFunctionUpdate
  | NotInitCode
  | NotSimpleMatchable
  | NotWrapCode
  | NotToStringCode
  | ExternalCall
  | InternalCall
  | NotLiterallyEqual
  | LiterallyEqual
  | NotToplevel
  | NotFunction
  | NotExit
  | Observable
  | NotAtToplevel

type 'a comparator = matching_state -> 'a -> 'a -> objeq * mismatch option
type predicate = matching_state -> rich_operation -> mismatch option
type call_comparator = matching_state -> rich_operation -> rich_operation -> mismatch option

let match_source { rt1; rt2; facts1; facts2; objeq; nonequivalent_functions } src1 src2 =
    match src1, src2 with
    | Argument i1, Argument i2 -> (objeq, (if i1 = i2 then None else Some DifferentArguments))
    | With r1, With r2 ->
        begin match
          MatchObjects.match_refs "source" rt1 rt2 facts1 facts2 nonequivalent_functions r1 r2 objeq
        with
          | (objeq, None) -> (objeq, None)
          | (objeq, Some (which, reason)) -> (objeq, Some (DifferentObjects (which, reason)))
        end
    | _ -> (objeq, Some DifferentType)

let (&&&) (objeq, cond) check =
  match cond with
    | Some err -> (objeq, Some err)
    | None -> check objeq
let (&&+) (objeq, cond) check =
  match cond with
    | Some err -> (objeq, Some err)
    | None -> (objeq, check)
let (!!) objeq = (objeq, None)
let (|||) res1 res2 = match res1 with Some _ -> res1 | None -> res2
let (!?) = function Some _ -> false | None -> true

let wrap_reason = function
  | (objeq, Some (name, reason)) -> (objeq, Some (DifferentObjects (name, reason)))
  | (objeq, None) -> (objeq, None)
 let explain reason = function true -> None | false -> Some reason

(**
* Check if two operations match. This does not take
* any stack state into account; it purely matches the arguments.
*)
let match_operations matching_state op1 op2 =
    let { rt1; rt2; facts1; facts2; objeq; nonequivalent_functions } = matching_state in
    let check name v1 v2 oe =
      MatchObjects.match_values name rt1 rt2 facts1 facts2 nonequivalent_functions v1 v2 oe |> wrap_reason
    and check_ref name r1 r2 oe =
      MatchObjects.match_refs name rt1 rt2 facts1 facts2 nonequivalent_functions r1 r2 oe |> wrap_reason
    and check_eq name x1 x2 objeq =
        if x1 = x2 then (objeq, None) else (objeq, Some (DifferentValues name)) in
    begin match op1, op2 with
        | RFunPre { f = f1; base = base1; args = args1; call_type = ct1 }, RFunPre { f = f2; base = base2; args = args2; call_type = ct2 } ->
            !!objeq &&& check "f" f1 f2 &&& check "base" base1 base2 &&& check "args" args1 args2 &&& check_eq "call type" ct1 ct2
        | RFunPost { f = f1; base = base1; args = args1; result = res1 }, RFunPost { f = f2; base = base2; args = args2; result = res2 } ->
            !!objeq &&& check "f" f1 f2 &&& check "base" base1 base2 &&& check "args" args1 args2 &&& check "res" res1 res2
        | RLiteral { value = val1; hasGetterSetter = hgs1 }, RLiteral { value = val2; hasGetterSetter = hgs2 } ->
            !!objeq &&& check "val" val1 val2 &&& check_eq "hgs" hgs1 hgs2
        | RLocal { name = name1; ref = ref1 }, RLocal { name = name2; ref = ref2 } ->
            !!objeq &&& check_ref "ref" ref1 ref2 &&& check_eq "name" name1 name2
        | RAlias { name = name1; ref = ref1; source = src1 }, RAlias { name = name2; ref = ref2; source = src2 } ->
            match_source matching_state src1 src2 &&& check_ref "ref" ref1 ref2 &&& check_eq "name" name1 name2
        | RRead { ref = ref1; value = val1 }, RRead { ref = ref2; value = val2 } ->
            !!objeq &&& check_ref "ref" ref1 ref2 &&& check "val" val1 val2
        | RWrite { ref = ref1; oldref = oref1; value = val1; success = succ1 }, RWrite { ref = ref2; oldref = oref2; value = val2; success = succ2 } ->
            !!objeq &&& check_ref "ref" ref1 ref2 &&& check "val" val1 val2 &&& check_ref "oref" oref1 oref2
        | RForIn val1, RForIn val2 -> !! objeq &&& check "val" val1 val2
        | RReturn val1, RReturn val2 -> !!objeq &&& check "val" val1 val2
        | RThrow val1, RThrow val2 -> !!objeq &&& check "val" val1 val2
        | RWith val1, RWith val2 -> !!objeq &&& check "val" val1 val2
        | RFunEnter { f = f1; this = this1; args = args1 }, RFunEnter { f = f2; this = this2; args = args2 } ->
            !!objeq &&& check "f" f1 f2 &&& check "this" this1 this2 &&& check "args" args1 args2
        | RFunExit { ret = ret1; exc = exc1 }, RFunExit { ret = ret2; exc = exc2 } ->
            !!objeq &&& check "ret" ret1 ret2 &&& check "ext" exc1 exc2
        | RScriptEnter, RScriptEnter -> !!objeq
        | RScriptExit, RScriptExit -> !!objeq
        | RScriptExc val1, RScriptExc val2 -> !!objeq &&& check "val" val1 val2
        | RBinary { op = op1; left = left1; right = right1; result = result1 }, RBinary { op = op2; left = left2; right = right2; result = result2 } ->
            !!objeq &&& check "left" left1 left2 &&& check "right" right1 right2 &&& check "result" result1 result2 &&& check_eq "op" op1 op2
        | RUnary { op = op1; arg = arg1; result = result1 }, RUnary { op = op2; arg = arg2; result = result2 } ->
            !!objeq &&& check "arg" arg1 arg2 &&& check "result" result1 result2 &&& check_eq "op" op1 op2
        | REndExpression, REndExpression -> !!objeq
        | RConditional val1, RConditional val2 -> !!objeq &&& check "val" val1 val2
        | _, _ -> (objeq, Some DifferentOperations)
    end

(**
* Predicates that check whether a function can be used in a specific
* context.
*
* These two predicates classify certain types of writes.
*)
let is_instrumentation_write { initialisation_data } = function
    | RWrite { oldref } ->
      if VersionReferenceSet.mem oldref initialisation_data then None else Some NotInitData
    | _ ->
       Some OtherOperation

let is_function_update { rt2 } = function
    | RWrite { ref } ->
        begin try match VersionReferenceMap.find ref rt2.points_to with
            | OFunction _ ->
               None
            | _ ->
               Some NotFunctionUpdate
            with Not_found -> Format.eprintf "%a not found in is_function_update@." pp_reference; raise Not_found 
        end
    | _ ->
      Some OtherOperation

let is_function_update func =
    try is_function_update func with Not_found -> failwith "is_function_update failed"

(**
* The folloing three predicates detect operations that can
* be used without any special handling in various contexts. *)
let may_insert_in_init matching_state op =
    (is_unobservable op || !?(is_instrumentation_write matching_state op) ||
        !?(is_function_update matching_state op))
        |> explain NotInitCode

let may_insert_in_matching_simple op =
    (is_unobservable op || is_write op || is_throw op)
    |> explain NotSimpleMatchable

let may_insert_in_wrap_simple matching_state op =
    !?(may_insert_in_init matching_state op) (* for now *)
    |> explain NotWrapCode

let may_insert_in_toString_simple op = is_unobservable op |> explain NotToStringCode

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
        facts1; facts2; nonequivalent_functions = noneq
    } = { funs1; funs2; pt1; pt2; facts1; facts2; noneq }

let is_internal_call_impl { funcs } f =
    try
        begin match funcs.(f) with Local _ -> None | External _ -> Some ExternalCall end
    with
    | e -> Format.eprintf "trying to get %d from %a@." f
            (FormatHelper.pp_print_array pp_funcspec) funcs; raise e

let is_internal_call rt = function
    | RFunPre { f = OFunction(_, f) } -> is_internal_call_impl rt f
    | _ -> Some OtherOperation

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
        if match_functions (convert matching_data) f1 f2 = literally_equal then
          if !?(is_internal_call_impl matching_data.rt1 f1) = local then
            None
          else
            if local then Some ExternalCall else Some InternalCall
        else
          if literally_equal then Some NotLiterallyEqual else Some LiterallyEqual
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
let is_matching_toString_call matching_data op1 op2 =
    match op1, op2 with
    | RFunPre { f = f1; base = this1 },
    RFunPre { f = f2; base = this2 } ->
        let { rt1; rt2; facts1; facts2; objeq; toString_data; nonequivalent_functions } = matching_data in
        begin match
            MatchObjects.match_values "this" rt1 rt2 facts1 facts2 nonequivalent_functions this1 this2 objeq
            with
            | (_, Some (name, failure)) -> Some (DifferentObjects (name, failure))
            | (_, None) -> if List.mem f1 toString_data then None else Some NotToString
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
*
*)
let is_not_function = function
    | RFunPre _ | RFunPost _ | RFunEnter _ | RFunExit _ | RReturn _ -> false
    | _ -> true
