open Misc
open LocalFacts
open CalculateVersions
open CalculatePointsTo
open PointsTo
open Types
open Reference
open Cleantrace

type alias_source = Argument of int | With of versioned_reference
type rfunpre = { f: jsval; base: jsval; args: jsval; call_type: call_type }
type rfunpost = { f: jsval; base: jsval; args: jsval; result: jsval }
type rliteral = { value: jsval; hasGetterSetter: bool }
type rlocal = { name: string; ref: versioned_reference }
type ralias = { name: string; source: alias_source; ref: versioned_reference }
type rread = { ref: versioned_reference; value: jsval }
type rwrite = {
    ref: versioned_reference;
    oldref: versioned_reference;
    value: jsval;
    success: bool
}
type rbinary = { op: string; left: jsval; right: jsval; result: jsval }
type runary = { op: string; arg: jsval; result: jsval }
type rfunenter = { f: jsval; this: jsval; args: jsval }
type rfunexit = { ret: jsval; exc: jsval }
type rich_operation =
    | RFunPre of rfunpre
    | RFunPost of rfunpost
    | RLiteral of rliteral
    | RForIn of jsval
    | RLocal of rlocal
    | RCatch of rlocal
    | RAlias of ralias
    | RRead of rread
    | RWrite of rwrite
    | RReturn of jsval
    | RThrow of jsval
    | RWith of jsval
    | RFunEnter of rfunenter
    | RFunExit of rfunexit
    | RScriptEnter
    | RScriptExit
    | RScriptExc of jsval
    | RBinary of rbinary
    | RUnary of runary
    | REndExpression
    | RConditional of jsval

type rich_trace = (rich_operation * local_facts) list
type rich_tracefile = {
    funcs: functions;
    objs: objects;
    trace: rich_trace;
    globals: globals;
    globals_are_properties: bool;
    points_to: points_to_map
}

open Format

let pp_alias_source pp = function
    | Argument i -> fprintf pp "declaration of argument %d" i
    | With ref -> fprintf pp "with statement on %a" pp_versioned_reference ref

let pp_rich_operation pp = function
    | RFunPre { f; base; args; call_type } ->
        fprintf pp "RFunPre(f=%a, base=%a, args=%a, call_type=%a)"
            pp_jsval f pp_jsval base pp_jsval args pp_call_type call_type
    | RFunPost { f; base; args; result } ->
        fprintf pp "RFunPost(f=%a, base=%a, args=%a, result=%a)"
            pp_jsval f pp_jsval base pp_jsval args pp_jsval result
    | RLiteral { value; hasGetterSetter } ->
        fprintf pp "RLiteral(value=%a, hasGetterSetter=%B)" pp_jsval value hasGetterSetter;
    | RForIn obj -> fprintf pp "RForIn(value=%a)" pp_jsval obj
    | RLocal { name; ref } ->
        fprintf pp "RLocal(name=%s, ref=%a)" name pp_versioned_reference ref
    | RCatch { name; ref } ->
        fprintf pp "RCatch(name=%s, ref=%a)" name pp_versioned_reference ref
    | RAlias { name; source; ref } ->
        fprintf pp "RAlias(name=%s, ref=%a, source=%a)"
            name pp_versioned_reference ref pp_alias_source source
    | RRead { ref; value } ->
        fprintf pp "RRead(ref=%a, value=%a)"
            pp_versioned_reference ref pp_jsval value
    | RWrite { ref; value; oldref; success } ->
        fprintf pp "RWrite(ref=%a, value=%a, oldref=%a, success=%B"
            pp_jsval value pp_versioned_reference ref
            pp_versioned_reference oldref success
    | RReturn obj -> fprintf pp "RReturn(value=%a)" pp_jsval obj
    | RThrow obj -> fprintf pp "RThrow(value=%a)" pp_jsval obj
    | RWith obj -> fprintf pp "RWith(value=%a)" pp_jsval obj
    | RFunEnter { f; this; args } ->
        fprintf pp "RFunEnter(f=%a, this=%a, args=%a)"
            pp_jsval f pp_jsval this pp_jsval args
    | RFunExit { ret; exc } ->
        fprintf pp "RFunExit(ret=%a, exc=%a)" pp_jsval ret pp_jsval exc
    | RScriptEnter -> pp_print_string pp "RScriptEnter"
    | RScriptExit -> pp_print_string pp "RScriptExit"
    | RScriptExc e -> fprintf pp "RScriptExit(value=%a)" pp_jsval e
    | RBinary { op; left; right; result } ->
        fprintf pp "RBinary(left=%a, op=%s, right=%a, result=%a)"
            pp_jsval left op pp_jsval right pp_jsval result
    | RUnary { op; arg; result } ->
        fprintf pp "RUnary(op=%s, arg=%a, result=%a)" op pp_jsval arg pp_jsval result
    | REndExpression -> pp_print_string pp "REndExpression"
    | RConditional value -> fprintf pp "RConditional(value=%a)" pp_jsval value

let dump_facts = ref false

let pp_rich_operation_with_facts pp (op, facts) =
    if !dump_facts then
        fprintf pp "@[<v 2>%a@.%a@]" pp_rich_operation op pp_local_facts facts
    else
        pp_rich_operation pp op
let pp_rich_trace pp trace =
    FormatHelper.pp_print_list_lines pp_rich_operation_with_facts pp trace

let pp_rich_tracefile pp
    { funcs; objs; trace; globals; globals_are_properties; points_to } =
    fprintf pp "@[< v > Globals are properties: %b@ \
        @[< hov >%a@]@ \
        @[< hov >%a@]@ \
        @[< hov > Globals:@ %a@]@ \
        Trace:@ @[< hov >%a@]@ \
        Points - to:@ %a@]"
        globals_are_properties
        pp_functions funcs
        pp_objects objs
        pp_globals globals
        pp_rich_trace trace
        pp_points_to_map points_to

let is_external data f = false (* XXX *)

let enrich_step globals_are_properties objs (op, data) =
    let mkfieldref base offset =
        reference_of_field base offset |> make_versioned data
    and mkvarref isGlobal name =
        reference_of_name globals_are_properties data.aliases isGlobal name
        |> make_versioned data in
    let res = match op with
        | CFunPre { f; base; args; call_type } when is_external data f ->
            [RFunPre { f; base; args; call_type }; RFunEnter { f; this = base; args } ]
        | CFunPre { f; base; args; call_type } ->
            [RFunPre { f; base; args; call_type } ]
        | CFunPost { f; base; args; result } when is_external data f ->
            [RFunExit { ret = result; exc = OUndefined }; RFunPost { f; base; args; result }]
        | CFunPost { f; base; args; result } -> [RFunPost { f; base; args; result }]
        | CLiteral { value; hasGetterSetter } -> [RLiteral { value; hasGetterSetter }]
        | CForIn value -> [RForIn value]
        | CDeclare { name; declaration_type = ArgumentBinding idx } ->
            if Misc.StringMap.mem name data.aliases then
                [RAlias { name;
                    ref = Misc.StringMap.find name data.aliases
                        |> reference_of_fieldref
                        |> make_versioned data;
                    source = Argument idx }]
            else
                let ref = reference_of_local_name name |> make_versioned data in
                [RLocal { name; ref };
                RWrite { ref; oldref = ref; value = OUndefined; success = true } ]
        | CDeclare { name; value; declaration_type = CatchParam } ->
            let ref = reference_of_local_name name |> make_versioned data in
            [RCatch { name; ref };
            RWrite { ref; oldref = ref; value; success = true } ]
        | CDeclare { name; value } ->
            let ref = reference_of_local_name name |> make_versioned data in
            [RLocal { name; ref };
            RWrite { ref; oldref = ref; value; success = true } ]
        | CGetField { base; offset; value } ->
            [RRead { ref = mkfieldref base offset; value }]
        | CPutField { base; offset; value } ->
        (* FIXME success handling *)
            [RWrite {
                ref = mkfieldref base offset;
                oldref = Option.some data.last_update;
                value; success = true
            }]
        | CRead { name; value; isGlobal } ->
            [RRead { ref = mkvarref isGlobal name; value }]
        | CWrite { name; lhs; value; isGlobal } ->
        (* FIXME success handling *)
            [RWrite {
                ref = mkvarref isGlobal name;
                oldref = Option.some data.last_update;
                value;
                success = true
            }]
        | CReturn value -> [RReturn value]
        | CThrow value -> [RThrow value]
        | CWith value -> [RWith value]
        | CFunEnter { f; this; args } -> [RFunEnter { f; this; args }]
        | CFunExit { ret; exc } -> [RFunExit { ret; exc }]
        | CScriptEnter -> [RScriptEnter]
        | CScriptExit -> [RScriptExit]
        | CScriptExc exc -> [RScriptExc exc]
        | CBinary { op; left; right; result } -> [RBinary { op; left; right; result }]
        | CUnary { op; arg; result } -> [RUnary { op; arg; result }]
        | CEndExpression -> [REndExpression]
        | CConditional value -> [RConditional value] in
    List.map (fun op -> (op, data)) res

let calculate_rich_tracefile tracefile =
    tracefile |>
    calculate_arguments_and_parameters |>
    calculate_versions |>
    fun tf ->
        let (funcs, objs, etrace, globals, globals_are_properties) = tf
        and points_to = calculate_pointsto tf in
        { funcs; objs; globals; globals_are_properties; points_to;
            trace = etrace
                |> List.map (enrich_step globals_are_properties objs)
                |> List.flatten }
