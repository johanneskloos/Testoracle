open Misc
open LocalFacts
open CalculateVersions
open CalculatePointsTo
open PointsTo
open Trace
open Reference
open Cleantrace

type alias_source = Argument of int | With of versioned_reference
type rfunpre = { f: objid; base: objid; args: objid; call_type: call_type }
type rfunpost = { f: objid; base: objid; args: objid; result: objid }
type rliteral = { value: objid; hasGetterSetter: bool }
type rlocal = { name: string; ref: versioned_reference }
type ralias = { name: string; source: alias_source; ref: versioned_reference }
type rread = { ref: versioned_reference; value: objid }
type rwrite = {
    ref: versioned_reference; 
    oldref: versioned_reference; 
    value: objid; 
    success: bool
}
type rbinary = { op: string; left: objid; right: objid; result: objid }
type runary = { op: string; arg: objid; result: objid }
type rfunenter = { f: objid; this: objid; args: objid }
type rfunexit = { ret: objid; exc: objid }
type rich_operation =
  | RFunPre of rfunpre
  | RFunPost of rfunpost
  | RLiteral of rliteral
  | RForIn of objid
  | RLocal of rlocal
  | RAlias of ralias
  | RRead of rread
  | RWrite of rwrite
  | RReturn of objid
  | RThrow of objid
  | RWith of objid
  | RFunEnter of rfunenter
  | RFunExit of rfunexit
  | RScriptEnter
  | RScriptExit
  | RScriptExc of objid
  | RBinary of rbinary
  | RUnary of runary
  | REndExpression
  | RConditional of objid

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
        begin match call_type with
        | Function ->
            fprintf pp "Calling function %a on %a with %a"
                pp_objid f pp_objid base pp_objid args
        | Method ->
            fprintf pp "Calling method %a on %a with %a"
                pp_objid f pp_objid base pp_objid args
        | Constructor ->
            fprintf pp "Calling constructor %a on %a with %a"
                pp_objid f pp_objid base pp_objid args
        | ConstructorMethod ->
            fprintf pp "Calling constructor method %a on %a with %a"
                pp_objid f pp_objid base pp_objid args
        end
    | RFunPost { f; base; args; result } ->
        fprintf pp "Calling %a on %a with %a returns %a"
            pp_objid f pp_objid base pp_objid args pp_objid result
    | RLiteral { value; hasGetterSetter } ->
        fprintf pp "Literal %a" pp_objid value;
        if hasGetterSetter then pp_print_string pp " (has getter and/or setter)"
    | RForIn obj -> fprintf pp "for (... in %a)" pp_objid obj
    | RLocal { name; ref } ->
        fprintf pp "var %s; (reference: %a)" name pp_versioned_reference ref
    | RAlias { name; source; ref } ->
        fprintf pp "introducting alias %s for %a due to %a"
            name pp_versioned_reference ref pp_alias_source source
    | RRead { ref; value } ->
        fprintf pp "Reading %a yields %a"
            pp_versioned_reference ref pp_objid value
    | RWrite { ref; value; oldref; success } ->
        if success then
            fprintf pp "Writing %a to %a (formerly %a)"
                pp_objid value pp_versioned_reference ref
                pp_versioned_reference oldref
        else
            fprintf pp "Ignored write of %a to %a (formerly %a)" 
                pp_objid value pp_versioned_reference ref
                pp_versioned_reference oldref
    | RReturn obj -> fprintf pp "return %a" pp_objid obj
    | RThrow obj -> fprintf pp "throw %a" pp_objid obj
    | RWith obj -> fprintf pp "with %a" pp_objid obj
    | RFunEnter { f; this; args } ->
        fprintf pp "Entering %a on %a with %a"
            pp_objid f pp_objid this pp_objid args
    | RFunExit { ret; exc } ->
        fprintf pp "Returning %a with exception %a" pp_objid ret pp_objid exc
    | RScriptEnter -> pp_print_string pp "Entering script"
    | RScriptExit -> pp_print_string pp "Exiting script"
    | RScriptExc e -> fprintf pp "Exiting script with exception %a" pp_objid e
    | RBinary { op; left; right; result } ->
        fprintf pp "Evaluationg %a %s %a yields %a"
            pp_objid left op pp_objid right pp_objid result
    | RUnary { op; arg; result } ->
        fprintf pp "Evaluationg %s %a yields %a" op pp_objid arg pp_objid result
    | REndExpression -> pp_print_string pp "(discarding expression result)"
    | RConditional value -> fprintf pp "Conditional yielded %a" pp_objid value

let pp_rich_operation_with_facts pp (op, _) = pp_rich_operation pp op
let pp_rich_trace pp trace =
    FormatHelper.pp_print_list_lines pp_rich_operation_with_facts pp trace

let pp_rich_tracefile pp
    { funcs; objs; trace; globals; globals_are_properties; points_to } =
  fprintf pp "@[<v>Globals are properties: %b@ \
    @[<hov>%a@]@ \
    @[<hov>%a@]@ \
    @[<hov>Globals:@ %a@]@ \
    Trace:@ @[<hov>%a@]@ \
    Points-to:@ %a@]"
    globals_are_properties
    pp_functions funcs
    pp_objects objs
    pp_globals globals
    pp_rich_trace trace
    pp_points_to_map points_to

let enrich_step globals_are_properties objs (op, data) =
    let mkfieldref base offset =
        reference_of_field base offset |> make_versioned data
    and mkvarref isGlobal name =
        reference_of_name globals_are_properties data.aliases isGlobal name
        |> make_versioned data in
    let res = match op with
  | CFunPre { f; base; args; call_type } ->
          [RFunPre { f; base; args; call_type } ]
  | CFunPost { f; base; args; result } -> [RFunPost {f; base; args; result}]
  | CLiteral { value; hasGetterSetter } -> [RLiteral {value; hasGetterSetter}]
  | CForIn value -> [RForIn value]
  | CDeclare { name; declaration_type = ArgumentBinding idx } ->
      [RAlias { name;
                ref=Misc.StringMap.find name data.aliases
                    |> reference_of_fieldref
                    |> make_versioned data;
                source = Argument idx }]
  | CDeclare { name; value } ->
          let ref = reference_of_local_name name |> make_versioned data in
          [RLocal { name; ref };
           RWrite { ref; oldref=ref; value; success=true} ]
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
  | CEndExpression _ -> [REndExpression]
  | CConditional value -> [RConditional value] in
    List.map (fun op -> (op, data)) res

let calculate_rich_tracefile tracefile =
    tracefile |>
    calculate_arguments_and_parameters |>
    calculate_globals |>
    calculate_versions |>
    fun tf ->
    let (funcs, objs, etrace, globals, globals_are_properties) = tf in
        { funcs; objs; globals; globals_are_properties;
          trace = etrace |> List.map (enrich_step globals_are_properties objs)
                         |> List.flatten;
          points_to = calculate_pointsto tf }
