open Types
open Trace
open Richtrace
open Arg
open Reference
open MatchTypes

type write_global = { ref: versioned_reference; value: jsval }
type call_external = { f: jsval; this: jsval; args: jsval; result: jsval }
type projected_op =
    | WriteGlobal of write_global
    | CallExternal of call_external
type projected_trace = {
    funcs: functions;
    objs: objects;
    trace: (projected_op * LocalFacts.local_facts) list;
    globals_are_properties: bool;
    points_to: PointsTo.points_to_map
}
let project_trace { funcs; objs; trace; globals; globals_are_properties; points_to } =
    let is_global (ref, _) = match ref with
      | GlobalVariable _ -> true
      | Field _ -> true
      | LocalVariable _ -> false
    and is_external = function
        | OFunction(_, fid) ->
            begin match funcs.(fid) with
                | External _ -> true
                | _ -> false
            end
        | _ -> false in
    let rec project_tr = function
        | (RWrite { ref; value }, facts) :: tr when is_global ref -> (WriteGlobal { ref; value }, facts) :: project_tr tr
        | (RFunPost { f; base; args; result }, facts) :: tr when is_external f -> (CallExternal { f; this = base; args; result }, facts) :: project_tr tr
        | _ :: tr -> project_tr tr
        | [] -> []
    in { funcs; objs; trace = project_tr trace; globals_are_properties; points_to }

let pp_named_failure_trace pp (_, tr) =
    let open Format in
    let open MatchObjects in
    match tr with
    | NonMatching (path, val1, val2) ->
        fprintf pp "At %a, %a vs. %a"
            (FormatHelper.pp_print_list pp_print_string) path
            pp_jsval val1 pp_jsval val2
    | MissingOrig (name, path) ->
        fprintf pp "At %a, %s missing in orig"
            (FormatHelper.pp_print_list pp_print_string) path name
    | MissingXfrm (name, path) ->
        fprintf pp "At %a, %s missing in xfrm"
            (FormatHelper.pp_print_list pp_print_string) path name
    | Other msg -> pp_print_string pp msg

let projected_match rt1 rt2 (op1, facts1) (op2, facts2) =
    let empty = Misc.IntIntMap.empty in
    let match_val key val1 val2 =
        match MatchObjects.match_values key rt1 rt2 facts1 facts2 Misc.IntIntSet.empty val1 val2 empty with
        | (_, None) -> true
        | (_, Some err) -> Format.eprintf "%s doesn't match: %a@." key pp_named_failure_trace err; false in
    let match_ref key ref1 ref2 =
        match MatchObjects.match_refs key rt1 rt2 facts1 facts2 Misc.IntIntSet.empty ref1 ref2 empty with
        | (_, None) -> true
        | (_, Some err) -> Format.eprintf "%s doesn't match: %a@." key pp_named_failure_trace err; false in
    match op1, op2 with
    | WriteGlobal { ref = ref1; value = val1 }, WriteGlobal { ref = ref2; value = val2 } ->
        match_ref "ref" ref1 ref2 && match_val "val" val1 val2
    | CallExternal { f = f1; this = this1; args = args1; result = result1 },
    CallExternal { f = f2; this = this2; args = args2; result = result2 } ->
        match_val "f" f1 f2 && match_val "this" this1 this2 && match_val "args" args1 args2 && match_val "result" result1 result2
    | _ -> Format.eprintf "Non-matching event types@."; false

let match_traces_simple rt1 rt2 =
    let pr1 = project_trace rt1
    and pr2 = project_trace rt2 in
    List.for_all2 (projected_match rt1 rt2) pr1.trace pr2.trace

let debug = ref false
let default_orig_trace = ".orig.trace"
let default_xfrm_trace = ".xfrm.trace"

let (>>) x f = f x; x

let debug_print msg pp data =
    if !debug then Format.printf "@[<v 2>%s:@ %a@]@."
            msg pp data

let rich_tracefile_from_path path =
    let chan = open_in path in
    let rt = parse_tracefile chan
        >> debug_print ("Read trace file " ^ path) pp_tracefile
        |> calculate_rich_tracefile
        >> debug_print "Enrichted trace file" pp_rich_tracefile in
    close_in chan; rt

let parse_args () =
    let names = ref [] in
    let args = [
        ("-D", Set debug, "Debugging mode");
        ("-t", String (MatchTracesObserver.open_observer), "Trace file")
        ]
    and usage_msg =
        "Test oracle for Javascript trace equivalence. Usage:\n\
    simple_oracle path_orig path_xfrm\n\
    simple_oracle path_orig\n\
    where path_orig is the path to the trace file for unmodified code\n\
    and path_xfrm is the path to the trace file for modified code" in
    parse args (fun s -> names := s :: !names) usage_msg;
    match !names with
    | [path_xfrm; path_orig] -> (path_orig, path_xfrm)
    | [path_orig] when Filename.check_suffix path_orig default_orig_trace ->
        (path_orig, (Filename.chop_suffix path_orig default_orig_trace) ^ default_xfrm_trace)
    | _ -> usage args usage_msg; exit 2

let calculate_matching path_orig path_xfrm =
    let rt1 = rich_tracefile_from_path path_orig
    and rt2 = rich_tracefile_from_path path_xfrm in
    match_traces_simple rt1 rt2

let main () =
    let (path_orig, path_xfrm) = parse_args() in
    match calculate_matching path_orig path_xfrm with
    | true -> print_endline "OK"; MatchTracesObserver.close_observer (); exit 0
    | false -> print_endline "FAIL"; MatchTracesObserver.close_observer (); exit 1;;

main ()