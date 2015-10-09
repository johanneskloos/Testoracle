open Misc.Notations
open Types

type local_facts = {
    last_arguments: int option;
    last_update: Reference.versioned_reference option;
    versions: int Reference.ReferenceMap.t;
    aliases: fieldref Misc.StringMap.t
}

type 'a enriched_trace = (Cleantrace.clean_operation * 'a) list
type 'a enriched_tracefile = functions * objects * 'a enriched_trace * globals * bool
type facts_trace = local_facts enriched_trace
type facts_tracefile = local_facts enriched_tracefile
type unit_trace = unit enriched_trace
type unit_tracefile = unit enriched_tracefile
type arguments_trace = int option enriched_trace
type arguments_tracefile = int option enriched_tracefile

let pp_local_facts pp
    { last_arguments; last_update; versions; aliases } =
    Format.fprintf pp "@[< v >\
        Last callee-side argument object: %a@ \
        Last update: %a@ \
        Versions: @[< hov 2 >%a@]@ \
        Aliases: @[< hov 2 >%a@]@ @]"
        (FormatHelper.pp_print_option Format.pp_print_int) last_arguments
        (FormatHelper.pp_print_option Reference.pp_versioned_reference) last_update
        (Reference.pp_reference_map Format.pp_print_int) versions
        (Misc.StringMapFormat.pp_print_map "" "" ","
                (fun pp name fr ->
                        Format.fprintf pp "%s -> %a" name pp_fieldref fr)) aliases

let pp_enriched_trace fmt =
    FormatHelper.pp_print_list_lines
        (fun pp (op, facts) ->
                Format.fprintf pp "@[<v 2>%a@ %a@]"
                    Cleantrace.pp_clean_operation op
                    fmt facts)
let pp_facts_trace = pp_enriched_trace pp_local_facts

let pp_enriched_tracefile fmt pp (f, o, t, g, gap) =
    Format.fprintf pp
        "@[< v > Globals are properties: %b@ \
        @[< hov >%a@]@ @[< hov >%a@]@ \
        @[< hov > Globals:@ %a@]@ Trace:@ \
        @[< hov >%a@]@]"
        gap pp_functions f pp_objects o pp_globals g (pp_enriched_trace fmt) t
let pp_facts_tracefile = pp_enriched_tracefile pp_local_facts

let trace_initialize  tracefile =
    Cleantrace.clean_tracefile tracefile |>
    fun (funcs, objs, trace, globals, gap) ->
      (funcs, objs, List.map (fun op -> (op, ())) trace, globals, gap) 

let trace_collect
    (f: 'state -> 'olddata -> Cleantrace.clean_operation -> 'newdata * 'state)
    (init: 'state) (tr: 'olddata enriched_trace):
('newdata enriched_trace * 'state) =
    List.fold_left (fun (enriched_trace, state) (event, olddata) ->
                let (newdata, state') = f state olddata event in
                ((event, newdata) :: enriched_trace, state'))
        ([], init) tr
    |> Misc.map12 List.rev

let trace_enrich f init tr = trace_collect f init tr |> fst

let trace_fold (f: 'acc -> 'data -> Cleantrace.clean_operation -> 'acc):
'acc -> 'data enriched_trace -> 'acc =
    List.fold_left (fun acc (event, data) -> f acc data event)

let collect_arguments_and_parameters_step
    last_arguments () op =
    let arguments = match op with
        | Cleantrace.CFunEnter { Cleantrace.args } -> args :: last_arguments
        | Cleantrace.CFunExit _ -> List.tl last_arguments
        | _ -> last_arguments
    in (Misc.hd_err arguments >|? get_object, arguments)
        
let collect_arguments_and_parameters tr =
    trace_enrich collect_arguments_and_parameters_step [] tr

let calculate_arguments_and_parameters tf =
  trace_initialize tf |>
  fun (funs, objs, trace, globals, globals_are_properties) ->
    (funs, objs, collect_arguments_and_parameters trace, globals, globals_are_properties)

let reference_of_variable gap facts global name =
    Reference.reference_of_name gap facts.aliases global name

let make_versioned state ref =
    try (ref, Reference.ReferenceMap.find ref state.versions)
    with Not_found -> (Format.eprintf "Did not find %a in %a"
                Reference.pp_reference ref pp_local_facts state; raise Not_found)


