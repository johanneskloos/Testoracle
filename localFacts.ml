open Trace
open Reference
open Misc
open Notations
open Cleantrace 

type local_facts = {
    last_arguments: int option;
    last_parameters: int option;
    last_update: versioned_reference option;
    versions: int ReferenceMap.t;
    aliases: (int * string) StringMap.t;
    globals: (int * string) list
}

type 'a enriched_trace = (clean_operation * 'a) list
type 'a enriched_tracefile = functions * objects * 'a enriched_trace * globals * bool
type facts_trace = local_facts enriched_trace
type facts_tracefile = local_facts enriched_tracefile

let pp_local_facts pp
  {last_arguments; last_parameters; last_update; versions; aliases} =
      Format.fprintf pp "@[<v>\
        Last caller-side argument object: %a@ \
        Last callee-side argument object: %a@ \
        Last update: %a@ \
        Versions: @[<hov 2>%a@]@ \
        Aliases: @[<hov 2>%a@]@ @]"
        (FormatHelper.pp_print_option Format.pp_print_int) last_arguments
        (FormatHelper.pp_print_option Format.pp_print_int) last_parameters
        (FormatHelper.pp_print_option pp_versioned_reference) last_update
        (pp_reference_map Format.pp_print_int) versions
        (StringMapFormat.pp_print_map "" "" ","
          (fun pp name (obj, fld) ->
              Format.fprintf pp "%s -> %d@@%s" name obj fld)) aliases
                
let pp_enriched_trace fmt =
    FormatHelper.pp_print_list_lines
        (fun pp (op, facts) ->
            Format.fprintf pp "@[<v 2>%a@ %a@]"
            pp_clean_operation op
            fmt facts)
let pp_facts_trace = pp_enriched_trace pp_local_facts

let pp_enriched_tracefile fmt pp (f, o, t, g, gap) =
  Format.fprintf pp
    "@[<v>Globals are properties: %b@ \
     @[<hov>%a@]@ @[<hov>%a@]@ \
     @[<hov>Globals:@ %a@]@ Trace:@ \
     @[<hov>%a@]@]"
    gap pp_functions f pp_objects o pp_globals g (pp_enriched_trace fmt) t 
let pp_facts_tracefile = pp_enriched_tracefile pp_local_facts

let empty_local_facts = {
    last_arguments = None;
    last_parameters = None;
    last_update = None;
    versions = ReferenceMap.empty;
    aliases = StringMap.empty;
    globals = []
}

let trace_initialize tr =
  tr |> clean_trace |> List.map (fun op -> (op, empty_local_facts))

let trace_collect
  (f: 'state -> 'olddata -> clean_operation -> 'newdata * 'state)
  (init: 'state) (tr: 'olddata enriched_trace):
  ('newdata enriched_trace * 'state) =
  List.fold_left (fun (enriched_trace, state) (operation, olddata) ->
      let (newdata, state') = f state olddata operation in
      ((operation, newdata) :: enriched_trace, state'))
        ([], init) tr
    |> map12 List.rev

let trace_enrich f init tr = trace_collect f init tr |> fst

let trace_fold (f: 'acc -> 'data -> clean_operation -> 'acc):
    'acc -> 'data enriched_trace -> 'acc =
  List.fold_left (fun acc (operation, data) -> f acc data operation)

let collect_arguments_and_parameters_step 
        (last_arguments, last_parameters) facts op =
    let (arguments, parameters) = match op with
    | CFunPre { args } -> (args::last_arguments, last_parameters)
    | CFunPost _ -> (List.tl last_arguments, last_parameters)
    | CFunEnter { args } -> (last_arguments, args::last_parameters)
    | CFunExit _ -> (last_arguments, List.tl last_parameters)
    | _ -> (last_arguments, last_parameters)
    in ({ facts with
        last_arguments = hd_err arguments >|? get_object;
        last_parameters = hd_err parameters >|? get_object },
        (arguments, parameters))
let collect_arguments_and_parameters tr =
    trace_enrich collect_arguments_and_parameters_step ([], []) tr

let collect_globals_step (locals, globals) facts op =
  let (locals', globals') = match op with
    | CLiteral { value = OObject id }
    | CLiteral { value = OOther (_, id) }
    | CLiteral { value = OFunction (id, _) } ->
      (id :: locals, globals)
    | CGetField { base; offset } when not (List.mem (get_object base) locals) ->
      (locals, (get_object base, offset) :: globals)
    | _ ->
      (locals, globals)
  in ({ facts with globals=globals' }, (locals', globals'))
   
let collect_globals tr =
  trace_enrich collect_globals_step ([], []) tr
  
let calculate_arguments_and_parameters
    (funs, objs, trace, globals, globals_are_properties) =
    (funs, objs, trace_initialize trace |> collect_arguments_and_parameters,
     globals, globals_are_properties)

let calculate_globals (funs, objs, trace, globals, globals_are_properties) =
  (funs, objs, collect_globals, globals, globals_are_properties)
  
let reference_of_variable gap facts global name =
    reference_of_name gap facts.aliases global name

let make_versioned state ref =
    try (ref, ReferenceMap.find ref state.versions)
    with Not_found -> (Format.eprintf "Did not find %a in %a"
      pp_reference ref pp_local_facts state; raise Not_found)


