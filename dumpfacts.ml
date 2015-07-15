open Format;;
open Trace;;
open Misc;;
open FormatHelper;;
open LocalFacts;;
open PointsTo;;
open Reference;;

let (>>) x f = f x; x

let build_facts tracefile =
    let (funs, objs, trace, globals, globals_are_properties) = tracefile in
    trace |>
    trace_initialize |>
    collect_arguments_and_parameters >>
    (fun _ -> printf "Collected arguments and parameters@.") |>
    collect_versions objs globals_are_properties >>
    (fun _ -> printf "Collected versions@.") |>
    fun tr -> (tracefile, tr, collect_pointsto globals_are_properties objs tr)

let pp_field pp (obj, fld) = fprintf pp "%d@@%s" obj fld

let pp_facts pp {last_arguments; last_parameters; versions; aliases} =
    fprintf pp "@[<v>Last caller argument object: %a@ \
      Last frame argument object: %a@ \
      Versions: %a@ \
      Aliases: %a@ @]"
      (pp_print_option pp_print_int) last_arguments
      (pp_print_option pp_print_int) last_parameters
      (pp_reference_map pp_print_int) versions
      (StringMapFormat.pp_print_map_default pp_print_string pp_field) aliases

let pp_lfop pp (op, facts) =
    fprintf pp "@[<v 2>%a@ %a@]@ " pp_operation op pp_facts facts

let pp_lftrace pp =
    fprintf pp "@[<v>%a@]" (fun pp -> List.iter (pp_lfop pp));;
let pp_pointsto pp map =
    fprintf pp "@[<v>%a@]"
        (fun pp -> VersionReferenceMap.iter (fun (ref, fld) value ->
            fprintf pp "%a:%d -> %a@ " Reference.pp_reference ref fld pp_objid value)) map

let print_results ((funs, objs, _, globals, globals_are_properties), trace, pointsto) =
    printf "@[<v>\
        Globals are %s@ \
        @ \
        Initial values of objects@ \
        =========================@ \
        @[<v>%a@]@ \
        @ \
        Trace, enriched with facts@ \
        ==========================@ \
        @[<v>%a@]@ \
        @ \
        Points-To facts@ \
        ===============@ \
        @[<v>%a@]@ \
        @."
        (if globals_are_properties then "properties" else "variables")
        pp_objects objs
        pp_lftrace trace
        pp_pointsto pointsto;;

let main () =
    Testharness.test_read_tracefile "dumpfacts" >>
    (fun _ -> printf "Read trace file@.") |>
    build_facts >>
    (fun _ -> printf "Built facts@.") |>
    print_results;;

main ();
