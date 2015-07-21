open Trace
open Reference
open Cleantrace

type local_facts = {
    last_arguments: int option;
    last_parameters: int option;
    last_update: versioned_reference option;
    versions: int ReferenceMap.t;
    aliases: (int * string) Misc.StringMap.t;
    globals: (int * string) list
}
val empty_local_facts: local_facts

val pp_local_facts: Format.formatter -> local_facts -> unit

type 'a enriched_trace = (clean_operation * 'a) list
type 'a enriched_tracefile = functions * objects * 'a enriched_trace * globals * bool
type facts_trace = local_facts enriched_trace
type facts_tracefile = local_facts enriched_tracefile

val pp_enriched_trace: (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a enriched_trace -> unit
val pp_enriched_tracefile: (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a enriched_tracefile -> unit
val pp_facts_trace: Format.formatter -> facts_trace -> unit
val pp_facts_tracefile: Format.formatter -> facts_tracefile -> unit

val trace_collect :
  ('state -> 'olddata -> clean_operation -> 'newdata * 'state) ->
  'state -> 'olddata enriched_trace -> 'newdata enriched_trace * 'state
val trace_enrich :
  ('state -> 'olddata -> clean_operation -> 'newdata * 'state) ->
  'state -> 'olddata enriched_trace -> 'newdata enriched_trace
val trace_fold:
  ('acc -> 'data -> clean_operation -> 'acc) -> 'acc ->
  'data enriched_trace -> 'acc
val trace_initialize : trace -> facts_trace
val collect_arguments_and_parameters: facts_trace -> facts_trace
val collect_globals: facts_trace -> facts_trace
val calculate_arguments_and_parameters: tracefile -> facts_tracefile
val calculate_globals: tracefile -> facts_tracefile
val reference_of_variable: bool -> local_facts -> bool -> string -> reference

val make_versioned : local_facts -> reference -> versioned_reference

