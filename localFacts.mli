(** Facts that are local to a position in the trace. *)
open Types

(** Local facts are facts that change depending on the position
 * in the trace. *)
type local_facts = {
    (** The last argument object that was created by a function call. *)
    last_arguments: int option;
    (** The last reference that was modified. *)
    last_update: Reference.versioned_reference option;
    (** The current version of all known references. *)
    versions: int Reference.ReferenceMap.t;
    (** All currently-existing aliases. *)
    aliases: Types.fieldref Misc.StringMap.t;
}

val pp_local_facts: Format.formatter -> local_facts -> unit

(** Traces and tracefiles enrichted with local facts. *)
type 'a enriched_trace = (Cleantrace.clean_operation * 'a) list
type 'a enriched_tracefile = functions * objects * 'a enriched_trace * globals * bool
type facts_trace = local_facts enriched_trace
type facts_tracefile = local_facts enriched_tracefile
type unit_trace = unit enriched_trace
type unit_tracefile = unit enriched_tracefile
type arguments_trace = int option enriched_trace
type arguments_tracefile = int option enriched_tracefile

val pp_enriched_trace: (Format.formatter -> 'a -> unit) ->
Format.formatter -> 'a enriched_trace -> unit
val pp_enriched_tracefile: (Format.formatter -> 'a -> unit) ->
Format.formatter -> 'a enriched_tracefile -> unit
val pp_facts_trace: Format.formatter -> facts_trace -> unit
val pp_facts_tracefile: Format.formatter -> facts_tracefile -> unit

(** Iterators that help in calculating enriched traces. *)
(** [trace_collect step initial_state trace]
 * runs over [trace], as if executing it, calling
 * [step] at each step. It keeps some state, initially
 * [initial_state], which may be updated by [step].
 *
 * The contract for [step] is: It is called as [step state old_data op]
 * and should return a pair [(new_data, new_state)], where [new_data]
 * is the data that should be used to enrich that step in the trace,
 * and [new_state] is the new value for the state.
 *
 * [trace_collect] returns the trace enriched with new data, and the
 * final state. *)
val trace_collect :
('state -> 'olddata -> Cleantrace.clean_operation -> 'newdata * 'state) ->
'state -> 'olddata enriched_trace -> 'newdata enriched_trace * 'state
(** [trace_enrich step initial_state trace] works exactly
 * like [trace_collect step initial_state trace], except that it only
 * returns the trace enriched with new data. *)
val trace_enrich :
('state -> 'olddata -> Cleantrace.clean_operation -> 'newdata * 'state) ->
'state -> 'olddata enriched_trace -> 'newdata enriched_trace
(** [trace_fold f init] runs over [trace], as if executing it,
 * calling [f] at each step to update some state (initially [init]).
 * It returns the final state after the execution. *)
val trace_fold:
('acc -> 'data -> Cleantrace.clean_operation -> 'acc) -> 'acc ->
'data enriched_trace -> 'acc
(** Transform a trace to a trace enriched with empty local facts. *)
val trace_initialize : Trace.tracefile -> unit_tracefile 
(** Fill the arguments and parameters fields of an enriched trace. *)
val collect_arguments_and_parameters: unit_trace -> arguments_trace

(** Transform a trace file into an enriched trace file with
 * information about arguments and parameters filled in. *)
val calculate_arguments_and_parameters: Trace.tracefile -> arguments_tracefile

(** Create a reference for a variable.
 * [reference_of_variable globals_are_properties facts is_global name]
 * creates the appropriate reference for the variable called [name],
 * which is global iff [is_global] holds, taking all relevant facts
 * into account. Note that this requires the [aliases] field of
 * [facts] to be filled in. *)
val reference_of_variable: bool -> local_facts -> bool -> string -> Reference.reference

(** Turn a reference into a versioned reference using the current
 * version from the given facts. Note that this required the [versions] field
 * to be filled in. *)
val make_versioned : local_facts -> Reference.reference -> Reference.versioned_reference

