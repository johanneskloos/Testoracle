open LocalFacts
open Reference

(** * Fill in the aliases and version fields of an enriched trace. *)
(** [collect_versions objs globals_are_properties trace]
 * fills in the [aliases] and [version] fields of [trace]. *)
val collect_versions :
  Trace.objects -> bool -> Trace.globals -> facts_trace -> facts_trace
(** This function
 * fills in the [aliases] and [version] fields of the given trace file. *)
val calculate_versions: facts_tracefile -> facts_tracefile
