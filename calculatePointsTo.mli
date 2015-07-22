open LocalFacts
open Reference
open PointsTo

(** Collect points-to information:
 * [collect_pointsto globals_are_properties objs trace]
 * collects points-to information for trace [trace] using
 * the list of objects [objs] and the [globals_are_properties] flag.
 *)
val collect_pointsto :
  bool ->
  Trace.objects -> facts_trace -> points_to_map
(** Calculate a points-to map for a given trace file. *)
val calculate_pointsto : facts_tracefile -> points_to_map
