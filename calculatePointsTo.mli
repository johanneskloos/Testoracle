open LocalFacts
open Reference
open PointsTo

val collect_pointsto :
  bool ->
  Trace.objects -> facts_trace -> points_to_map
val calculate_pointsto : facts_tracefile -> points_to_map
