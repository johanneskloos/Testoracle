open LocalFacts
open Reference
open PointsTo
open Types

(** Calculate a points-to map for a given trace file. Note that this requires
 * the version map to be initialized. *)
val calculate_pointsto : facts_tracefile -> points_to_map
