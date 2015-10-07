open LocalFacts
open Reference
open Types

(** A points-to map assigned a value to each versioned reference occuring in a trace. *)
type points_to_map = jsval VersionReferenceMap.t
(** Pretty-printer. *)
val pp_points_to_map: Format.formatter -> points_to_map -> unit
(** [find_object_facts id facts pt] finds all points-to facts
* for object [id], using [facts] to find versions for all fields and [pt] to find
* the corresponding values. *)
val find_object_facts: objectid -> local_facts -> points_to_map -> jsval Misc.StringMap.t
