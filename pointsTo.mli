open LocalFacts
open Reference
open Trace

type points_to_map = objid VersionReferenceMap.t
val pp_points_to_map: Format.formatter -> points_to_map -> unit
val find_object_facts: int -> local_facts -> points_to_map -> objid Misc.StringMap.t
