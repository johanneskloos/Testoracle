type fieldref = int * string
type reference
module ReferenceMap: Map.S with type key = reference
val reference_compare: reference -> reference -> int
val pp_fieldref: Format.formatter -> fieldref -> unit
val pp_reference: Format.formatter -> reference -> unit
val pp_reference_map: (Format.formatter -> 'a -> unit) ->
    Format.formatter -> 'a ReferenceMap.t -> unit
val reference_of_name:
    bool -> fieldref Misc.StringMap.t -> bool -> string -> reference
val reference_of_field: Trace.objid -> string -> reference
val reference_of_fieldref: fieldref -> reference
val reference_of_local_name: string -> reference
val get_fieldref: reference -> fieldref option
val is_global: reference -> bool
val get_name: reference -> string option
val parse_reference: string -> reference

type versioned_reference = reference * int
module VersionReferenceMap : Map.S with type key = versioned_reference
module VersionReferenceSet : Set.S with type elt = versioned_reference
val pp_versioned_reference: Format.formatter -> versioned_reference -> unit

