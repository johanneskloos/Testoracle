open Trace
open LocalFacts
open PointsTo
open MatchTypes

(** Check if a value is of base type. *)
val is_base : Trace.jsval -> bool

type data = {
    funs1 : functions;
    funs2 : functions;
    facts1 : local_facts;
    facts2 : local_facts;
    pt1 : points_to_map;
    pt2 : points_to_map;
    noneq: Misc.IntIntSet.t;
}
(** Strict matching of functions. Two functions match strictly
* if they have the same implementation. *)
val match_functions : data -> int -> int -> fun_match_failure option
(** Looser matching of functions. Two functions are associated
* if do not have proof that they are not behaviourally equivalent. *)
val match_functions_associated : data -> int -> int -> bool

type recursive_matcher =
    data ->
    Misc.IntIntSet.t ->
    objeq -> Trace.jsval * Trace.jsval -> objeq * failure_trace

(** ** Matching of the recursive object structure. *)
(** The algorithm is based around the usual "remove distinguishables"
* algorithm that is well - known from, e.g., the comparison of automata.
*
* Because we have several layers of indirection, it has been split into
* multiple functions.
*
* [match_objects_raw matchobj ignored data seen objeq m1 m2]
* checks whether two objects [m1] and [m2] (as given by their field
* descriptions) are isomorphic: They must have the same fields, and for
* each field, the contents of that field must be isomorphic. Some fields
* may be ignored by the comparison; a list of such fields is given by
* the [ignored] field.
*
* It defers checking of value isomorphism to [matchobj], which uses the
* data in [data], [seen] and [objeq] to implement a terminating
* checking algorithm.
*
* It returns a pair [(objeq', result)], where [result] is an option
* stating whether the objects are isomorphic (if None) or why they
* are not isomorphic, and [objeq'] memoizes matching results.
*)
val match_objects_raw :
recursive_matcher ->
Misc.StringMap.key list ->
data ->
Misc.IntIntSet.t ->
objeq ->
Trace.jsval Misc.StringMap.t ->
Trace.jsval Misc.StringMap.t -> objeq * failure_trace

(** [match_objects_memo matchobj ignored data seen objeq id1 id2]
* checks whether two objects, refered to by their ids [id1] and [id2],
* are isomorphic using the current version and points - to maps, as given
* by [data]. It is a memoization wrapper around [match_objects_raw],
* using [matchobj] to ensure that the values in the object fields
* are isomorphic, and using [seen] and [objeq] for memoization.
*
* In particular, [seen] saves the object pairs considered on the
* current "path" through the objects and can be used to break cycles,
* while [objeq] memoizes previous comparison results.
*
* It returns a pair [(objeq', result)], where [result] is an option
* stating whether the objects are isomorphic (if None) or why they
* are not isomorphic, and [objeq'] memoizes matching results.
*)
val match_objects_memo :
recursive_matcher ->
Misc.StringMap.key list ->
data -> Misc.IntIntSet.t -> objeq ->
Reference.objectid -> Reference.objectid -> objeq * failure_trace

(** [match_values_raw cmpname data seen objeq (v1, v2)] checks
* whether two values [v1] and [v2] are isomorphic. For base values,
* the values must be identical, while for objects and functions,
* we check object isomorphism. In the case of functions, two special
* rules apply: the associated function implementation must match,
* using [match_functions_associated], and in the object isomorphism, the
* [toString] field is ignored. The role of [data], [seen] and [objeq]
* and the return value as as for [match_objects_memo]. *)
val match_values_raw : recursive_matcher

(** [match_values cmpname rt1 rt2 facts1 facts2 v1 v2 objeq]
* checks whether the values [v1] and [v2] are isomorphic,
* using information from the rich traces [rt1] and [rt2] and
* the fact collections [facts1] and [facts2]. The memoized object
* matching information in [objeq] is taking into account and updated.
*
* It returns a pair [(objeq', result)], where [result] is an option
* stating whether the objects are isomorphic (if None) or why they
* are not isomorphic, and [objeq'] memoizes matching results.
*)
val match_values :
string ->
Richtrace.rich_tracefile ->
Richtrace.rich_tracefile ->
LocalFacts.local_facts ->
LocalFacts.local_facts ->
Misc.IntIntSet.t ->
Trace.jsval -> Trace.jsval -> objeq -> objeq * named_failure_trace

(** [match_references rt1 rt2 facts1 facts2 r1 r2 objeq]
* checks whether the versioned references [r1] and [r2] point to
* isomporphic objects. *)
val match_refs :
string ->
Richtrace.rich_tracefile ->
Richtrace.rich_tracefile ->
LocalFacts.local_facts ->
LocalFacts.local_facts ->
Misc.IntIntSet.t ->
Reference.VersionReferenceMap.key ->
Reference.VersionReferenceMap.key -> objeq -> objeq * named_failure_trace
