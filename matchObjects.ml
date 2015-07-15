open PointsTo
open LocalFacts
open Trace
open Misc
open Richtrace
open Reference

let is_base = function
    | OFunction _ | OObject _ | OOther _ -> false
    | _ -> true

type data = {
    funs1: functions;
    funs2: functions;
    facts1: local_facts;
    facts2: local_facts;
    pt1: points_to_map;
    pt2: points_to_map
}

(** Strict matching of functions. *)
let match_functions {funs1; funs2} fun1 fun2 =
    match (funs1.(fun1), funs2.(fun2)) with
    | (Local { instrumented=i1; uninstrumented=u1 },
       Local { instrumented=i2; uninstrumented=u2 }) ->

           (u1 <> "(unknown)" && u1 = u2) || (u1 = "(unknown)" && i1 = i2)
    | (External id1, External id2) -> id1 = id2
    | _ -> false

(** Associated matching of functions. This gives a coarse
 * over-approximation. *)
let match_functions_associated {funs1; funs2} fun1 fun2 =
    match funs1.(fun1), funs2.(fun2) with
    | (Local _, Local _) -> true
    | (External id1, External id2) -> id1 = id2
    | _ -> false

module StringMapExtra = MapExtra(StringMap);;

(** ** Matching of the recursive object structure. *)
(** The algorithm is based around the usual "remove distinguishables"
 * algorithm that is well-known from, e.g., the comparison of automata.
 * 
 * Because we have several layers of indirection, it has been split into
 * multiple functions.
 *
 * [match_objects_raw matchobj ignored data seen objeq m1 m2] checked
 * whether two objects [m1] and [m2] (as given by their field descriptions)
 * are isomorphic: They must have the same fields, and for each field, the
 * contents of that field must be isomorphic. Some fields may be ignored
 * by the comparison; a list of such fields is given by the [ignored]
 * field.
 *
 * It defers checking of value isomorphism to [matchobj], which uses the
 * data in [data], [seen] and [objeq] to implement a terminating
 * checking algorithm.
 *
 * It returns a pair [(result, objeq')], where [result] is a boolean
 * stating whether the objects are isomorphic, and [objeq'] memoizes
 * matching results.
 *)
let match_objects_raw 
        (matchobj: data -> IntIntSet.t -> bool IntIntMap.t -> Trace.objid * Trace.objid -> bool * bool IntIntMap.t)
        ignored data seen objeq m1 m2 =
    let one_side_only field _ objeq =
        if List.mem field ignored then Some objeq else None
    and match_both_sides field val1 val2 objeq =
        if List.mem field ignored then
            Some objeq
        else
            match matchobj data seen objeq (val1, val2) with
            | (false, _) -> None
            | (true, objeq') -> Some objeq' in
    StringMapExtra.fold2_option one_side_only one_side_only
        match_both_sides m1 m2 objeq

(** [match_objects_memo matchobj ignored data seen objeq id1 id2]
 * checks whether two objects, refered to by their ids [id1] and [id2],
 * are isomorphic using the current version and points-to maps, as given
 * by [data]. It is a memoization wrapper around [match_objects_raw],
 * using [matchobj] to ensure that the values in the object fields
 * are isomorphic, and using [seen] and [objeq] for memoization.
 *
 * In particular, [seen] saves the object pairs considered on the
 * current "path" through the objects and can be used to break cycles,
 * while [objeq] memoizes previous comparison results.
 *
 * It returns a pair [(result, objeq')], where [result] is a boolean
 * stating whether the objects are isomorphic, and [objeq'] memoizes
 * matching results.
 *)
let match_objects_memo matchobj ignored data seen objeq id1 id2 =
    if IntIntSet.mem (id1, id2) seen then begin
        (true, objeq)
    end else if IntIntMap.mem (id1, id2) objeq then begin
        (IntIntMap.find (id1, id2) objeq, objeq)
    end else begin
        let m1 = find_object_facts id1 data.facts1 data.pt1
        and m2 = find_object_facts id2 data.facts2 data.pt2
        and seen' = IntIntSet.add (id1, id2) seen in
        match
            match_objects_raw matchobj ignored data seen' (Some objeq) m1 m2 
        with
        | Some objeq -> (true, IntIntMap.add (id1, id2) true objeq)
        | None -> (false, IntIntMap.add (id1, id2) false objeq)
    end

(** [match_values_raw data seen objeq (v1, v2)] checks
 * whether two values [v1] and [v2] are isomorphic. For base values,
 * the values must  be identical, while for objects and functions,
 * we check object isomorphism. In the case of functions, two special
 * rules apply: the associated function implementation must match,
 * using [match_functions_associated], and in the object isomorphism, the
 * [toString] field is ignored. The role of [data], [seen] and [objeq]
 * and the return value as as for ]match_objects_memo]. *)
let rec match_values_raw data seen objeq = function
    | (o1, o2)
        when (o1 = o2 && is_base o1) ->
            (true, objeq)
    | (OObject id1, OObject id2) ->
            match_objects_memo match_values_raw [] data seen objeq id1 id2
    | (OOther (ty1, id1), OOther (ty2, id2))
        when ty1 = ty2 ->
            match_objects_memo match_values_raw [] data seen objeq id1 id2
    | (OFunction (id1, fun1), OFunction (id2, fun2))
        when match_functions_associated data fun1 fun2 ->
            match_objects_memo match_values_raw ["toString"] data  seen objeq
                 id1 id2
    | (o1, o2) ->
            (false, objeq)

(** [match_values rt1 rt2 facts1 facts2 v1 v2 objeq]
 * checks whether the values [v1] and [v2] are isomorphic,
 * using information from the rich traces [rt1] and [rt2] and
 * the fact collections [facts1] and [facts2]. The memoized object
 * matching information in [objeq] is taking into account and updated.
 *
 * It returns a pair [(isIso, objeq')], where [isIso] states whether the
 * values are isomorphic, and [objeq'] contains the updated matching
 * information.
 *)
let match_values
    { funcs=funs1; points_to = pt1 }
    { funcs=funs2; points_to = pt2 }
    facts1 facts2 obj1 obj2 objeq =
    match_values_raw { funs1; funs2; facts1; facts2; pt1; pt2 }
        IntIntSet.empty objeq (obj1, obj2)

(** [match_references rt1 rt2 facts1 facts2 r1 r2 objeq]
 * checks whether the versioned references [r1] and [r2] point to
 * isomporphic objects. *)
let match_refs rt1 rt2 facts1 facts2 r1 r2 objeq =
    match_values rt1 rt2 facts1 facts2
        (VersionReferenceMap.find r1 rt1.points_to)
        (VersionReferenceMap.find r2 rt2.points_to)
        objeq
