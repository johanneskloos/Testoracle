open PointsTo
open LocalFacts
open Trace
open Misc
open Richtrace
open Reference

type obj_match_failure =
  | NonMatching of string list * jsval * jsval
  | MissingOrig of string list
  | MissingXfrm of string list
  | Other of string
  
type objeq = {
  objeq_cache: bool Misc.IntIntMap.t;
  failure_trace: (string * obj_match_failure) option 
  }
  
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

let function_body_split s =
    try
        if s = "(unknown)" then
            None
        else
            Some (Str.string_after s (String.index s '('))
    with Not_found -> None

(** Strict matching of functions. *)
let match_functions { funs1; funs2 } fun1 fun2 =
    match (funs1.(fun1), funs2.(fun2)) with
    | (Local { instrumented = i1; uninstrumented = u1 },
    Local { instrumented = i2; uninstrumented = u2 }) ->
    (* This is a hack. The function text used here can be either
     * function name(args) { body } or (unknown). We say that
     * two functions are "intensionally equivalent" if they have the same
     * function body. This is, strictly speaking, not correct, but we simply
     * use this as a heuristic to drive the search, and actually validate that
     * guess using the trace. Note that the original implementation
     * blindly compared the two strings. This will not work: It is likely
     * that the function names are different, due to the way the transform
     * works. Thus, we compare the arguments and the body separately;
     * XXX Consider using an even better comparison based on actually parsing
     * the Javascript, at least removing comments and normalizing semicolons
     * and whitespace.
     *)
    begin
        match function_body_split u1, function_body_split u2 with
          | Some body1, Some body2 ->
               body1 = body2
          | None, None ->
            i1 = i2
          | _ ->
            false
     end
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
let match_objects_raw name
    (matchobj: string -> data -> IntIntSet.t -> objeq -> Trace.jsval * Trace.jsval -> bool * objeq)
    ignored data seen objeq m1 m2 =
    let fold fleft fright fboth =
        StringMapExtra.fold2
            (fun field _ (so_far, objeq) ->
                    if so_far then fleft field objeq else (false, objeq))
            (fun field _ (so_far, objeq) ->
                    if so_far then fright field objeq else (false, objeq))
            (fun field val1 val2 (so_far, objeq) ->
                    if so_far then fboth field val1 val2 objeq else (false, objeq))
            m1 m2 (true, objeq)
    and extend_error field (res, { objeq_cache; failure_trace }) =
      let failure_trace = match failure_trace with
      | Some (name, NonMatching (trace, v1, v2)) -> Some (name, NonMatching (field :: trace, v1, v2))
      | Some (name, MissingOrig tr) -> Some (name, MissingOrig (field :: tr))
      | Some (name, MissingXfrm tr) -> Some (name, MissingXfrm (field :: tr))
      | _ -> failure_trace in
      (res, { objeq_cache; failure_trace }) in
    fold
      (fun field objeq ->
         assert (objeq.failure_trace = None);
         if List.mem field ignored then
           (true, objeq)
          else
            (false, { objeq with  failure_trace = Some (name, MissingXfrm []) }))
      (fun field objeq ->
         assert (objeq.failure_trace = None);
         if List.mem field ignored then
           (true, objeq)
          else
            (false, { objeq with  failure_trace = Some (name, MissingOrig []) }))
      (fun field val1 val2 objeq ->
         assert (objeq.failure_trace = None);
         matchobj name data seen objeq (val1, val2) |> extend_error field)

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
let match_objects_memo (name: string) matchobj ignored data seen objeq id1 id2 =
    if IntIntSet.mem (id1, id2) seen then begin
        (true, objeq)
    end else if IntIntMap.mem (id1, id2) objeq.objeq_cache then begin
        (IntIntMap.find (id1, id2) objeq.objeq_cache, objeq)
    end else begin
        let m1 = find_object_facts id1 data.facts1 data.pt1
        and m2 = find_object_facts id2 data.facts2 data.pt2
        and seen' = IntIntSet.add (id1, id2) seen
        and extend_cache (res, { objeq_cache; failure_trace }) =
          (res, { objeq_cache =
             IntIntMap.add (id1, id2) res objeq_cache; failure_trace }) in
        match_objects_raw name matchobj ignored data seen' objeq m1 m2
        |> extend_cache 
    end

(** [match_values_raw data seen objeq (v1, v2)] checks
 * whether two values [v1] and [v2] are isomorphic. For base values,
 * the values must  be identical, while for objects and functions,
 * we check object isomorphism. In the case of functions, two special
 * rules apply: the associated function implementation must match,
 * using [match_functions_associated], and in the object isomorphism, the
 * [toString] field is ignored. The role of [data], [seen] and [objeq]
 * and the return value as as for ]match_objects_memo]. *)
let rec match_values_raw (name: string) data seen objeq = function
    | (o1, o2)
        when (o1 = o2 && is_base o1) ->
            (true, objeq)
    | (OObject id1, OObject id2) ->
            match_objects_memo name match_values_raw [] data seen objeq id1 id2
    | (OOther (ty1, id1), OOther (ty2, id2))
        when ty1 = ty2 ->
            match_objects_memo name match_values_raw [] data seen objeq id1 id2
    | (OFunction (id1, fun1), OFunction (id2, fun2))
        when match_functions_associated data fun1 fun2 ->
            match_objects_memo name match_values_raw ["toString"] data  seen objeq
                 id1 id2
    | (o1, o2) ->
            (false, { objeq with failure_trace = Some (name, NonMatching ([], o1, o2) )})


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
let match_values name
    { funcs=funs1; points_to = pt1 }
    { funcs=funs2; points_to = pt2 }
    facts1 facts2 obj1 obj2 objeq =
    match_values_raw name { funs1; funs2; facts1; facts2; pt1; pt2 }
        IntIntSet.empty
        objeq
        (obj1, obj2)

(** [match_references rt1 rt2 facts1 facts2 r1 r2 objeq]
 * checks whether the versioned references [r1] and [r2] point to
 * isomporphic objects. *)
let match_refs name rt1 rt2 facts1 facts2 r1 r2 objeq =
    match_values name rt1 rt2 facts1 facts2
        (VersionReferenceMap.find r1 rt1.points_to)
        (VersionReferenceMap.find r2 rt2.points_to)
        objeq
