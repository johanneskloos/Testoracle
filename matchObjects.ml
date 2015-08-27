open PointsTo
open LocalFacts
open Trace
open Misc
open Richtrace
open Reference
open MatchTypes
open CamomileLibraryDefault.Camomile

type obj_match_failure =
  | NonMatching of string list * jsval * jsval
  | MissingOrig of string * string list
  | MissingXfrm of string * string list
  | Other of string

type fun_match_failure =
  | DifferentBodies of string * string
  | DifferentInstrumentedBodies of string * string
  | InconsistentlyInstrumented
  | DifferentExternal of int * int
  | InternalExternal

type failure_trace = obj_match_failure option
type named_failure_trace = (string * obj_match_failure) option 
type objeq = failure_trace Misc.IntIntMap.t
  
let is_base = function
    | OFunction _ | OObject _ | OOther _ -> false
    | _ -> true

type data = {
    funs1: functions;
    funs2: functions;
    facts1: local_facts;
    facts2: local_facts;
    pt1: points_to_map;
    pt2: points_to_map;
    noneq: IntIntSet.t
}

type cmpname = string
type recursive_matcher =
  data -> Misc.IntIntSet.t -> objeq -> jsval * jsval -> objeq * failure_trace

let body_regex =
	let open Pcre in
	regexp ~study:true ~flags:[`DOTALL; `MULTILINE] "{(.*)}"

let function_body_split = function
  | None ->
		None
  | Some s ->
		let open Pcre in
		try
			let sstr = exec ~rex:body_regex s in
			Some (get_substring sstr 1)
		with Not_found -> None

type whitespace_state = Initial | Pending | NotPending
let whitespace_set = let open UCharInfo in load_property_set `White_Space;;

let use_strict = Pcre.regexp "['\"]use ?strict[\"']( *;)? *"
let whitespace = Pcre.regexp "\\s+"
let empty_subst = Pcre.subst ""

let normalize str =
	let open Pcre in
	str |>
	replace ~rex:use_strict ~itempl:empty_subst |>
	replace ~rex:whitespace ~itempl:empty_subst 
  
(** Strict matching of functions. *)
let match_functions { funs1; funs2 } fun1 fun2 =
    Format.eprintf "Matching functions %d and %d@." fun1 fun2;
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
						let body1' = normalize body1
						and body2' = normalize body2 in
            if body1' = body2' then None else Some (DifferentBodies (body1', body2'))
          | None, None ->
            if i1 = i2 then None else Some (DifferentInstrumentedBodies (i1, i2))
          | _ ->
            Some InconsistentlyInstrumented
     end
    | (External id1, External id2) -> if id1 = id2 then None else Some (DifferentExternal (id1, id2))
    | _ -> Some InternalExternal

(** Associated matching of functions. This gives a coarse
 * over-approximation. *)
let match_functions_associated {funs1; funs2; noneq} fun1 fun2 =
    match funs1.(fun1), funs2.(fun2) with
    | (Local _, Local _) -> not (IntIntSet.mem (fun1, fun2) noneq) 
    | (External id1, External id2) -> id1 = id2
    | _ -> false

module StringMapExtra = MapExtra(StringMap);;

let match_objects_raw
    (matchobj: recursive_matcher)
    ignored data seen objeq m1 m2 =
    let fold fleft fright fboth =
        StringMapExtra.fold2
          (fun field _ -> function
            | (_, Some _) as r -> r
            | objeq -> fleft field objeq)
          (fun field _ -> function
            | (_, Some _) as r -> r
            | objeq -> fright field objeq)
          (fun field val1 val2 -> function
            | (_, Some _) as r -> r
            | objeq -> fboth field val1 val2 objeq)
            m1 m2 (objeq, None)
    and extend_error field (objeq_cache, failure_trace) =
      let failure_trace = match failure_trace with
      | Some (NonMatching (trace, v1, v2)) -> Some (NonMatching (field :: trace, v1, v2))
      | Some (MissingOrig (fld, tr)) -> Some (MissingOrig (fld, field :: tr))
      | Some (MissingXfrm (fld, tr)) -> Some (MissingXfrm (fld, field :: tr))
      | _ -> failure_trace in
      (objeq_cache, failure_trace) in
    fold
      (fun field (objeq_cache, failure_trace) ->
         assert (failure_trace = None);
         if List.mem field ignored then
           (objeq_cache, None)
          else
            (objeq_cache, Some (MissingXfrm (field, []))))
      (fun field (objeq_cache, failure_trace) ->
         assert (failure_trace = None);
         if List.mem field ignored then
           (objeq_cache, None)
          else
            (objeq_cache, Some (MissingOrig (field, []))))
      (fun field val1 val2 (objeq, failure_trace) ->
         assert (failure_trace = None);
				if List.mem field ignored then
					(objeq, None)
			  else
				  matchobj data seen objeq (val1, val2) |> extend_error field)

let match_objects_memo matchobj ignored data seen objeq id1 id2 =
	 	let id1' = get_object_id id1 and id2' = get_object_id id2 in
    if IntIntSet.mem (id1', id2') seen then begin
        (objeq, None)
    end else if IntIntMap.mem (id1', id2') objeq then begin
        (objeq, IntIntMap.find (id1', id2') objeq)
    end else begin
        let m1 = find_object_facts id1 data.facts1 data.pt1
        and m2 = find_object_facts id2 data.facts2 data.pt2
        and seen' = IntIntSet.add (id1', id2') seen
        and extend_cache (objeq, res) = (IntIntMap.add (id1', id2') res objeq, res) in
        match_objects_raw matchobj ignored data seen' objeq m1 m2
        |> extend_cache 
    end

let rec match_values_raw data seen objeq = function
	  | (ONumberFloat f1, ONumberFloat f2) ->
			  begin match classify_float f1, classify_float f2 with
				| FP_nan, FP_nan -> (objeq, None)
				| c1, c2 when c1 = c2 && f1 = f2 -> (objeq, None)
				| _, _ -> (objeq, Some (NonMatching ([], ONumberFloat f1, ONumberFloat f2))) end
    | (o1, o2)
        when (o1 = o2 && is_base o1) ->
            (objeq, None)
    | (OObject 0, OObject 0) ->
            (objeq, None) (* HACK HACK HACK *)
    | (OObject id1, OObject id2) ->
            match_objects_memo match_values_raw [] data seen objeq (Object id1) (Object id2)
    | (OOther (ty1, id1), OOther (ty2, id2))
        when ty1 = ty2 ->
            match_objects_memo match_values_raw [] data seen objeq (Other (ty1, id1)) (Other (ty2, id2))
    | (OFunction (id1, fun1), OFunction (id2, fun2))
        when match_functions_associated data fun1 fun2 ->
          (* Who thought that having a name property on functions was a good idea?
             And why do we have toString? *)
            match_objects_memo match_values_raw ["toString"; "name"; "length"; "*J$SID*"; "*J$IID*"] data  seen objeq
                 (Function (id1, fun1)) (Function (id2, fun2))
    | (o1, o2) ->
        (objeq, Some (NonMatching ([], o1, o2) ))


let match_values name
    { funcs = funs1; points_to = pt1 }
    { funcs = funs2; points_to = pt2 }
    facts1 facts2 noneq obj1 obj2 objeq:
    objeq * named_failure_trace =
    match
    match_values_raw { funs1; funs2; facts1; facts2; pt1; pt2; noneq }
        IntIntSet.empty
        objeq
        (obj1, obj2)
    with
    | (objeq, Some err) -> (objeq, Some (name, err))
    | (objeq, None) -> (objeq, None)

let match_refs name rt1 rt2 facts1 facts2 noneq r1 r2 objeq =
  try
    match_values name rt1 rt2 facts1 facts2 noneq
        (VersionReferenceMap.find r1 rt1.points_to)
        (VersionReferenceMap.find r2 rt2.points_to)
        objeq
  with Not_found -> Format.eprintf "Some ref not find in points_to"; raise Not_found 