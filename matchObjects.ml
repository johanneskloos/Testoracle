open TypesJS
open MatchTypes
open TraceTypes

type failure_trace = obj_match_failure option
type named_failure_trace = (string * obj_match_failure) option
type objeq = failure_trace IntIntMap.t

let is_base = function
  | OFunction _ | OObject _ | OOther _ -> false
  | _ -> true

type data = {
  funs1: functions;
  funs2: functions;
  facts1: rich_facts;
  facts2: rich_facts;
  pt1: Reference.points_to_map;
  pt2: Reference.points_to_map;
  noneq: IntIntSet.t
}

type cmpname = string
type recursive_matcher =
  data -> IntIntSet.t -> objeq ref -> jsval * jsval -> failure_trace

let body_regex =
  let open Pcre in
  regexp ~study: true ~flags:[`DOTALL; `MULTILINE] "{(.*)}"

let function_body_split = function
  | None ->
    None
  | Some s ->
    let open Pcre in
    try
      let sstr = exec ~rex: body_regex s in
      Some (get_substring sstr 1)
    with Not_found -> None

type whitespace_state = Initial | Pending | NotPending

let use_strict = Pcre.regexp "['\"]use ?strict[\"']( *;)? *"
let whitespace = Pcre.regexp "\\s+"
let empty_subst = Pcre.subst ""

let normalize str =
  let open Pcre in
  str |>
  replace ~rex: use_strict ~itempl: empty_subst |>
  replace ~rex: whitespace ~itempl: empty_subst

(** Strict matching of functions. *)
let match_functions { funs1; funs2 } fun1 fun2 =
  (* This is a hack. The function text used here can be either *)
  (* function name(args) { body } or (unknown). We say that two *)
  (* functions are "intensionally equivalent" if they have the same *)
  (* function body. This is, strictly speaking, not correct, but we *)
  (* simply use this as a heuristic to drive the search, and actually *)
  (* validate that guess using the trace. Note that the original  *)
  (* implementation blindly compared the two strings. This will not *)
  (* work: It is likely that the function names are different, due to *)
  (* the way the transform works. Thus, we compare the arguments and *)
  (* the body separately.*)
  (* XXX Consider using an even better comparison *)
  (* based on actually parsing the Javascript, at least removing *)
  (* comments and normalizing semicolons and whitespace. *)
  match (BatDynArray.get funs1 fun1, BatDynArray.get funs2 fun2) with
  | (ReflectedCode i1, ReflectedCode i2) ->
    if i1 = i2 then None else Some (DifferentInstrumentedBodies (i1, i2))
  | (OrigCode (_, u1), OrigCode (_, u2)) ->
    let body1' = normalize u1
    and body2' = normalize u2 in
    if body1' = body2' then None else Some (DifferentBodies (body1', body2'))
  | (OrigCode _, ReflectedCode _)
  | (ReflectedCode _, OrigCode _) ->
    Some InconsistentlyInstrumented
  | (External id1, External id2) -> if id1 = id2 then None else Some (DifferentExternal (id1, id2))
  | _ -> Some InternalExternal

(** Associated matching of functions. This gives a coarse
 * over-approximation. *)
let match_functions_associated { funs1; funs2; noneq } fun1 fun2 =
  match BatDynArray.get funs1 fun1, BatDynArray.get funs2 fun2 with
  | (OrigCode _, OrigCode _)
  | (ReflectedCode _, ReflectedCode _) -> not (IntIntSet.mem (fun1, fun2) noneq)
  | (External id1, External id2) -> id1 = id2
  | _ -> false

type map_result = OK of jsval * jsval | Fail of bool

let match_objects_raw
    (matchobj: recursive_matcher)
    ignored data seen objeq m1 m2 =
  let mboth =
    StringMap.merge (fun field ov1 ov2 ->
        if List.mem field ignored then
          None
        else
          match ov1, ov2 with
          | Some v1, Some v2 -> Some (OK (v1, v2))
          | Some _, None -> Some (Fail true)
          | None, Some _ -> Some (Fail false)
          | None, None -> failwith "Bad merge")
      m1 m2
  and extend_error field failure_trace =
    match failure_trace with
    | Some (NonMatching (trace, v1, v2)) -> Some (NonMatching (field :: trace, v1, v2))
    | Some (MissingOrig (fld, tr)) -> Some (MissingOrig (fld, field :: tr))
    | Some (MissingXfrm (fld, tr)) -> Some (MissingXfrm (fld, field :: tr))
    | _ -> failure_trace
  in StringMap.fold (fun field vals -> function
      | Some _ as err -> err
      | None -> match vals with
        | OK (v1, v2) ->
          matchobj data seen objeq (v1, v2) |> extend_error field
        | Fail true -> Some (MissingXfrm (field, []))
        | Fail false -> Some (MissingOrig (field, [])))
    mboth None

let match_objects_memo matchobj ignored data seen objeq id1 id2 =
  let id1' = get_object_id id1 and id2' = get_object_id id2 in
  if IntIntSet.mem (id1', id2') seen then begin
    None
  end else if IntIntMap.mem (id1', id2') !objeq then begin
    IntIntMap.find (id1', id2') !objeq
  end else begin
    let m1 = PointsTo.find_object_facts id1 data.facts1 data.pt1
    and m2 = PointsTo.find_object_facts id2 data.facts2 data.pt2
    and seen' = IntIntSet.add (id1', id2') seen
    and extend_cache res = objeq := IntIntMap.add (id1', id2') res !objeq; res in
    match_objects_raw matchobj ignored data seen' objeq m1 m2
    |> extend_cache
  end

let rec match_values_raw data seen objeq = function
  | (ONumberFloat f1, ONumberFloat f2) ->
    begin match classify_float f1, classify_float f2 with
      | FP_nan, FP_nan -> None
      | c1, c2 when c1 = c2 && f1 = f2 -> None
      | _, _ -> Some (NonMatching ([], ONumberFloat f1, ONumberFloat f2)) end
  | (o1, o2)
    when (o1 = o2 && is_base o1) ->
    None
  | (OObject 0, OObject 0) ->
    None (* HACK HACK HACK *)
  | (OObject id1, OObject id2) ->
    match_objects_memo match_values_raw [] data seen objeq (Object id1) (Object id2)
  | (OOther (ty1, id1), OOther (ty2, id2))
    when ty1 = ty2 ->
    match_objects_memo match_values_raw [] data seen objeq (Other (ty1, id1)) (Other (ty2, id2))
  | (OFunction (id1, fun1), OFunction (id2, fun2))
    when match_functions_associated data fun1 fun2 ->
    (* Who thought that having a name property on functions was a good
     * idea? And why do we have toString? *)
    match_objects_memo match_values_raw
      ["toString"; "name"; "length"; "*J$SID*"; "*J$IID*"; "apply"; "call"] data seen objeq
      (Function (id1, fun1)) (Function (id2, fun2))
  | (o1, o2) ->
    Some (NonMatching ([], o1, o2) )

let match_values name = fun
  { funcs = funs1; points_to = pt1 }
  { funcs = funs2; points_to = pt2 }
  facts1 facts2 noneq obj1 obj2 objeq ->
  match
    match_values_raw { funs1; funs2; facts1; facts2; pt1; pt2; noneq }
      IntIntSet.empty
      objeq
      (obj1, obj2)
  with
  | Some err -> Some (name, err)
  | None -> None

let match_refs_naming name (r1, _) (r2, _) = let open Reference in match r1, r2 with
  | Variable (_, v1), Variable (_, v2) when v1 = v2 -> None
  | Field (_, f1), Field (_, f2) when f1 = f2 -> None
  | _, _ -> Some (name, Other "Non-matching reference names")

let match_refs name rt1 rt2 facts1 facts2 noneq r1 r2 objeq =
  try
    match match_refs_naming name r1 r2 with
    | None ->
      match_values name rt1 rt2 facts1 facts2 noneq
        (Reference.VersionedReferenceMap.find rt1.points_to r1)
        (Reference.VersionedReferenceMap.find rt2.points_to r2)
        objeq
    | Some _ as r -> r
  with Not_found -> Format.eprintf "Some ref not find in points_to"; raise Not_found
