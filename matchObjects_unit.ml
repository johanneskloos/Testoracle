open MatchObjects
open Kaputt.Abbreviations
open Types
open TestBaseData
open MatchTypes
open TraceTypes
let (|>) = Pervasives.(|>)

let test_data = {
  funs1 = functab1;
  funs2 = functab2;
  facts1 = local_facts_1;
  facts2 = local_facts_2;
  pt1 = points_to_1;
  pt2 = points_to_2;
  noneq = Misc.IntIntSet.empty
}

let is_base_test_true (name, value) =
  Test.make_simple_test ~title:("is_base - " ^ name) (fun () ->	Assert.is_true (is_base value))
let is_base_test_false (name, value) =
  Test.make_simple_test ~title:("is_base - " ^ name) (fun () ->	Assert.is_false (is_base value))

let is_base_tests =
  List.map is_base_test_true [
    ("undefined", OUndefined);
    ("boolean", OBoolean true);
    ("int", ONumberInt 42);
    ("float", ONumberFloat 3.14);
    ("string", OString "xyzzy");
    ("symbol", OSymbol "foobar");
    ("null", ONull);
  ] @
  List.map is_base_test_false [
    ("function", OFunction (1, 2));
    ("object", OObject 3);
    ("other", OOther ("ty", 4))
  ]

module ValPairMap = Map.Make(struct
    type t = jsval * jsval
    let compare = Pervasives.compare
  end);;
module ValPairMapFormat = FormatHelper.MapFormat(ValPairMap);;

let print_reason = Misc.to_string MatchTypes.pp_fun_match_failure

let match_functions_ins_ins_eq =
  Test.make_simple_test ~title:"match_functions - equal instrumented code"
    (fun () -> assert_is_None ~prn:print_reason (match_functions test_data 4 3))

let match_functions_ins_ins_neq =
  Test.make_simple_test ~title:"match_functions - non-equal instrumented code"
    (fun () -> assert_is_Some ~prn:print_reason (match_functions test_data 3 3))

let match_functions_nins_eq =
  Test.make_simple_test ~title:"match_functions - equal non-instrumented code"
    (fun () -> assert_is_None ~prn:print_reason (match_functions test_data 5 4))

let match_functions_nins_ins =
  Test.make_simple_test ~title:"match_functions - instrumented vs. uninstrumented code"
    (fun () -> assert_is_Some ~prn:print_reason (match_functions test_data 5 3))

let match_functions_ext_eq =
  Test.make_simple_test ~title:"match_functions - equal external code"
    (fun () -> assert_is_None ~prn:print_reason (match_functions test_data 0 0))

let match_functions_ext_neq =
  Test.make_simple_test ~title:"match_functions - non-equal external code"
    (fun () -> assert_is_Some ~prn:print_reason (match_functions test_data 0 1))

let match_functions_ext_ins =
  Test.make_simple_test ~title:"match_functions - instrumented vs. external code"
    (fun () -> assert_is_Some ~prn:print_reason (match_functions test_data 4 1))

let match_functions_nins_ext =
  Test.make_simple_test ~title:"match_functions - uninstrumented vs. external code"
    (fun () -> assert_is_Some ~prn:print_reason (match_functions test_data 5 1))

let match_functions_tests =
  [match_functions_ins_ins_eq; match_functions_ins_ins_neq; match_functions_nins_eq;
   match_functions_nins_ins; match_functions_ext_eq; match_functions_ext_neq;
   match_functions_ext_ins; match_functions_nins_ext]

let testobj1 =
  Misc.StringMap.empty
  |> Misc.StringMap.add "f1" v1
  |> Misc.StringMap.add "f2" obj1_cyc1
  |> Misc.StringMap.add "ign1" vundef
  |> Misc.StringMap.add "ign2" vnull

let testobj2 =
  Misc.StringMap.empty
  |> Misc.StringMap.add "f1" v1
  |> Misc.StringMap.add "f2" obj2_cyc1
  |> Misc.StringMap.add "ign1" vnull
  |> Misc.StringMap.add "ign3" vundef

let testobj2' =
  Misc.StringMap.empty
  |> Misc.StringMap.add "f1" v1
  |> Misc.StringMap.add "f2" obj2_cyc2
  |> Misc.StringMap.add "ign1" vnull
  |> Misc.StringMap.add "ign3" vundef

let test_tab =
  ValPairMap.empty
  |> ValPairMap.add (v1, v1) true
  |> ValPairMap.add (v0, v0) true
  |> ValPairMap.add (obj1_cyc1, obj2_cyc1) true
  |> ValPairMap.add (obj1_cyc1, obj2_cyc2) false
  |> ValPairMap.add (obj1_cyc2, obj2_cyc2) true
  |> ValPairMap.add (obj1_cyc2, obj2_cyc3) false

module IIA = Assert.Set(Misc.IntIntSet)
               (struct
                  type t = int * int
                  let to_string = Misc.to_string Misc.pp_print_int_pair
                end)
let same_data
      { funs1 = funs11;
        funs2 = funs21;
        facts1 = facts11;
        facts2 = facts21;
        pt1 = pt11;
        pt2 = pt21;
        noneq = noneq1 }
      { funs1 = funs12;
        funs2 = funs22;
        facts1 = facts12;
        facts2 = facts22;
        pt1 = pt12;
        pt2 = pt22;
        noneq = noneq2 } =
  same_functions funs11 funs12;
  same_functions funs21 funs22;
  same_local_facts facts11 facts12;
  same_local_facts facts21 facts22;
  AssertVersionedReferenceMap.make_equal (=) (Misc.to_string pp_jsval)
    pt11 pt12;
  AssertVersionedReferenceMap.make_equal (=) (Misc.to_string pp_jsval)
    pt21 pt22;
  IIA.equal noneq1 noneq2 

let matcher_stub seen_tab data cycle_set objeq vals =
  Format.eprintf "Considering value pair %a@."
    (FormatHelper.pp_print_pair pp_jsval pp_jsval) vals;
  same_data test_data data;
  seen_tab := ValPairMap.add vals
      (1 + try ValPairMap.find vals !seen_tab with Not_found -> 0)
      !seen_tab;
  try
    if ValPairMap.find vals test_tab then None else Some (MatchTypes.Other "testing")
  with
    Not_found ->
    Assert.fail_msg ("Trying to look up " ^
                     Misc.to_string (FormatHelper.pp_print_pair pp_jsval pp_jsval) vals) 

let good_ignore = [ "ign1"; "ign2"; "ign3" ]

let pp_seen_map =
  ValPairMapFormat.pp_print_map_default
    (FormatHelper.pp_print_pair pp_jsval pp_jsval)
    (Format.pp_print_int)

let eq_seen_map = ValPairMap.equal (=)
let assert_equal_seen_map =	Assert.make_equal eq_seen_map (Misc.to_string pp_seen_map)

let test_match_objects_raw_obj1_obj2_good_ignore =
  Test.make_simple_test ~title:"match_objects_raw - obj1 vs. obj2, good ignore set"
    (fun () ->
       let seen = ref ValPairMap.empty
       and objeq = ref Misc.IntIntMap.empty in
       assert_is_None (match_objects_raw (matcher_stub seen) good_ignore test_data Misc.IntIntSet.empty objeq testobj1 testobj2);
       assert_equal_seen_map
         (ValPairMap.empty |> ValPairMap.add (v1, v1) 1 |> ValPairMap.add (obj1_cyc1, obj2_cyc1) 1)
         !seen
    )

let test_match_objects_raw_obj1_obj2_not_ignoring_2 =
  Test.make_simple_test ~title:"match_objects_raw - obj1 vs. obj2, not ignoring ign2"
    (fun () ->
       let seen = ref ValPairMap.empty
       and objeq = ref Misc.IntIntMap.empty in
       assert_is_Some (match_objects_raw (matcher_stub seen) ["ign1";"ign3"] test_data Misc.IntIntSet.empty objeq testobj1 testobj2);
    )

let test_match_objects_raw_obj1_obj2_not_ignoring_3 =
  Test.make_simple_test ~title:"match_objects_raw - obj1 vs. obj2, not ignoring ign3"
    (fun () ->
       let seen = ref ValPairMap.empty
       and objeq = ref Misc.IntIntMap.empty in
       assert_is_Some (match_objects_raw (matcher_stub seen) ["ign1";"ign2"] test_data Misc.IntIntSet.empty objeq testobj1 testobj2);
    )

let test_match_objects_raw_obj1_obj2_not_ignoring_1 =
  Test.make_simple_test ~title:"match_objects_raw - obj1 vs. obj2, not ignoring ign1"
    (fun () ->
       let seen = ref ValPairMap.empty
       and objeq = ref Misc.IntIntMap.empty in
       Assert.raises ~msg:"Expected Not_found exception"
         (fun () -> match_objects_raw (matcher_stub seen) ["ign3";"ign2"] test_data Misc.IntIntSet.empty objeq testobj1 testobj2)
    )

let test_match_objects_raw_obj1_obj2'_good_ignore =
  Test.make_simple_test ~title:"match_objects_raw - obj1 vs. obj2', good ignore set"
    (fun () ->
       let seen = ref ValPairMap.empty
       and objeq = ref Misc.IntIntMap.empty in
       assert_is_Some (match_objects_raw (matcher_stub seen) good_ignore test_data Misc.IntIntSet.empty objeq testobj1 testobj2');
       Assert.equal_int 1 (ValPairMap.find (obj1_cyc1, obj2_cyc2) !seen)
    )

let obj1desc_cyc1 =
  simplefields [ ("next", obj1_cyc2); ("val", v0) ]

let test_match_objects_memo_cyc1_cyc1 =
  Test.make_simple_test ~title:"match_objects_memo - cyc1 vs. cyc1, uncached"
    (fun () ->
       let seen = ref ValPairMap.empty
       and objeq = ref Misc.IntIntMap.empty
       and id1 = objectid_of_jsval obj1_cyc1
       and id2 = objectid_of_jsval obj2_cyc1 in
       assert_is_None ~msg:"Return value" (match_objects_memo (matcher_stub seen) ["toString"] test_data Misc.IntIntSet.empty objeq id1 id2);
       assert_is_None ~msg:"Cached values" (Misc.IntIntMap.find (get_object_id id1, get_object_id id2) !objeq)
    )

let test_match_objects_memo_cyc1_cyc2 =
  Test.make_simple_test ~title:"match_objects_memo - cyc1 vs. cyc2, uncached"
    (fun () ->
       let seen = ref ValPairMap.empty
       and objeq = ref Misc.IntIntMap.empty
       and id1 = objectid_of_jsval obj1_cyc1
       and id2 = objectid_of_jsval obj2_cyc2 in
       assert_is_Some ~msg:"Return value" (match_objects_memo (matcher_stub seen) ["toString"] test_data Misc.IntIntSet.empty objeq id1 id2);
       assert_is_Some ~msg:"Cached values" (Misc.IntIntMap.find (get_object_id id1, get_object_id id2) !objeq)
    )

let test_match_objects_memo_cyc3_cyc3_seen_cache =
  Test.make_simple_test ~title:"match_objects_memo - cyc3 vs. cyc3, seen case"
    (fun () ->
       let seen = ref ValPairMap.empty
       and objeq = ref Misc.IntIntMap.empty
       and id1 = objectid_of_jsval obj1_cyc3
       and id2 = objectid_of_jsval obj2_cyc3 in
       let cycle_seen = Misc.IntIntSet.empty |> Misc.IntIntSet.add (get_object_id id1, get_object_id id2) in
       assert_is_None (match_objects_memo (matcher_stub seen) ["toString"] test_data cycle_seen objeq id1 id2)
    )

let test_match_objects_memo_cyc3_cyc3_objeq_cache =
  Test.make_simple_test ~title:"match_objects_memo - cyc3 vs. cyc3, objeq case"
    (fun () ->
       let seen = ref ValPairMap.empty
       and id1 = objectid_of_jsval obj1_cyc3
       and id2 = objectid_of_jsval obj2_cyc3
       and cycle_seen = Misc.IntIntSet.empty in
       let objeq = ref (Misc.IntIntMap.empty |> Misc.IntIntMap.add (get_object_id id1, get_object_id id2) None) in
       assert_is_None (match_objects_memo (matcher_stub seen) ["toString"] test_data cycle_seen objeq id1 id2)
    )

let test_match_objects_memo_cyc3_cyc3_objeq_cache_fail =
  Test.make_simple_test ~title:"match_objects_memo - cyc3 vs. cyc3, objeq case (negative)"
    (fun () ->
       let seen = ref ValPairMap.empty
       and id1 = objectid_of_jsval obj1_cyc3
       and id2 = objectid_of_jsval obj2_cyc3
       and cycle_seen = Misc.IntIntSet.empty
       and msg = MatchTypes.Other "mark" in
       let objeq = ref (Misc.IntIntMap.empty |> Misc.IntIntMap.add (get_object_id id1, get_object_id id2) (Some msg)) in
       Assert.make_equal (=)
         (function
              None -> "None"
            | Some (NonMatching (_, _, _)) -> "non-matching elements ..."
            | Some (MissingOrig (n, _)) -> "missing orig called " ^ n ^ " at ..."
            | Some (MissingXfrm (n, _)) -> "missing xfrm called " ^ n ^ " at ..."
            | Some (Other msg) -> msg)
         (Some msg)
         (match_objects_memo (matcher_stub seen) ["toString"] test_data cycle_seen objeq id1 id2)
    )

let test_match_raw_values_v1_v1 =
  Test.make_simple_test ~title:"match_values_raw - equal ints"
    (fun () ->
       let objeq = ref Misc.IntIntMap.empty in
       assert_is_None (match_values_raw test_data Misc.IntIntSet.empty objeq (v1, v1));
       Assert.equal_int 0 (Misc.IntIntMap.cardinal !objeq))

let test_match_raw_values_cyc1_cyc1 =
  Test.make_simple_test ~title:"match_values_raw - cyc1 vs. cyc1"
    (fun () ->
       let objeq = ref Misc.IntIntMap.empty in
       assert_is_None (match_values_raw test_data Misc.IntIntSet.empty objeq (obj1_cyc1, obj2_cyc1)))

let test_match_raw_values_cyc1_cyc2 =
  Test.make_simple_test ~title:"match_values_raw - cyc1 vs. cyc2"
    (fun () ->
       let objeq = ref Misc.IntIntMap.empty in
       assert_is_Some (match_values_raw test_data Misc.IntIntSet.empty objeq (obj1_cyc1, obj2_cyc2)))

let test_match_values_v1_v1 =
  Test.make_simple_test ~title:"match_values - equal ints"
    (fun () ->
       let objeq = ref Misc.IntIntMap.empty in
       assert_is_None (match_values "XYZ" test_rt1 test_rt2 test_lf1 test_lf2 Misc.IntIntSet.empty v1 v1 objeq);
       Assert.equal_int 0 (Misc.IntIntMap.cardinal !objeq))

let test_match_values_cyc1_cyc1 =
  Test.make_simple_test ~title:"match_values - cyc1 vs. cyc1"
    (fun () ->
       let objeq = ref Misc.IntIntMap.empty in
       assert_is_None (match_values "XYZ" test_rt1 test_rt2 test_lf1 test_lf2 Misc.IntIntSet.empty obj1_cyc1 obj2_cyc1 objeq))

let test_match_values_cyc1_cyc2 =
  Test.make_simple_test ~title:"match_values - cyc1 vs. cyc2"
    (fun () ->
       let objeq = ref Misc.IntIntMap.empty in
       assert_is_Some (match_values "XYZ" test_rt1 test_rt2 test_lf1 test_lf2 Misc.IntIntSet.empty obj1_cyc1 obj2_cyc2 objeq))

let tests = (is_base_tests @ [
    match_functions_ins_ins_eq;
    match_functions_ins_ins_neq;
    match_functions_nins_eq;
    match_functions_nins_ins;
    match_functions_ext_eq;
    match_functions_ext_neq;
    match_functions_ext_ins;
    match_functions_nins_eq;
    test_match_objects_raw_obj1_obj2_good_ignore;
    test_match_objects_raw_obj1_obj2_not_ignoring_1;
    test_match_objects_raw_obj1_obj2_not_ignoring_2;
    test_match_objects_raw_obj1_obj2_not_ignoring_3;
    test_match_objects_raw_obj1_obj2'_good_ignore;
    test_match_objects_memo_cyc1_cyc1;
    test_match_objects_memo_cyc1_cyc2;
    test_match_objects_memo_cyc3_cyc3_seen_cache;
    test_match_objects_memo_cyc3_cyc3_objeq_cache;
    test_match_objects_memo_cyc3_cyc3_objeq_cache_fail;
    test_match_raw_values_v1_v1;
    test_match_raw_values_cyc1_cyc1;
    test_match_raw_values_cyc1_cyc2;
    test_match_values_v1_v1;
    test_match_values_cyc1_cyc1;
    test_match_values_cyc1_cyc2
  ])
