open MatchObjects;;
open Kaputt;;
open Abbreviations;;
open LocalFacts;;
open Trace;;
open Richtrace;;
open PointsTo;;
open Reference;;
open Misc;;
open FormatHelper;;

let (|>) = Pervasives.(|>)

(** Tests for the function matcher. *)
let funs1 =
    [| Local { instrumented="random stuff 1"; uninstrumented="function name (args) 1" };
       Local { instrumented="random stuff 2"; uninstrumented="function name (args) 2" };
       Local { instrumented="random stuff 3"; uninstrumented="(unknown)" };
       Local { instrumented="random stuff 4"; uninstrumented="(unknown)" };
       External 0;
       External 1 |];;
let funs2 =
    [| External 1;
       Local { instrumented="random stuff 1"; uninstrumented="function name (args) 1'" };
       Local { instrumented="random stuff 2"; uninstrumented="function name' (args) 2" };
       Local { instrumented="random stuff 3"; uninstrumented="(unknown)" };
       Local { instrumented="random stuff 4"; uninstrumented="(unknown)" };
       External 0 |];;

let fundata =
    { funs1; funs2;
    facts1 = empty_local_facts; facts2 = empty_local_facts;
    pt1 = VersionReferenceMap.empty; pt2 = VersionReferenceMap.empty }

let test_funs_1 =
    Test.make_simple_test ~title:"Equal instrumented functions" (fun () ->
        Assert.is_true (match_functions fundata 1 2));;
let test_funs_2 =
    Test.make_simple_test ~title:"Non-equal instrumented functions" (fun () ->
        Assert.is_false (match_functions fundata 0 1));;
let test_funs_3 =
    Test.make_simple_test ~title:"Equal non-instrumented functions" (fun () ->
        Assert.is_true (match_functions fundata 2 3));;
let test_funs_4 =
    Test.make_simple_test ~title:"Non-equal non-instrumented functions" (fun () ->
        Assert.is_false (match_functions fundata 2 4));;
let test_funs_5 =
    Test.make_simple_test ~title:"Equal external functions" (fun () ->
        Assert.is_true (match_functions fundata 4 5));;
let test_funs_6 =
    Test.make_simple_test ~title:"Non-equal external functions" (fun () ->
        Assert.is_false (match_functions fundata 4 0));;
let test_funs_7 =
    Test.make_simple_test ~title:"Instrumented vs. non-instrumented functions" (fun () ->
        Assert.is_false (match_functions fundata 1 3));;
let test_funs_8 =
    Test.make_simple_test ~title:"Instrumented vs. external functions" (fun () ->
        Assert.is_false (match_functions fundata 1 5));;
let test_funs_9 =
    Test.make_simple_test ~title:"Non-instrumented vs. external functions" (fun () ->
        Assert.is_false (match_functions fundata 3 5));;

(** Testing the raw object matcher, using a mock object comparator. *)
(* Format: ignored, m1, m2, added tuples. The objmatcher is mocked,
 * and objeq and seen are chosen arbitrarily (and checked by the mock matcher). *)
let (++) (k,v) m = StringMap.add k v m
let emp = StringMap.empty
let obj1 = ("foo", ONumberInt 0) ++ emp
let obj2 = ("ign", ONumberInt 1) ++ obj1
let obj3 = ("bar", ONumberInt 2) ++ obj1

let tests_mor_positive = [
    ("two empties, no ignore", emp, emp, []);
    ("two empties, ingore", emp, emp, ["ign"]);
    ("obj1, no ignore", obj1, obj1, []);
    ("obj1, ignore", obj1, obj1, ["ign"]);
    ("obj3, no ignore", obj3, obj3, []);
    ("obj1, obj2 with ignore", obj1, obj2, ["ign"]);
    ("obj12 obj1 with ignore", obj2, obj1, ["ign"])
]
let tests_mor_negative = [
    ("empty vs obj1", emp, obj1, []);
    ("obj1 vs obj2", obj1, obj2, []);
    ("obj2 vs obj3", obj2, obj3, []);
    ("obj1 vs obj2 with ignore", obj1, obj2, ["foo"])
]

let pp_objeq =
    IntIntMapFormat.pp_print_map_default pp_print_int_pair
        Format.pp_print_bool

let objeq_to_string = to_string pp_objeq
        
let objeq_option_to_string = function
    | None -> "None"
    | Some objeq -> objeq_to_string objeq

let same_objeq_option =
    Assert.make_equal (=) objeq_option_to_string

let equal_objeq =
    Assert.make_equal (IntIntMap.equal (=)) objeq_to_string

let equal_objeq_option =
    Assert.make_equal (fun exp got ->
        match exp, got with
        | None, None -> true
        | Some exp, Some got -> IntIntMap.equal (=) exp got
        | _ -> false)
        objeq_option_to_string

let gen_test_match_objects_raw_positive (msg, m1, m2, ignore) =
    Test.make_simple_test ~title:msg (fun () ->
        let dummy_seen = IntIntSet.add (47, 11) IntIntSet.empty
        and dummy_objeq = IntIntMap.add (24, 601) true IntIntMap.empty in
        let mock_match data seen objeq _ =
            Assert.same fundata data;
            Assert.same dummy_seen seen;
            Assert.same dummy_objeq objeq;
            (true, objeq) in
        match_objects_raw mock_match ignore fundata dummy_seen
            (Some dummy_objeq) m1 m2
        |> same_objeq_option (Some dummy_objeq))

let gen_test_match_objects_raw_negative (msg, m1, m2, ignore) =
    Test.make_simple_test ~title:msg (fun () ->
        let dummy_seen = IntIntSet.add (47, 11) IntIntSet.empty
        and dummy_objeq = IntIntMap.add (24, 601) true IntIntMap.empty in
        let mock_match data seen objeq _ =
            Assert.same fundata data;
            Assert.same dummy_seen seen;
            Assert.same dummy_objeq objeq;
            (true, objeq) in
        match_objects_raw mock_match ignore fundata dummy_seen
            (Some dummy_objeq) m1 m2
        |> same_objeq_option None)

let tests_match_objects_raw_positive =
    List.map gen_test_match_objects_raw_positive tests_mor_positive

let tests_match_objects_raw_negative =
    List.map gen_test_match_objects_raw_negative tests_mor_negative

let test_match_objects_raw_targeted_objeq_update =
    Test.make_simple_test ~title:"Testing targeted objeq updates"
        (fun () ->
            let mock_match data _ objeq (val1, _) =
                Assert.make_equal (=) (to_string Trace.pp_jsval)
                        (ONumberInt 0) val1;
                (true, IntIntMap.add (0, 0) true objeq) in
            match_objects_raw mock_match ["ign"] fundata IntIntSet.empty
                    (Some (IntIntMap.add (1,1) true IntIntMap.empty)) obj1 obj2
            |> equal_objeq_option
                (Some (IntIntMap.add (0, 0) true
                    (IntIntMap.add (1,1) true IntIntMap.empty))))

let test_match_objects_objmatch_fails =
    Test.make_simple_test ~title:"Testing failing objmatch"
        (fun () ->
            let mock_match _ _ objeq _ = (false, objeq) in
            match_objects_raw mock_match [] fundata IntIntSet.empty
                (Some IntIntMap.empty) obj1 obj1 
            |> equal_objeq_option None)

let test_match_objects_objmatch_fails_on_ignored =
    Test.make_simple_test ~title:"Testing failing objmatch for onsided"
        (fun () ->
            let mock_match _ _ objeq (val1, val2) =
                (not (val1 = ONumberInt 1 || val2 = ONumberInt 1), objeq) in
            match_objects_raw mock_match ["ign"] fundata IntIntSet.empty
                (Some IntIntMap.empty) obj1 obj2
            |> equal_objeq_option (Some IntIntMap.empty);
            match_objects_raw mock_match ["ign"] fundata IntIntSet.empty
                (Some IntIntMap.empty) obj2 obj1
            |> equal_objeq_option (Some IntIntMap.empty))

(** Test the memoization.
 * We do not retest all the matching properties from above,
 * since we know that match_objects_raw should work. *)
(* Simulate the objects from above. *)
let emp = StringMap.empty
let obj1 = ("foo", ONumberInt 0) ++ emp
let obj2 = ("ign", ONumberInt 1) ++ obj1
let obj3 = ("bar", ONumberInt 2) ++ obj1

let add_field obj fld ver vmap =
    ReferenceMap.add (reference_of_fieldref (obj, fld)) ver vmap
let empty_vmap = ReferenceMap.empty
let add_data obj fld ver data pmap =
    VersionReferenceMap.add (reference_of_fieldref (obj, fld), ver) data pmap

let obj_fields1 =
    empty_vmap
        |> add_field 1 "foo" 0
        |> add_field 2 "foo" 0
        |> add_field 2 "ign" 0
        |> add_field 3 "foo" 0
        |> add_field 3 "bar" 0
let obj_fields2 =
    empty_vmap
        |> add_field 1 "foo" 1
        |> add_field 2 "foo" 1
        |> add_field 2 "ign" 1
        |> add_field 3 "foo" 1
        |> add_field 3 "bar" 1
let pt =
    VersionReferenceMap.empty
        |> add_data 1 "foo" 0 (ONumberInt 0)
        |> add_data 1 "foo" 1 (ONumberInt 0)
        |> add_data 2 "foo" 0 (ONumberInt 0)
        |> add_data 2 "foo" 1 (ONumberInt 0)
        |> add_data 3 "foo" 0 (ONumberInt 0)
        |> add_data 3 "foo" 1 (ONumberInt 0)
        |> add_data 2 "ign" 0 (ONumberInt 2)
        |> add_data 2 "ign" 1 (ONumberInt 42)
        |> add_data 3 "bar" 0 (ONumberInt 3)
        |> add_data 3 "bar" 1 (ONumberInt 3)


let memodata = {
    fundata with
    facts1 = { empty_local_facts with versions = obj_fields1 };
    facts2 = { empty_local_facts with versions = obj_fields2 };
    pt1 = pt;
    pt2 = pt
}

let int_int_set_to_string =
    to_string (IntIntSetFormat.pp_print_gen_set pp_print_int_pair)

let test_match_objects_memo_unmemoized_success =
    let mock_seen = IntIntSet.empty
    and objeq = IntIntMap.add (17,18) true IntIntMap.empty in
    let mock_matchobj data seen objeq (v1, v2) =
        Assert.make_equal IntIntSet.equal int_int_set_to_string (IntIntSet.add (1,1) mock_seen) seen;
        Assert.same ~msg:"same data" data memodata;
        Assert.equal (ONumberInt 0) v1;
        Assert.equal (ONumberInt 0) v2;
        (true, IntIntMap.add (1,1) true objeq) in
    Test.make_simple_test
        ~title:"Testing memoized objmatch, unmemoized success"
        (fun () ->
            let (res, objeq') = match_objects_memo mock_matchobj ["ign"]
                memodata mock_seen objeq 1 1 in
            Assert.is_true res;
            equal_objeq (IntIntMap.add (1,1) true objeq) objeq')

let test_match_objects_memo_unmemoized_failure =
    let mock_seen = IntIntSet.empty
    and objeq = IntIntMap.add (17,18) true IntIntMap.empty in
    let mock_matchobj data seen objeq (v1, v2) =
        Assert.make_equal IntIntSet.equal int_int_set_to_string (IntIntSet.add (1,2) mock_seen) seen;
        Assert.same data memodata;
        Assert.equal (ONumberInt 0) v1;
        Assert.equal (ONumberInt 0) v2;
        (true, IntIntMap.add (1,1) true objeq) in
    Test.make_simple_test
        ~title:"Testing memoized objmatch, unmemoized failure"
        (fun () ->
            let (res, objeq') = match_objects_memo mock_matchobj []
                memodata mock_seen objeq 1 2 in
            Assert.is_false res;
            equal_objeq (IntIntMap.add (1,2) false objeq) objeq')

let test_match_objects_memo_in_map_success =
    let mock_seen = IntIntSet.empty
    and objeq = IntIntMap.add (17,18) true IntIntMap.empty in
    let mock_matchobj _ _ _ _ =
        Assert.fail_msg "This code shouldn't be reached" in
    Test.make_simple_test
        ~title:"Testing memoized objmatch, memoized success"
        (fun () ->
            let (res, objeq') = match_objects_memo mock_matchobj []
                memodata mock_seen objeq 17 18 in
            Assert.is_true res;
            Assert.same objeq objeq')

let test_match_objects_memo_in_map_failure =
    let mock_seen = IntIntSet.empty
    and objeq = IntIntMap.add (17,18) false IntIntMap.empty in
    let mock_matchobj _ _ _ _ =
        Assert.fail_msg "This code shouldn't be reached" in
    Test.make_simple_test
        ~title:"Testing memoized objmatch, memoized failure"
        (fun () ->
            let (res, objeq') = match_objects_memo mock_matchobj []
                memodata mock_seen objeq 17 18 in
            Assert.is_false res;
            Assert.same objeq objeq')

let test_match_objects_memo_cycle_cut =
    let mock_seen = IntIntSet.add (47, 11) IntIntSet.empty
    and objeq = IntIntMap.add (17,18) true IntIntMap.empty in
    let mock_matchobj _ _ _ _ =
        Assert.fail_msg "This code shouldn't be reached" in
    Test.make_simple_test
        ~title:"Testing memoized objmatch, memoized success"
        (fun () ->
            let (res, objeq') = match_objects_memo mock_matchobj []
                memodata mock_seen objeq 47 11 in
            Assert.is_true res;
            Assert.same objeq objeq')

(** Test value matching.
 * We start with simple, flat objects to test the general functionality.
 * Later on, we test with deeper objects structures, and finally with
 * cyclic structures.
 *)
let test_match_values_base_success =
    let mock_seen = IntIntSet.add (47, 11) IntIntSet.empty
    and objeq = IntIntMap.add (17,18) true IntIntMap.empty in
    Test.make_simple_test ~title:"Testing value matching: Same base value"
        (fun () ->
            let (res, objeq') =
                match_values_raw memodata mock_seen objeq
                        (ONumberInt 1, ONumberInt 1)
            in
            Assert.is_true res;
            Assert.same objeq objeq')

let test_match_values_base_failure =
    let mock_seen = IntIntSet.add (47, 11) IntIntSet.empty
    and objeq = IntIntMap.add (17,18) true IntIntMap.empty in
    Test.make_simple_test ~title:"Testing value matching: Different base value"
        (fun () ->
            let (res, objeq') =
                match_values_raw memodata mock_seen objeq
                        (ONumberInt 1, ONumberInt 2)
            in
            Assert.is_false res;
            Assert.same objeq objeq')

let test_match_values_base_nonbase =
    let mock_seen = IntIntSet.add (47, 11) IntIntSet.empty
    and objeq = IntIntMap.add (17,18) true IntIntMap.empty in
    Test.make_simple_test ~title:"Testing value matching: Base vs. non-base"
        (fun () ->
            let (res, objeq') =
                match_values_raw memodata mock_seen objeq
                        (ONumberInt 1, OObject 2)
            in
            Assert.is_false res;
            Assert.same objeq objeq')

let test_match_values_simple_objects =
    let mock_seen = IntIntSet.add (47, 11) IntIntSet.empty
    and objeq = IntIntMap.add (17,18) true IntIntMap.empty in
    Test.make_simple_test ~title:"Testing value matching: Same simple object"
        (fun () ->
            let (res, objeq') =
                match_values_raw memodata mock_seen objeq
                        (OObject 1, OObject 1)
            in
            Assert.is_true res;
            equal_objeq (IntIntMap.add (1,1) true objeq) objeq')

(* Test data for the more complicated objects. *)
let ext_versions =
    ReferenceMap.empty
        |> add_field 1 "data" 0
        |> add_field 1 "next" 0
        |> add_field 14 "data" 0
        |> add_field 14 "next" 0
        |> add_field 3 "data" 0
        |> add_field 3 "next" 0
        |> add_field 28 "data" 0
        |> add_field 28 "next" 0
        |> add_field 5 "data" 0
        |> add_field 5 "next" 0
        |> add_field 81 "stuff" 0
        |> add_field 81 "next" 0
        |> add_field 4 "next" 0
        |> add_field 37 "next" 0

let ext_points_to =
    VersionReferenceMap.empty
        |> add_data 1 "data" 0 (OString "a")
        |> add_data 1 "next" 0 (OObject 2)
        |> add_data 14 "data" 0 (OString "a")
        |> add_data 14 "next" 0 (OObject 20)
        |> add_data 3 "data" 0 (OString "a")
        |> add_data 3 "next" 0 (OObject 4)
        |> add_data 28 "data" 0 (OString "a")
        |> add_data 28 "next" 0 (OObject 37)
        |> add_data 5 "data" 0 (OString "b")
        |> add_data 5 "next" 0 (OObject 6)
        |> add_data 81 "stuff" 0 (OString "b")
        |> add_data 81 "next" 0 (OObject 97)
        |> add_data 4 "next" 0 (OObject 3)
        |> add_data 37 "next" 0 (OObject 28)

let ext_facts = { empty_local_facts with versions = ext_versions }

let ext_data = {
    funs1; funs2; facts1 = ext_facts; facts2 = ext_facts;
    pt1 = ext_points_to; pt2 = ext_points_to
}

type expectation = True | False | MaybeTrue | MaybeFalse
let ext_test_cases = [
    (1,14, true, [(1,14,True); (2,20,True)]);
    (1,28, false, [(2,37,MaybeFalse); (1,28,False)]);
    (1,81, false, [(2,97,MaybeTrue); (1,81,False)]);
    (3,28, true, [(3,28,True); (4,37,True)]);
    (3,81, false, [(4,97,MaybeFalse); (3,81,False)]);
    (5,14, false, [(6,20,MaybeTrue); (5,14,False)]);
    (5,81, false, [(6,97,MaybeTrue); (5,81,False)])
];;

let test_match_values_make (id1, id2, exp, objeq_exp) =
    Test.make_simple_test
        ~title:(Format.sprintf "%d vs. %d -- complex layout" id1 id2)
        (fun () ->
            let (res, objeq') = match_values_raw ext_data
                IntIntSet.empty IntIntMap.empty
                (OObject id1, OObject id2) in
            Assert.equal_bool exp res;
            List.iter (fun (j1, j2, exp) ->
                match exp with
                | MaybeTrue | MaybeFalse -> ()
                | True | False ->
                    Assert.is_true
                        ~msg:(Format.asprintf
                                "(%d, %d) is required to appear in %a"
                                j1 j2 pp_objeq objeq')
                    (IntIntMap.mem (j1, j2) objeq')) objeq_exp;
            let flag_to_bool = function
                | True -> true
                | False -> false
                | MaybeTrue -> true
                | MaybeFalse -> false in
            let (pos, neg) =
                objeq_exp |>
                List.map (fun (i1, i2, flag) -> ((i1, i2), flag_to_bool flag)) |>
                List.partition (fun (_, flag) -> flag) |>
                bmap (List.map fst) in
            IntIntMap.iter (fun key flag ->
                if flag then
                    Assert.is_true
                        ~msg:(Format.asprintf
                                "Expected positive list %a, but also got %a"
                                (pp_print_list pp_print_int_pair) pos
                                pp_print_int_pair key)
                        (List.mem key pos)
                else
                    Assert.is_true
                        ~msg:(Format.asprintf
                                "Expected negative list %a, but also got %a"
                                (pp_print_list pp_print_int_pair) neg
                                pp_print_int_pair key)
                        (List.mem key neg))
                objeq')
let tests_match_values_complex =
    List.map test_match_values_make ext_test_cases

(** Run all tests *)
let () = Test.run_tests
        ([test_funs_1; test_funs_2; test_funs_3; test_funs_4; test_funs_5;
        test_funs_6; test_funs_7; test_funs_8; test_funs_9] @
        tests_match_objects_raw_positive @ tests_match_objects_raw_negative @
        [test_match_objects_raw_targeted_objeq_update;
         test_match_objects_objmatch_fails;
         test_match_objects_objmatch_fails_on_ignored;
         test_match_objects_memo_unmemoized_success;
         test_match_objects_memo_unmemoized_failure;
         test_match_objects_memo_in_map_success;
         test_match_objects_memo_in_map_failure;
         test_match_objects_memo_cycle_cut;
         test_match_values_base_success;
         test_match_values_base_failure;
         test_match_values_base_nonbase;
         test_match_values_simple_objects
        ] @ tests_match_values_complex) 

