open LocalFacts;;
open Kaputt;;
open Abbreviations;;
open Trace;;
open Cleantrace;;
open Reference
open Types

let print_trace tr =
    Trace.pp_trace Format.str_formatter tr;
    Format.flush_str_formatter()

let test_get_object1 =
    Test.make_simple_test ~title:"get_object: object"
        (fun () -> Assert.equal ~prn: string_of_int (get_object (OObject 1)) 1)
let test_get_object2 =
    Test.make_simple_test ~title:"get_object: function"
        (fun () -> Assert.equal ~prn: string_of_int (get_object (OFunction (1, 2))) 1)
let test_get_object3 =
    Test.make_simple_test ~title:"get_object: other"
        (fun () -> Assert.equal ~prn: string_of_int (get_object (OOther ("type", 1))) 1)
let test_get_object4 =
    Test.make_simple_test ~title:"get_object: non-object"
        (fun () -> Assert.raises (fun () -> get_object ONull))

let sample_raw_trace = [
    Return { iid = 1; value = ONull };
    Throw { iid = 2; value = OUndefined };
    ScriptExit
    ]
let sample_trace = [
    CReturn ONull;
    CThrow OUndefined ;
    CScriptExit
    ]
let sample_extended_trace = [
    (CReturn ONull, 5);
    (CThrow OUndefined, 17);
    (CScriptExit, 23)
    ]

let test_trace_initialize =
    Test.make_simple_test ~title:"trace_initialize"
        (fun () -> Assert.equal ~prn: (Misc.to_string pp_clean_trace)
                    (List.map fst (trace_initialize sample_raw_trace)) sample_trace)
let test_trace_fold =
    Test.make_simple_test ~title:"trace_fold"
        (fun () -> Assert.equal ~prn: string_of_int (trace_fold
                            (fun sum data -> function
                                    | CReturn _ -> sum + 2 * data
                                    | CThrow _ -> sum + 3 * data
                                    | _ -> sum + data) 42 sample_extended_trace)
                    126)
let test_trace_collect =
    Test.make_simple_test ~title:"trace_collect"
        (fun () ->
                let (new_trace, res) = trace_collect (fun sum data -> function
                                | CReturn _ -> ((sum, 2 * data), sum + 2 * data)
                                | CThrow _ -> ((sum, 3 * data), sum + 3 * data)
                                | _ -> ((sum, data), sum + data)) 42 sample_extended_trace in
                Assert.equal ~prn: string_of_int res 126;
                Assert.equal ~prn: (Misc.to_string pp_clean_trace) (List.map fst new_trace) sample_trace;
                Assert.equal (List.map snd new_trace) [(42, 10); (52, 51); (103, 23)])

let test_reference_of_variable_1 =
    Test.make_simple_test ~title:"reference to global, globals as vars"
        (fun () ->
                let ref = reference_of_variable false empty_local_facts true "x" in
                Assert.equal
                    ~prn: (Misc.to_string (FormatHelper.pp_print_option Format.pp_print_string))
                    (Some "x") (get_name ref);
                Assert.is_true ~msg:"Should be global" (is_global ref))
let test_reference_of_variable_2 =
    Test.make_simple_test ~title:"reference to global, globals as props"
        (fun () ->
                let ref = reference_of_variable true empty_local_facts true "x" in
                Assert.equal
                    ~prn: (Misc.to_string (FormatHelper.pp_print_option (FormatHelper.pp_print_pair Reference.pp_objectid Format.pp_print_string)))
                    (Some (Object 0, "x")) (get_fieldref ref))
let test_reference_of_variable_3 =
    Test.make_simple_test ~title:"reference to local, no alias"
        (fun () ->
                let ref = reference_of_variable false empty_local_facts false "x" in
                Assert.equal
                    ~prn: (Misc.to_string (FormatHelper.pp_print_option Format.pp_print_string))
                    (Some "x") (get_name ref);
                Assert.is_false ~msg:"Should be global" (is_global ref))
let test_reference_of_variable_4 =
    Test.make_simple_test ~title:"reference to local, no alias"
        (fun () ->
                let ref = reference_of_variable false { empty_local_facts with aliases = Misc.StringMap.add "x" (Object 0, "x") Misc.StringMap.empty } false "x" in
                Assert.equal
                    ~prn: (Misc.to_string (FormatHelper.pp_print_option (FormatHelper.pp_print_pair Reference.pp_objectid Format.pp_print_string)))
                    (Some (Object 0, "x")) (get_fieldref ref))

let test_make_versioned =
    Test.make_simple_test ~title: "make_versioned"
        (fun () ->
                let ref = reference_of_variable false empty_local_facts true "x" in
                let facts = { empty_local_facts with versions = ReferenceMap.add ref 17 ReferenceMap.empty } in
                Assert.equal ~prn: (Misc.to_string Reference.pp_versioned_reference)
                    (ref, 17) (make_versioned facts ref))

let () = Test.run_tests [test_get_object1; test_get_object2; test_get_object3;
        test_get_object4; test_trace_initialize; test_trace_fold; test_trace_collect;
        test_reference_of_variable_1; test_reference_of_variable_2;
        test_reference_of_variable_3; test_reference_of_variable_4;
        test_make_versioned
        ]
