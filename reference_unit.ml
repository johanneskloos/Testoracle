open Kaputt
open Abbreviations
open Reference

let (|>>) = (|>)
let (|>) = Pervasives.(|>)

let test_reference_of_name_1 =
    Test.make_simple_test ~title:"reference to global, globals as vars"
        (fun () ->
                let ref = reference_of_name false Misc.StringMap.empty true "x" in
                Assert.equal
                    ~prn: (Misc.to_string (FormatHelper.pp_print_option Format.pp_print_string))
                    (Some "x") (get_name ref);
                Assert.is_true ~msg:"Should be global" (is_global ref))
let test_reference_of_name_2 =
    Test.make_simple_test ~title:"reference to global, globals as props"
        (fun () ->
                let ref = reference_of_name true Misc.StringMap.empty true "x" in
                Assert.equal
                    ~prn: (Misc.to_string (FormatHelper.pp_print_option (FormatHelper.pp_print_pair Reference.pp_objectid Format.pp_print_string)))
                    (Some (Object 0, "x")) (get_fieldref ref))
let test_reference_of_name_3 =
    Test.make_simple_test ~title:"reference to local, no alias"
        (fun () ->
                let ref = reference_of_name false Misc.StringMap.empty false "x" in
                Assert.equal
                    ~prn: (Misc.to_string (FormatHelper.pp_print_option Format.pp_print_string))
                    (Some "x") (get_name ref);
                Assert.is_false ~msg:"Should be global" (is_global ref))
let test_reference_of_name_4 =
    Test.make_simple_test ~title:"reference to local, no alias"
        (fun () ->
                let ref = reference_of_name false (Misc.StringMap.add "x" (Object 0, "x") Misc.StringMap.empty) false "x" in
                Assert.equal
                    ~prn: (Misc.to_string (FormatHelper.pp_print_option (FormatHelper.pp_print_pair Reference.pp_objectid Format.pp_print_string)))
                    (Some (Object 0, "x")) (get_fieldref ref))
let test_reference_of_fieldref =
    Test.make_simple_test ~title:"field reference"
        (fun () ->
                let ref = reference_of_fieldref (Object 0, "x") in
                Assert.equal
                    ~prn: (Misc.to_string (FormatHelper.pp_print_option (FormatHelper.pp_print_pair Reference.pp_objectid Format.pp_print_string)))
                    (Some (Object 0, "x")) (get_fieldref ref))
let test_reference_of_field =
    Test.make_simple_test ~title:"field reference"
        (fun () ->
                let ref = reference_of_field (Trace.OObject 0) "x" in
                Assert.equal
                    ~prn: (Misc.to_string (FormatHelper.pp_print_option (FormatHelper.pp_print_pair Reference.pp_objectid Format.pp_print_string)))
                    (Some (Object 0, "x")) (get_fieldref ref))
let test_reference_of_local_name =
    Test.make_simple_test ~title:"reference to definite local"
        (fun () ->
                let ref = reference_of_local_name "x" in
                Assert.equal
                    ~prn: (Misc.to_string (FormatHelper.pp_print_option Format.pp_print_string))
                    (Some "x") (get_name ref);
                Assert.is_false ~msg:"Should be global" (is_global ref))
let () = Test.run_tests
        [ test_reference_of_name_1; test_reference_of_name_2;
        test_reference_of_name_3; test_reference_of_name_4;
        test_reference_of_fieldref; test_reference_of_field;
        test_reference_of_local_name ]