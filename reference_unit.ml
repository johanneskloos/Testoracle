open Reference
open Types
open Kaputt.Abbreviations

let (|>) = Pervasives.(|>)
let alias_map = let open Misc.StringMap in
  empty |> add "y" (Object 1, "a")

let test1 =
  Test.make_simple_test ~title:"reference_of_name: global when globals are not properties"
  (fun () ->
    let ref = reference_of_name false alias_map true "x" in
    Assert.is_true
      ~msg:(Format.asprintf "Expected 'Global(\"x\")', but got '%a'" pp_reference ref)
      (match ref with GlobalVariable "x" -> true | _ -> false))
let test2 =
  Test.make_simple_test ~title:"reference_of_name: global when globals are properties"
  (fun () ->
    let ref = reference_of_name true alias_map true "x" in
    Assert.is_true
      ~msg:(Format.asprintf "Expected 'Field(Object 0, \"x\")', but got '%a'" pp_reference ref)
      (match ref with Field(Object 0, "x") -> true | _ -> false))
let test3 =
  Test.make_simple_test ~title:"reference_of_name: global when globals are properties; alias handling"
  (fun () ->
    let ref = reference_of_name true alias_map true "y" in
    Assert.is_true
      ~msg:(Format.asprintf "Expected 'Field(Object 0, \"y\")', but got '%a'" pp_reference ref)
      (match ref with Field(Object 0, "y") -> true | _ -> false))
let test4 =
  Test.make_simple_test ~title:"reference_of_name: local, not an alias"
  (fun () ->
    let ref = reference_of_name true alias_map false "x" in
    Assert.is_true
      ~msg:(Format.asprintf "Expected 'Local \"x\"', but got '%a'" pp_reference ref)
      (match ref with LocalVariable "x" -> true | _ -> false))
let test5 =
  Test.make_simple_test ~title:"reference_of_name: local name; alias handling"
  (fun () ->
    let ref = reference_of_name true alias_map false "y" in
    Assert.is_true
      ~msg:(Format.asprintf "Expected 'Field(Object 1, \"a\")', but got '%a'" pp_reference ref)
      (match ref with Field(Object 1, "a") -> true | _ -> false))

let test6 =
  Test.make_simple_test ~title:"reference_of_fieldref"
  (fun () ->
    let ref = reference_of_fieldref (Object 2, "b") in
    Assert.is_true
      ~msg:(Format.asprintf "Expected 'Field(Object 2, \"b\")', but got '%a'" pp_reference ref)
      (match ref with Field(Object 2, "b") -> true | _ -> false))

let test7 =
  Test.make_simple_test ~title:"reference_of_field"
  (fun () ->
    let ref = reference_of_field (OObject 2) "b" in
    Assert.is_true
      ~msg:(Format.asprintf "Expected 'Field(Object 2, \"b\")', but got '%a'" pp_reference ref)
      (match ref with Field(Object 2, "b") -> true | _ -> false))

let test8 =
  Test.make_simple_test ~title:"reference_of_local_name"
  (fun () ->
    let ref = reference_of_local_name "x" in
    Assert.is_true
      ~msg:(Format.asprintf "Expected 'Local \"x\"', but got '%a'" pp_reference ref)
      (match ref with LocalVariable "x" -> true | _ -> false))
     
let _ = Test.run_tests [test1; test2; test3; test4; test5; test6; test7; test8]