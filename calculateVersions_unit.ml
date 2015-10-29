open Kaputt.Abbreviations
open Types
open Test_base_data

let (|>) = Pervasives.(|>)

let test_calculate_versions =
  let eqstep (op1, facts1) (op2, facts2) =
    let open LocalFacts in
    op1 = op2 &&
    (*facts1.last_update = facts2.last_update &&*)
    facts1.last_arguments = facts2.last_arguments &&
    Reference.ReferenceMap.equal (=) facts1.versions facts2.versions &&
    Misc.StringMap.equal (=) facts1.aliases facts2.aliases
  in
  Test.make_simple_test ~title:"calculate_versions" (fun () ->
      let (funcs, objs, vertrace, globals', gap) =
          CalculateVersions.calculate_versions (functab1, objtab1, argtrace1, globals, true) in
      let open FormatHelper in
      Assert.same functab1 funcs;
      Assert.same objtab1 objs;
      Assert.same globals globals';
      Assert.is_true gap;
      Assert.make_equal_list eqstep
        (Misc.to_string (pp_print_pair Cleantrace.pp_clean_operation LocalFacts.pp_local_facts))
        facttrace1 vertrace)
 
let _ = Test.run_tests [ test_calculate_versions ]