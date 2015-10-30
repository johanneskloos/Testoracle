open Kaputt.Abbreviations
open Types
open Test_base_data

let (|>) = Pervasives.(|>)

let test_calculate_pointsTo =
    Test.make_simple_test ~title:"calculate_versions" (fun () ->
                let pointsto = CalculatePointsTo.calculate_pointsto (functab1, objtab1, facttrace1, globals, true) in
                let module VRFmt = FormatHelper.MapFormat(Reference.VersionReferenceMap) in
                Assert.make_equal (Reference.VersionReferenceMap.equal (=))
                    (Misc.to_string (VRFmt.pp_print_map_default Reference.pp_versioned_reference pp_jsval))
                    trace1_pointsto pointsto)
 
let _ = Test.run_tests [ test_calculate_pointsTo ]