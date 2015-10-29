open PointsTo
open Kaputt.Abbreviations
open Types
open Test_base_data

let (|>) = Pervasives.(|>)

let test_find_object_facts =
  Test.make_simple_test ~title:"find_object_facts" (fun () ->
    let facts = find_object_facts (objectid_of_jsval obj1_simp1) (List.hd (List.rev cleantrace1_facts)) trace1_pointsto in
    Assert.make_equal (Misc.StringMap.equal (=))
      (Misc.to_string (Misc.StringMapFormat.pp_print_map_default Format.pp_print_string pp_jsval))
      (Misc.StringMap.empty
      |> Misc.StringMap.add "0" vnull
      |> Misc.StringMap.add "1" vundef
      |> Misc.StringMap.add "marker" vundef
      |> Misc.StringMap.add "toString" (OFunction (0,2)))
      facts)

let _ = Test.run_tests [ test_find_object_facts ]
