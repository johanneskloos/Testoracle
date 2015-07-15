open Misc
open LocalFacts
open Reference
open Trace
open Kaputt
open Abbreviations
open PointsTo

let (|>) = Pervasives.(|>)

let add_field obj fld ver vmap =
    ReferenceMap.add (reference_of_fieldref (obj, fld)) ver vmap
let add_data obj fld ver data pmap =
    VersionReferenceMap.add (reference_of_fieldref (obj, fld), ver) data pmap
let add_cell fld data cmap = StringMap.add fld data cmap

let versions =
    ReferenceMap.empty
    |> add_field 0 "a" 0
    |> add_field 0 "b" 1
    |> add_field 0 "c" 0
    |> add_field 1 "a" 2
    |> add_field 1 "b" 3
    |> add_field 1 "d" 4
    |> add_field 2 "e" 5
let local_facts = { empty_local_facts with versions }

let points_to_facts =
    VersionReferenceMap.empty
    |> add_data 0 "a" 0 ONull
    |> add_data 0 "b" 0 OUndefined
    |> add_data 0 "b" 1 (OBoolean true)
    |> add_data 0 "c" 0 (OString "abc")
    |> add_data 1 "a" 0 ONull
    |> add_data 1 "a" 2 (OString "xyz")
    |> add_data 1 "b" 3 OUndefined
    |> add_data 1 "d" 4 OUndefined
    |> add_data 2 "e" 5 OUndefined

let print_objfacts =
    to_string
        (StringMapFormat.pp_print_map_default Format.pp_print_string
                Trace.pp_objid)

let equal_objfacts =
    Assert.make_equal (StringMap.equal (=)) print_objfacts

let test_object_facts =
    Test.make_simple_test ~title:"Testing find_object_facts" (fun () ->
        find_object_facts 0 local_facts points_to_facts
        |> equal_objfacts 
                (StringMap.empty
                |> add_cell "a" ONull
                |> add_cell "b" (OBoolean true)
                |> add_cell "c" (OString "abc")))

let () = Test.run_tests [test_object_facts]
