open Types
open MatchOperations
open Richtrace
open Kaputt
open Abbreviations
open Reference
open LocalFacts
open Cleantrace
open MatchTypes

module StringMap = Misc.StringMap

let (|>>) = (|>)
let (|>) = Pervasives.(|>)

let funcs = [|
  Local { from_toString = "ar3wraw3eraw3r"; from_jalangi = Some "function () { stuff }" };
  External 1;
  Local { from_toString = "hsrthytyhjdrr"; from_jalangi = Some "function () { f() }" }
|]
let add_field name value =
  StringMap.add name { value; writable = true; get = None; set = None; enumerable = true; configurable = true }
let objs = [|
  (* Default handling for functions *)
  StringMap.empty |> add_field "toString" (OFunction (9999, 0));
  StringMap.empty |> add_field "toString" (OFunction (9999, 0));
  StringMap.empty |> add_field "toString" (OFunction (9999, 0));
  (* base1 *)
  StringMap.empty;
  (* args1 *)
  StringMap.empty |> add_field "0" (ONumberInt 17) |> add_field "1" (ONumberInt 23);
  (* obj1 *)
  StringMap.empty |> add_field "x" ONull
|]
let func1 = OFunction(0, 0)
let func2 = OFunction(1, 1)
let func3 = OFunction(2, 2)
let base1 = OObject 3
let args1 = OObject 4
let args2 = OObject 5
let obj1 = OObject 5
let val1 = ONumberInt 42
let val2 = ONumberFloat 3.14
let val3 = OBoolean true
let val4 = OString "xyz"
let obj2 = OObject 4
let ref1 = (reference_of_fieldref (Object 4, "0"), 1)
let ref2 = (reference_of_fieldref (Object 4, "0"), 0)
let ref3 = (reference_of_fieldref (Object 5, "x"), 5)
let ref4 = (reference_of_fieldref (Object 5, "x"), 8)
let ref5 = (reference_of_fieldref (Object 0, "toString"), 0)
let ref6 = (reference_of_fieldref (Object 0, "toString"), 1)
let add_pt = VersionReferenceMap.add
let gref1 = (Reference.reference_of_name false StringMap.empty true "a", 0)
let gref2 = (Reference.reference_of_name false StringMap.empty true "b", 0)
let points_to =
  VersionReferenceMap.empty
  |> add_pt ref1 (ONumberInt 17)
  |> add_pt ref2 (ONumberInt 18)
  |> add_pt ref3 (OBoolean true)
  |> add_pt ref4 (OBoolean true)
  |> add_pt ref5 (OFunction (9999, 0))
  |> add_pt ref6 (OFunction (666, 1))
  |> add_pt gref1 OUndefined
  |> add_pt gref2 ONull

let dummy_rt = {
  funcs;
  objs;
  trace = [];
  globals = StringMap.empty |> StringMap.add "a" (OObject 0);
  globals_are_properties = false;
  points_to
}
let state1_facts = {
  last_arguments = Some 42;
  last_update = Some ref2;
  versions = ReferenceMap.empty
             |> ReferenceMap.add (fst ref1) 0
             |> ReferenceMap.add (fst ref3) 5
             |> ReferenceMap.add (fst ref5) 0
             |> ReferenceMap.add (fst gref1) 0
             |> ReferenceMap.add (fst gref2) 0;
  aliases = StringMap.empty
}

let add_init = VersionReferenceSet.add
let state1 = {
  rt1 = dummy_rt;
  rt2 = dummy_rt;
  objeq = ref Misc.IntIntMap.empty;
  initialisation_data =
    VersionReferenceSet.empty
    |> add_init ref3;
  toString_data = [];
  known_blocked = Misc.IntIntMap.empty;
  nonequivalent_functions = Misc.IntIntSet.empty
}

(** Some more small functions. *)
let trace_init =
  List.map (fun op -> (op, state1_facts))
    [ RWrite { ref = ref6; oldref = ref5;
               value = val1; success = true };
      RRead { ref = ref1; value = val1 };
      RForIn val1;
      RScriptExit
    ]

