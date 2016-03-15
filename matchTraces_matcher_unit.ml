open Trace
open MatchTraces
open MatchTypes
open TraceTypes
open Kaputt
open Abbreviations
open Reference
open LocalFacts
open Types

let (|>>) = (|>)
let (|>) = Pervasives.(|>)

let f1 = OFunction (1, 0)
let f1' = OFunction(2, 1)
let f1'' = OFunction(3, 2)
let b = OObject 2
let b' = OObject 4
let a = OObject 3
let a' = OObject 5
let v5 = ONumberInt 5
let vm5 = ONumberInt (-5)
let v1 = ONumberInt 1
let v2 = ONumberInt 2
let v3 = ONumberInt 3

let rfp_0 = RFunPre { f = f1; args = a; base = b; call_type = Method }
let rfp_1 = RFunPre { f = f1'; args = a'; base = b'; call_type = Method }
let rfp_2 = RFunPre { f = f1''; args = a'; base = b'; call_type = Method }
let rfe_0 = RFunEnter { f = f1; this = b; args = a }
let rfe_1 = RFunEnter { f = f1'; args = a'; this = b' }
let rfe_2 = RFunEnter { f = f1''; args = a'; this = b' }
let rfx = RFunExit { ret = v1; exc = OUndefined }
let rfo_0 = RFunPost { f = f1; args = a; base = b; result = v1; call_type = Method }
let rfo_1 = RFunPost { f = f1'; args = a'; base = b'; result = v1; call_type = Method }
let rfo_2 = RFunPost { f = f1''; args = a'; base = b'; result = v1; call_type = Method }
let run = RUnary { op = "-"; arg = v5; result = vm5 }
let ree2 = RConditional v2
let ree3 = RConditional v3
let rse = RScriptExit

let tu1 = [ rfp_0; rfe_0; rfx; rfo_0; run ]
let tm1 = [ rfp_1; rfe_1; rfx; rfo_1; ree2; run; ree3 ]
let tu2 = [ rfp_0; rfe_0; run; rfx; rfo_0; rse ]
let tm2 = [ rfp_2; rfe_2; rfp_1; rfe_1; run; rfx; rfo_1; rfx; rfo_2; rse ]

let enrich = 
  let empty_local_facts = { last_arguments = None; last_update = None; versions = ReferenceMap.empty; aliases = StringMap.empty; points_to = Reference.VersionReferenceMap.empty } in
  List.map (fun x -> (x, empty_local_facts))

module StringMap = StringMap

let body1 = "function f1 (args) { body 1 }"
let body2 = "function f2 (args) { body 2 }"
let fun_u = [|
  Local { from_toString = body1; from_jalangi = Some body1 }
|] |> ExtArray.of_array
let add_field name value fields =
  StringMap.add name
    { value; writable = true; get = None; set = None; enumerable = true;
      configurable = true } fields

let objs_u = [|
  StringMap.empty |> add_field "dist0l" OUndefined;
  StringMap.empty;
  StringMap.empty |> add_field "base" (OBoolean true);
  StringMap.empty |> add_field "0" (ONumberFloat 1.0)
|] |> ExtArray.of_array
let objs_m = [|
  StringMap.empty |> add_field "dist0r" OUndefined;
  StringMap.empty |> add_field "dist1r" OUndefined;
  StringMap.empty;
  StringMap.empty;
  StringMap.empty |> add_field "dist4r" OUndefined;
  StringMap.empty |> add_field "base" (OBoolean true);
  StringMap.empty |> add_field "0" (ONumberFloat 1.0)
|] |> ExtArray.of_array

let fun_m = [|
  External 17;
  Local { from_toString = body1; from_jalangi = Some body1 };
  Local { from_toString = body2; from_jalangi = Some body2 }
|] |> ExtArray.of_array
let rtu1 = {
  funcs = fun_u;
  objs = objs_u;
  trace = enrich tu1;
  points_to = VersionReferenceMap.empty;
  globals_are_properties = false;
  globals = StringMap.empty
}
let rtu2 = {
  funcs = fun_u;
  objs = objs_u;
  trace = enrich tu2;
  points_to = VersionReferenceMap.empty;
  globals_are_properties = false;
  globals = StringMap.empty
}
let rtm1 = {
  funcs = fun_m;
  objs = objs_m;
  trace = enrich tm1;
  points_to = VersionReferenceMap.empty;
  globals_are_properties = false;
  globals = StringMap.empty
}
let rtm2 = {
  funcs = fun_m;
  objs = objs_m;
  trace = enrich tm2;
  points_to = VersionReferenceMap.empty;
  globals_are_properties = false;
  globals = StringMap.empty
}

let match1 = [
  Pair(rfp_0, rfp_1);
  Pair(rfe_0, rfe_1);
  Pair(rfx, rfx);
  Pair(rfo_0, rfo_1);
  Init(ree2);
  Pair(run, run);
  Init(ree3)
]
let match2 = [
  Pair(rfp_0, rfp_2);
  Wrap(rfe_2);
  Wrap(rfp_1);
  Pair(rfe_0, rfe_1);
  Pair(run, run);
  Pair(rfx, rfx);
  Wrap(rfo_1);
  Wrap(rfx);
  Pair(rfo_0, rfo_2);
  Pair(rse, rse)
]

let pp_match pp = function
  | Pair(op1, op2) -> Format.fprintf pp "%a -> %a" pp_rich_operation op1 pp_rich_operation op2
  | Wrap op -> Format.fprintf pp "wrap %a" pp_rich_operation op
  | Init op -> Format.fprintf pp "init %a" pp_rich_operation op
let match_print_trace tr =
  Misc.to_string (fun pp -> Format.fprintf pp "@[<v 4>%a@]" (Fmt.list pp_match)) tr
let match_print tr1 tr2 = function
  | Some tr -> match_print_trace tr
  | None -> "failed"

let match_equal tr1 tr2 exp = Assert.make_equal (=) (match_print tr1 tr2) exp (match_traces tr1 tr2)

let test_11 =
  Test.make_simple_test ~title:"Comparing traces unmod. 1 and mod. 1"
    (fun () ->
       match_equal rtu1 rtm1 (Some match1))
let test_22 =
  Test.make_simple_test ~title:"Comparing traces unmod. 2 and mod. 2"
    (fun () ->
       match_equal rtu2 rtm2 (Some match2))
let test_12 =
  Test.make_simple_test ~title:"Comparing traces unmod. 1 and mod. 2"
    (fun () ->
       Assert.equal_bool false (match match_traces rtu1 rtm2 with Some _ -> true | None -> false))

let tests = [test_11; test_22; test_12]
