open Trace
open Richtrace
open Kaputt
open Abbreviations
open Reference
open LocalFacts

let (|>>) = (|>)
let (|>) = Pervasives.(|>)

let add_field name value =
    Misc.StringMap.add name { value; writable = true; enumerable = true; configurable = true; get = None; set = None }
let empty = Misc.StringMap.empty
let update ref old ver facts =
    { facts with versions = ReferenceMap.add ref ver facts.versions; last_update = Some (ref, old) }
let pt ref ver value ptdata = VersionReferenceMap.add (ref, ver) value ptdata

let trace_call = [
    Literal { iid =1; value = OFunction(1,0); hasGetterSetter = false };
    Declare { iid =2; name = "f"; value = OFunction(1,0); argument = None; isCatchParam = false };
    Literal { iid = 4; value = OFunction(2,1); hasGetterSetter = false };
    Write { iid =3; name ="f"; lhs = OFunction(1,0); value = OFunction(2,1); isGlobal = false; isScriptLocal = true };
    Read { iid =5; name ="f"; value = OFunction(2,1); isGlobal = false; isScriptLocal = true };
    Literal { iid =7; value = OObject 3; hasGetterSetter = false };
    FunPre { iid =6; f = OFunction(2,1); base = OObject 0; args = OObject 3; isConstructor = false; isMethod = false };
    FunEnter { iid =8; f = OFunction(2,1); this = OObject 0; args = OObject 3 };
    Declare { iid =9; name ="arguments"; value = OObject 4; argument = Some (-1); isCatchParam = false };
    Declare { iid =18; name ="f"; value = ONumberFloat 3.14; argument = None; isCatchParam = false };
    Write { iid =19; name ="f"; value = ONumberFloat 2.71; lhs = ONumberFloat 3.14; isGlobal = false; isScriptLocal = false };
    BinPre { iid =10; op ="+"; left = ONumberInt 1; right = ONumberInt 2; isOpAssign = false; isSwitchCaseComparison = false; isComputed = false };
    BinPost { iid =11; op ="+"; left = ONumberInt 1; right = ONumberInt 2; isOpAssign = false; isSwitchCaseComparison = false; isComputed = false; result = ONumberInt 3 };
    UnaryPre { iid =12; op ="-"; arg = ONumberInt 3 };
    UnaryPost { iid =13; op ="-"; arg = ONumberInt 3; result = ONumberInt (-3) };
    Return { iid =14; value = ONumberInt (-3) };
    FunExit { iid =15; ret = ONumberInt(-3); exc = OUndefined };
    FunPost { iid =16; f = OFunction(2,1); base = OObject 0; args = OObject 3; isConstructor = false; isMethod = false; result = ONumberInt(-3) };
    EndExpression 17;
    ]

let objects_call = [|
    empty;
    empty;
    empty;
    empty;
    empty
    |]
let functions_call = [|
    Local { from_toString = "f ver. 1"; from_jalangi = "f ver. 1" };
    Local { from_toString = "f ver. 2"; from_jalangi = "f ver. 2" };
    |]

let fref = reference_of_name false empty false "f"
let argref = reference_of_local_name "arguments"

let richtrace_fst_call = [
    RLiteral { value = OFunction(1,0); hasGetterSetter = false };
    RLocal { name ="f"; ref = (fref, 0) };
    RWrite { ref = (fref, 0); oldref = (fref, 0); value = OFunction(1,0); success = true };
    RLiteral { value = OFunction(2,1); hasGetterSetter = false };
    RWrite { ref = (fref, 1); oldref = (fref, 0); value = OFunction(2,1); success = true };
    RRead { ref = (fref,1); value = OFunction(2,1) };
    RLiteral { value = OObject 3; hasGetterSetter = false };
    RFunPre { f = OFunction(2,1); base = OObject 0; args = OObject 3; call_type = Function };
    RFunEnter { f = OFunction(2,1); this = OObject 0; args = OObject 3 };
    RLocal { name = "arguments"; ref = (argref, 0) };
    RWrite { ref = (argref, 0); oldref = (argref,0); value = OObject 4; success = true };
    RLocal { name ="f"; ref = (fref, 2) };
    RWrite { ref = (fref,2); oldref = (fref,2); value = ONumberFloat 3.14; success = true };
    RWrite { ref = (fref,3); oldref = (fref,2); value = ONumberFloat 2.71; success = true };
    RBinary { op ="+"; left = ONumberInt 1; right = ONumberInt 2; result = ONumberInt 3 };
    RUnary { op ="-"; arg = ONumberInt 3; result = ONumberInt(-3) };
    RReturn (ONumberInt (-3));
    RFunExit { ret = ONumberInt(-3); exc = OUndefined };
    RFunPost { f = OFunction(2,1); base = OObject 0; args = OObject 3; result = ONumberInt(-3) };
    REndExpression;
    ]

let facts_call_1 = empty_local_facts
let facts_call_2 = update fref 0 0 facts_call_1
let facts_call_3 = update fref 0 1 facts_call_2
let facts_call_4 = { facts_call_3 with last_arguments = Some 3 }
let facts_call_5 = { facts_call_4 }
let facts_call_6 = update argref 0 0 facts_call_5
let facts_call_7 = update fref 0 2 facts_call_6
let facts_call_8 = update fref 2 3 facts_call_7
let facts_call_9 = facts_call_4 |> update fref 2 1
let facts_call_10 = { facts_call_9 with last_arguments = None }
let richtrace_snd_call = [
    facts_call_1;
    facts_call_2;
    facts_call_2;
    facts_call_2;
    facts_call_3;
    facts_call_3;
    facts_call_3;
    facts_call_4;
    facts_call_5;
    facts_call_6;
    facts_call_6;
    facts_call_7;
    facts_call_7;
    facts_call_8;
    facts_call_8;
    facts_call_8;
    facts_call_8;
    facts_call_9;
    facts_call_10;
    facts_call_10
    ]
let richtrace_call = List.map2 (fun fst snd -> (fst, snd)) richtrace_fst_call richtrace_snd_call
let pt_call = VersionReferenceMap.empty
    |> pt fref 0 (OFunction(1, 0))
    |> pt fref 1 (OFunction(2,1))
    |> pt fref 2 (ONumberFloat 3.14)
    |> pt fref 3 (ONumberFloat 2.71)
    |> pt argref 0 (OObject 4)

let trace_obj = [
    Literal { iid =1; value = OObject 1; hasGetterSetter = false };
    PutFieldPre { iid =2; base = OObject 1; offset ="a"; value = OString "xyz"; isComputed = false; isOpAssign = false };
    PutField { iid =2; base = OObject 1; offset ="a"; value = OString "xyz"; isComputed = false; isOpAssign = false };
    PutFieldPre { iid =3; base = OObject 1; offset ="b"; value = OString "abc"; isComputed = false; isOpAssign = false };
    PutField { iid =3; base = OObject 1; offset ="b"; value = OString "abc"; isComputed = false; isOpAssign = false };
    PutFieldPre { iid =4; base = OObject 1; offset ="a"; value = OString "xyz!"; isComputed = false; isOpAssign = false };
    PutField { iid =4; base = OObject 1; offset ="a"; value = OString "xyz!"; isComputed = false; isOpAssign = false };
    ForIn { iid =5; value = OObject 1 };
    With { iid =6; value = OObject 1 };
    GetFieldPre { iid =7; base = OObject 1; offset ="a"; isComputed = false; isOpAssign = false; isMethodCall = false };
    GetField { iid =7; base = OObject 1; offset ="a"; isComputed = false; isOpAssign = false; isMethodCall = false; value = OString "xyz!" };
    ]

let objects_obj = [|
    empty;
    empty |> add_field "b" (OString "uvw")
    |]
let functions_obj = [| |]

let r1a = reference_of_fieldref (1, "a")
let r1b = reference_of_fieldref (1, "b")

let richtrace_fst_obj = [
    RLiteral { value = OObject 1; hasGetterSetter = false };
    RWrite { ref = (r1a, 0); oldref = (r1a, 0); value = OString "xyz"; success = true };
    RWrite { ref = (r1b, 1); oldref = (r1b, 0); value = OString "abc"; success = true };
    RWrite { ref = (r1a, 1); oldref = (r1a, 0); value = OString "xyz!"; success = true };
    RForIn (OObject 1);
    RWith (OObject 1);
    RRead { ref = (r1a, 1); value = OString "xyz!" }
    ]

let obj_facts_1 = update r1b 0 0 empty_local_facts
let obj_facts_2 = update r1a 0 0 obj_facts_1
let obj_facts_3 = update r1b 0 1 obj_facts_2
let obj_facts_4 = update r1a 0 1 obj_facts_3
let richtrace_snd_obj = [
    obj_facts_1;
    obj_facts_2;
    obj_facts_3;
    obj_facts_4;
    obj_facts_4;
    obj_facts_4;
    obj_facts_4
    ]
let richtrace_obj = List.map2 (fun fst snd -> (fst, snd)) richtrace_fst_obj richtrace_snd_obj
let pt_obj =
    VersionReferenceMap.empty
    |> pt r1b 0 (OString "uvw")
    |> pt r1a 0 (OString "xyz")
    |> pt r1b 1 (OString "abc")
    |> pt r1a 1 (OString "xyz!")

let trace_control = [
    ScriptEnter;
    ScriptEnter;
    Throw { iid = 1; value = OString "fail" };
    ScriptExc (OString "fail");
    Declare { iid =2; name ="e"; value = OString "fail"; argument = None; isCatchParam = true };
    Conditional { iid =3; value = OString "fail" };
    EndExpression 4;
    ScriptExit
    ]

let objects_control = [| empty |]
let functions_control = [| |]

let eref = reference_of_local_name "e"

let richtrace_fst_control = [
    RScriptEnter;
    RScriptEnter;
    RThrow (OString "fail");
    RScriptExc (OString "fail");
    RLocal { name ="e"; ref = (eref, 0) };
    RWrite { ref = (eref, 0); oldref = (eref, 0); value = OString "fail"; success = true };
    RConditional (OString "fail");
    REndExpression;
    RScriptExit
    ]
let control_1_facts = empty_local_facts
let control_2_facts = update eref 0 0 control_1_facts

let richtrace_snd_control = [
    control_1_facts;
    control_1_facts;
    control_1_facts;
    control_1_facts;
    control_2_facts;
    control_2_facts;
    control_2_facts;
    control_2_facts;
    control_2_facts
    ]
let richtrace_control = List.map2 (fun fst snd -> (fst, snd)) richtrace_fst_control richtrace_snd_control
let pt_control = VersionReferenceMap.empty |> pt eref 0 (OString "fail")

let globals = empty |> Misc.StringMap.add "window" (OObject 0)

let tf_call = (functions_call, objects_call, trace_call, globals, false)
let tf_obj = (functions_obj, objects_obj, trace_obj, globals, false)
let tf_control = (functions_control, objects_control, trace_control, globals, false)

let tracefiles = [ tf_call; tf_obj; tf_control ]

let rtf_call = {
    funcs = functions_call; objs = objects_call; trace = richtrace_call; globals;
    globals_are_properties = false; points_to = pt_call
}
let rtf_obj = {
    funcs = functions_obj; objs = objects_obj; trace = richtrace_obj; globals;
    globals_are_properties = false; points_to = pt_obj
}
let rtf_control = {
    funcs = functions_control; objs = objects_control; trace = richtrace_control; globals;
    globals_are_properties = false; points_to = pt_control
}

let rich_tracefiles = [ rtf_call; rtf_obj; rtf_control ]
let titles = [ "Call"; "Object"; "Control" ]

let rec map3 f l1 l2 l3 = match l1, l2, l3 with
    | (x1:: l1), (x2:: l2), (x3:: l3) -> f x1 x2 x3 :: map3 f l1 l2 l3
    | [], [], [] -> []
    | _, _, _ -> failwith "Lists of different length"

let compare_step
    (rt1, { last_arguments = la1; last_update = lu1; versions = ver1; aliases = al1 })
    (rt2, { last_arguments = la2; last_update = lu2; versions = ver2; aliases = al2 }) =
    rt1 = rt2 && la1 = la2 && lp1 = lp2 && lu1 = lu2 && ReferenceMap.equal (=) ver1 ver2 && Misc.StringMap.equal (=) al1 al2

let print_step (rt, lf) =
    Format.asprintf "@[<v 2>%a@ %a@]" pp_rich_operation rt pp_local_facts lf

let tests = map3 (fun title tf rtf_exp ->
                Test.make_simple_test ~title: title (fun () ->
                            let rtf_got = calculate_rich_tracefile tf in
                            Assert.equal ~msg:"functions" rtf_exp.funcs rtf_got.funcs;
                            Assert.equal ~msg:"objects" rtf_exp.objs rtf_got.objs;
                            Assert.equal ~msg:"globals" rtf_exp.globals rtf_got.globals;
                            Assert.equal ~msg:"globals_are_properties" rtf_exp.globals_are_properties rtf_got.globals_are_properties;
                            Assert.make_equal_list ~msg:"Trace" compare_step print_step rtf_exp.trace rtf_got.trace;
                            Assert.make_equal ~msg:"points_to" (VersionReferenceMap.equal (=)) (Misc.to_string PointsTo.pp_points_to_map) rtf_exp.points_to rtf_got.points_to))
        titles tracefiles rich_tracefiles

let () =
    Test.run_tests tests