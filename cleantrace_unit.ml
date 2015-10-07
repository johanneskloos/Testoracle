open Kaputt
open Cleantrace
open Trace
open Abbreviations

let (|>) = Pervasives.(|>)

let func1 = OFunction(23, 42)
let obj1 = OObject 5
let obj2 = OObject 17
let obj3 = OBoolean true

let trace1 =
    [
    FunPre { iid = 1; f = func1; base = obj1; args = obj2; isMethod = false; isConstructor = false };
    FunPost { iid = 1; f = func1; base = obj1; args = obj2; result = obj3; isMethod = false; isConstructor = false };
    Literal { iid = 2; value = obj3; hasGetterSetter = false };
    ForIn { iid = 4; value = obj2 };
    Declare { iid = 5; name = "x"; value = obj2; argument = None; isCatchParam = false };
    GetFieldPre { iid = 9; base = obj1; offset = "x"; isComputed = false; isOpAssign = false; isMethodCall = false };
    GetField { iid = 9; base = obj1; offset = "x"; isComputed = false; isOpAssign = false; isMethodCall = false; value = obj3 };
    PutFieldPre { iid = 10; base = obj1; offset = "x"; value = obj2; isComputed = false; isOpAssign = false };
    PutField { iid = 10; base = obj1; offset = "x"; value = obj2; isComputed = false; isOpAssign = false };
    Read { iid = 11; name = "x"; value = obj1; isGlobal = true; isScriptLocal = false };
    Write { iid = 12; name = "x"; value = obj1; lhs = obj2; isGlobal = true; isScriptLocal = false };
    Return { iid = 13; value = obj2 };
    Throw { iid = 13; value = obj2 };
    With { iid = 13; value = obj2 };
    FunEnter { iid = 14; f = func1; this = obj1; args = obj2 };
    FunExit { iid = 15; ret = obj1; exc = obj2 };
    ScriptEnter;
    ScriptExit;
    ScriptExc obj2;
    BinPre { iid = 17; op = "+"; left = obj1; right = obj2; isOpAssign = false; isSwitchCaseComparison = false; isComputed = false };
    BinPost { iid = 17; op = "+"; left = obj1; right = obj2; isOpAssign = false; isSwitchCaseComparison = false; isComputed = false; result = obj3 };
    UnaryPre { iid = 18; op = "?"; arg = obj2 };
    UnaryPost { iid = 19; op = "?"; arg = obj2; result = obj3 };
    EndExpression 20;
    Conditional { iid = 21; value = obj3 }
    ]
let exp1 =
    [
    CFunPre { f = func1; base = obj1; args = obj2; call_type = Function };
    CFunPost { f = func1; base = obj1; args = obj2; result = obj3; call_type = Function };
    CLiteral { value = obj3; hasGetterSetter = false };
    CForIn obj2;
    CDeclare { name = "x"; value = obj2; declaration_type = Var };
    CGetField { base = obj1; offset = "x"; value = obj3 };
    CPutField { base = obj1; offset = "x"; value = obj2 };
    CRead { name = "x"; value = obj1; isGlobal = false };
    CWrite { name = "x"; value = obj1; lhs = obj2; isGlobal = false; isSuccessful = true };
    CReturn obj2;
    CThrow obj2;
    CWith obj2;
    CFunEnter { f = func1; this = obj1; args = obj2 };
    CFunExit { ret = obj1; exc = obj2 };
    CScriptEnter;
    CScriptExit;
    CScriptExc obj2;
    CBinary { op = "+"; left = obj1; right = obj2; result = obj3 };
    CUnary { op = "?"; arg = obj2; result = obj3 };
    CEndExpression;
    CConditional obj3
    ]
    (*
let test1 =
    Test.make_simple_test ~title:"Basic translation from trace to clean trace"
        (fun () ->
                Assert.equal ~prn: (Misc.to_string pp_clean_trace) exp1 (clean_trace trace1))
*)
let trace2 = [
    Read { iid = 1; name = "x"; value = obj1; isGlobal = true; isScriptLocal = false };
    Read { iid = 2; name = "x"; value = obj1; isGlobal = false; isScriptLocal = false };
    Write { iid = 2; name = "x"; value = obj1; lhs = obj2; isGlobal = false; isScriptLocal = false };
    FunPre { iid = 1; f = func1; base = obj1; args = obj2; isMethod = false; isConstructor = false };
    FunEnter { iid = 14; f = func1; this = obj1; args = obj2 };
    Read { iid = 1; name = "x"; value = obj1; isGlobal = true; isScriptLocal = false };
    Write { iid = 2; name = "x"; value = obj1; lhs = obj2; isGlobal = false; isScriptLocal = false };
    Declare { iid = 5; name = "x"; value = obj2; argument = None; isCatchParam = false };
    Read { iid = 1; name = "x"; value = obj1; isGlobal = true; isScriptLocal = false };
    Write { iid = 2; name = "x"; value = obj1; lhs = obj2; isGlobal = false; isScriptLocal = false };
    FunExit { iid = 15; ret = obj1; exc = obj2 };
    FunPost { iid = 1; f = func1; base = obj1; args = obj2; result = obj3; isMethod = false; isConstructor = false };
    Read { iid = 2; name = "x"; value = obj1; isGlobal = false; isScriptLocal = false };
    Write { iid = 2; name = "x"; value = obj1; lhs = obj2; isGlobal = false; isScriptLocal = false };
    ]

let exp2 = [
    CRead { name = "x"; value = obj1; isGlobal = true };
    CRead { name = "x"; value = obj1; isGlobal = true };
    CWrite { name = "x"; value = obj1; lhs = obj2; isGlobal = true; isSuccessful = true };
    CFunPre { f = func1; base = obj1; args = obj2; call_type = Function };
    CFunEnter { f = func1; this = obj1; args = obj2 };
    CRead { name = "x"; value = obj1; isGlobal = true };
    CWrite { name = "x"; value = obj1; lhs = obj2; isGlobal = true; isSuccessful = true };
    CDeclare { name = "x"; value = obj2; declaration_type = Var };
    CRead { name = "x"; value = obj1; isGlobal = false };
    CWrite { name = "x"; value = obj1; lhs = obj2; isGlobal = false; isSuccessful = true };
    CFunExit { ret = obj1; exc = obj2 };
    CFunPost { f = func1; base = obj1; args = obj2; result = obj3; call_type = Function };
    CRead { name = "x"; value = obj1; isGlobal = true };
    CWrite { name = "x"; value = obj1; lhs = obj2; isGlobal = true; isSuccessful = true }
    ]
let test2 =
    Test.make_simple_test ~title:"Global tracking"
        (fun () ->
                Assert.equal ~prn: (Misc.to_string pp_clean_trace) exp2 (clean_trace trace2))

let trace3 = [
    FunPre { iid =1; f = func1; base = obj1; args = obj2; isMethod = false; isConstructor = false };
    FunPre { iid =1; f = func1; base = obj1; args = obj2; isMethod = false; isConstructor = true };
    FunPre { iid =1; f = func1; base = obj1; args = obj2; isMethod = true; isConstructor = false };
    FunPre { iid =1; f = func1; base = obj1; args = obj2; isMethod = true; isConstructor = true }
    ]
let exp3 = [
    CFunPre { f = func1; base = obj1; args = obj2; call_type = Function };
    CFunPre { f = func1; base = obj1; args = obj2; call_type = Constructor };
    CFunPre { f = func1; base = obj1; args = obj2; call_type = Method };
    CFunPre { f = func1; base = obj1; args = obj2; call_type = ConstructorMethod }
    ]
let test3 =
    Test.make_simple_test ~title:"Call type translation"
        (fun () ->
                Assert.equal ~prn: (Misc.to_string pp_clean_trace) exp3 (clean_trace trace3))

let trace4 = [
    Declare { iid = 1; name = "x"; value = obj1; argument = None; isCatchParam = false };
    Declare { iid = 2; name = "x"; value = obj1; argument = Some (-1); isCatchParam = false };
    Declare { iid = 3; name = "x"; value = obj1; argument = Some 0; isCatchParam = false };
    Declare { iid = 4; name = "x"; value = obj1; argument = None; isCatchParam = true };
    ]
let exp4 = [
    CDeclare { name = "x"; value = obj1; declaration_type = Var };
    CDeclare { name = "x"; value = obj1; declaration_type = ArgumentArray };
    CDeclare { name = "x"; value = obj1; declaration_type = ArgumentBinding 0 };
    CDeclare { name = "x"; value = obj1; declaration_type = CatchParam };
    ]
let test4 =
    Test.make_simple_test ~title:"Binding type translation"
        (fun () ->
                Assert.equal ~prn: (Misc.to_string pp_clean_trace) exp4 (clean_trace trace4))

let () = Test.run_tests [(*test1; *)test2; test3; test4]