open Cleantrace
open Kaputt.Abbreviations
open Types
open Trace
open Test_base_data

let (|>) = Pervasives.(|>)

(* First test, basically a smoke test: Transform the generic trace. *)
let test1 =
    Test.make_simple_test ~title:"Basic trace cleanup" (fun () ->
                let (funs, objs, cleantrace, globals', gap) = clean_tracefile tracefile1 in
                Assert.make_equal (=) (Misc.to_string pp_functions) functab1 funs;
                Assert.make_equal (=) (Misc.to_string pp_objects) objtab1 objs;
                Assert.make_equal (=) (Misc.to_string pp_globals) globals globals';
                Assert.equal_bool true gap;
                Assert.make_equal (=) (Misc.to_string pp_clean_trace) [
                    CForIn obj1_simp2;
                    CWith obj1_simp2;
                    CScriptEnter;
                    CThrow obj1_simp2;
                    CScriptExc obj1_simp2;
                    CDeclare { name = "e"; value = obj1_simp2; declaration_type = CatchParam };
                    CEndExpression;
                    CLiteral { value = vtrue; hasGetterSetter = false };
                    CWrite { name = "x"; lhs = vundef; value = vtrue; isGlobal = true; isSuccessful = true };
                    CRead { name = "x"; value = vtrue; isGlobal = true };
                    CFunPre { f = obj1_fun1; base = obj1_cyc1; args = obj1_simp1; call_type = Method };
                    CFunEnter { f = obj1_fun1; this = obj1_cyc1; args = obj1_simp1 };
                    CDeclare { name = "arguments"; value = obj1_simp1; declaration_type = ArgumentArray };
                    CDeclare { name = "x"; value = vundef; declaration_type = ArgumentBinding 0 };
                    CReturn vfalse;
                    CFunExit { ret = vfalse; exc = vundef };
                    CFunPost { f = obj1_fun1; args = obj1_simp1; call_type = Method; result = vfalse; base = obj1_cyc1 };
                    CScriptEnter;
                    CBinary { op = "+"; left = v0; right = v1; result = v1 };
                    CUnary { op = "-"; arg = v0; result = v0 };
                    CScriptExit;
                    CGetField { base = obj1_simp1; offset = "marker"; value = vundef };
                    CPutField { base = obj1_simp1; offset = "marker"; value = vundef };
                    CLiteral { value = obj1_simp2; hasGetterSetter = false };
                    CDeclare { name = "y"; value = obj1_simp2; declaration_type = Var };
                    CConditional vfalse
                    ] cleantrace)

(* Second test: Check that global calcuation works correctly. *)
let test2 =
  Test.make_simple_test ~title:"Global calculation" (fun () ->
    let trace = [
      Read { name = "r1"; value = vundef; isGlobal = true; isScriptLocal = true; iid = 0 }; 
      Read { name = "r2"; value = vundef; isGlobal = true; isScriptLocal = true; iid = 0 }; 
      Write { name = "w1"; value = vundef; lhs = vundef; isGlobal = true; isScriptLocal = true; iid = 0 }; 
      Write { name = "w2"; value = vundef; lhs = vundef; isGlobal = true; isScriptLocal = true; iid = 0 }; 
      Declare { name = "r2"; value = vundef; argument = None; isCatchParam = false; iid = 0 };
      Read { name = "r1"; value = vundef; isGlobal = true; isScriptLocal = true; iid = 0 }; 
      Read { name = "r2"; value = vundef; isGlobal = true; isScriptLocal = true; iid = 0 }; 
      Write { name = "w1"; value = vundef; lhs = vundef; isGlobal = true; isScriptLocal = true; iid = 0 }; 
      Write { name = "w2"; value = vundef; lhs = vundef; isGlobal = true; isScriptLocal = true; iid = 0 }; 
      FunPre { f = obj1_fun1; base = vundef; args = vundef; isConstructor = false; isMethod = false; iid = 0 };
      FunEnter { f = vundef; this = vundef; args = vundef;  iid = 0 };
      Read { name = "r1"; value = vundef; isGlobal = true; isScriptLocal = true; iid = 0 }; 
      Read { name = "r2"; value = vundef; isGlobal = true; isScriptLocal = true; iid = 0 }; 
      Write { name = "w1"; value = vundef; lhs = vundef; isGlobal = true; isScriptLocal = true; iid = 0 }; 
      Write { name = "w2"; value = vundef; lhs = vundef; isGlobal = true; isScriptLocal = true; iid = 0 }; 
      Declare { name = "w1"; value = vundef; argument = None; isCatchParam = false; iid = 0 };
      Read { name = "r1"; value = vundef; isGlobal = true; isScriptLocal = true; iid = 0 }; 
      Read { name = "r2"; value = vundef; isGlobal = true; isScriptLocal = true; iid = 0 }; 
      Write { name = "w1"; value = vundef; lhs = vundef; isGlobal = true; isScriptLocal = true; iid = 0 }; 
      Write { name = "w2"; value = vundef; lhs = vundef; isGlobal = true; isScriptLocal = true; iid = 0 }; 
      FunExit { ret = vundef; exc = vundef ; iid = 0 };
      FunPost { f = obj1_fun1; base = vundef; args = vundef; result = vundef; isConstructor = false; isMethod = false; iid = 0 };
      Read { name = "r1"; value = vundef; isGlobal = true; isScriptLocal = true; iid = 0 }; 
      Read { name = "r2"; value = vundef; isGlobal = true; isScriptLocal = true; iid = 0 }; 
      Write { name = "w1"; value = vundef; lhs = vundef; isGlobal = true; isScriptLocal = true; iid = 0 }; 
      Write { name = "w2"; value = vundef; lhs = vundef; isGlobal = true; isScriptLocal = true; iid = 0 } 
    ] in
    let (_, _, cleantrace, _, _) = clean_tracefile (functab1, objtab1, trace, globals, true) in
    Assert.make_equal (=) (Misc.to_string pp_clean_trace) [
      CRead { name = "r1"; value = vundef; isGlobal = true }; 
      CRead { name = "r2"; value = vundef; isGlobal = true }; 
      CWrite { name = "w1"; value = vundef; lhs = vundef; isGlobal = true; isSuccessful = true }; 
      CWrite { name = "w2"; value = vundef; lhs = vundef; isGlobal = true; isSuccessful = true }; 
      CDeclare { name = "r2"; value = vundef; declaration_type = Var };
      CRead { name = "r1"; value = vundef; isGlobal = true }; 
      CRead { name = "r2"; value = vundef; isGlobal = false }; 
      CWrite { name = "w1"; value = vundef; lhs = vundef; isGlobal = true; isSuccessful = true }; 
      CWrite { name = "w2"; value = vundef; lhs = vundef; isGlobal = true; isSuccessful = true }; 
      CFunPre { f = obj1_fun1; base = vundef; args = vundef; call_type = Function };
      CFunEnter { f = vundef; this = vundef; args = vundef };
      CRead { name = "r1"; value = vundef; isGlobal = true }; 
      CRead { name = "r2"; value = vundef; isGlobal = false }; 
      CWrite { name = "w1"; value = vundef; lhs = vundef; isGlobal = true; isSuccessful = true }; 
      CWrite { name = "w2"; value = vundef; lhs = vundef; isGlobal = true; isSuccessful = true }; 
      CDeclare { name = "w1"; value = vundef; declaration_type = Var };
      CRead { name = "r1"; value = vundef; isGlobal = true }; 
      CRead { name = "r2"; value = vundef; isGlobal = false }; 
      CWrite { name = "w1"; value = vundef; lhs = vundef; isGlobal = false; isSuccessful = true }; 
      CWrite { name = "w2"; value = vundef; lhs = vundef; isGlobal = true; isSuccessful = true }; 
      CFunExit { ret = vundef; exc = vundef };
      CFunPost { f = obj1_fun1; base = vundef; args = vundef; result = vundef; call_type = Function };
      CRead { name = "r1"; value = vundef; isGlobal = true }; 
      CRead { name = "r2"; value = vundef; isGlobal = false }; 
      CWrite { name = "w1"; value = vundef; lhs = vundef; isGlobal = true; isSuccessful = true }; 
      CWrite { name = "w2"; value = vundef; lhs = vundef; isGlobal = true; isSuccessful = true } 
    ] cleantrace
    )

(* Third test: Correct translation of declaration and function types *)
let test3 =
  Test.make_simple_test ~title:"Declaration and function types" (fun () ->
    let trace = [
      FunPre { f = obj1_fun1; base = vundef; args = vundef; isMethod = false; isConstructor = false; iid = 0 }; 
      FunEnter { f = obj1_fun1; this = vundef; args = vundef; iid = 0 }; 
      FunPre { f = obj1_fun1; base = vundef; args = vundef; isMethod = true; isConstructor = false; iid = 0 }; 
      FunEnter { f = obj1_fun1; this = vundef; args = vundef; iid = 0 }; 
      FunPre { f = obj1_fun1; base = vundef; args = vundef; isMethod = false; isConstructor = true; iid = 0 }; 
      FunEnter { f = obj1_fun1; this = vundef; args = vundef; iid = 0 }; 
      FunPre { f = obj1_fun1; base = vundef; args = vundef; isMethod = true; isConstructor = true; iid = 0 }; 
      FunEnter { f = obj1_fun1; this = vundef; args = vundef; iid = 0 };
      FunExit { ret = vundef; exc = vundef; iid = 0 };
      FunPost { f = obj1_fun1; base = vundef; args = vundef; isMethod = true; isConstructor = true; iid = 0; result = vundef }; 
      FunExit { ret = vundef; exc = vundef; iid = 0 };
      FunPost { f = obj1_fun1; base = vundef; args = vundef; isMethod = false; isConstructor = true; iid = 0; result = vundef }; 
      FunExit { ret = vundef; exc = vundef; iid = 0 };
      FunPost { f = obj1_fun1; base = vundef; args = vundef; isMethod = true; isConstructor = false; iid = 0; result = vundef }; 
      FunExit { ret = vundef; exc = vundef; iid = 0 };
      FunPost { f = obj1_fun1; base = vundef; args = vundef; isMethod = false; isConstructor = false; iid = 0; result = vundef }; 
      Declare { name = "x"; value = vundef; argument = None; isCatchParam = false; iid = 0 };
      Declare { name = "x"; value = vundef; argument = None; isCatchParam = true; iid = 0 };
      Declare { name = "x"; value = vundef; argument = Some 0; isCatchParam = false; iid = 0 };
      Declare { name = "x"; value = vundef; argument = Some 1; isCatchParam = false; iid = 0 };
      Declare { name = "x"; value = vundef; argument = Some (-1); isCatchParam = false; iid = 0 };
      ]in
    let (_, _, cleantrace, _, _) = clean_tracefile (functab1, objtab1, trace, globals, true) in
    Assert.make_equal (=) (Misc.to_string pp_clean_trace) [
      CFunPre { f = obj1_fun1; base = vundef; args = vundef; call_type = Function }; 
      CFunEnter { f = obj1_fun1; this = vundef; args = vundef }; 
      CFunPre { f = obj1_fun1; base = vundef; args = vundef; call_type = Method }; 
      CFunEnter { f = obj1_fun1; this = vundef; args = vundef }; 
      CFunPre { f = obj1_fun1; base = vundef; args = vundef; call_type = Constructor }; 
      CFunEnter { f = obj1_fun1; this = vundef; args = vundef }; 
      CFunPre { f = obj1_fun1; base = vundef; args = vundef; call_type = ConstructorMethod }; 
      CFunEnter { f = obj1_fun1; this = vundef; args = vundef };
      CFunExit { ret = vundef; exc = vundef };
      CFunPost { f = obj1_fun1; base = vundef; args = vundef; call_type = ConstructorMethod; result = vundef }; 
      CFunExit { ret = vundef; exc = vundef };
      CFunPost { f = obj1_fun1; base = vundef; args = vundef; call_type = Constructor; result = vundef }; 
      CFunExit { ret = vundef; exc = vundef };
      CFunPost { f = obj1_fun1; base = vundef; args = vundef; call_type = Method; result = vundef }; 
      CFunExit { ret = vundef; exc = vundef };
      CFunPost { f = obj1_fun1; base = vundef; args = vundef; call_type = Function; result = vundef }; 
      CDeclare { name = "x"; value = vundef; declaration_type = Var };
      CDeclare { name = "x"; value = vundef; declaration_type = CatchParam };
      CDeclare { name = "x"; value = vundef; declaration_type = ArgumentBinding 0 };
      CDeclare { name = "x"; value = vundef; declaration_type = ArgumentBinding 1 };
      CDeclare { name = "x"; value = vundef; declaration_type = ArgumentArray };
      ] cleantrace
    )

(* Fourth test: Correct external event synthesis *)
let test4 =
  Test.make_simple_test ~title: "External event synthesis" (fun () ->
    let trace = [
      FunPre { f = obj1_fun4; base = v0; args = v1; isMethod = false; isConstructor = false; iid = 0 };
      FunPost  { f = obj1_fun4; base = v0; args = v1; result = vtrue; isMethod = false; isConstructor = false; iid = 0 };
      FunPre { f = obj1_fun4; base = v0; args = v1; isMethod = false; isConstructor = false; iid = 0 };
      FunEnter { f = obj1_fun2; this = v0; args = v1; iid = 0 };
      FunExit { ret = vnull; exc = vundef; iid = 0 };
      FunPost  { f = obj1_fun4; base = v0; args = v1; result = vtrue; isMethod = false; isConstructor = false; iid = 0 };
      ] in
    let (_, _, cleantrace, _, _) = clean_tracefile (functab1, objtab1, trace, globals, true) in
    Assert.make_equal (=) (Misc.to_string pp_clean_trace) [
      CFunPre { f = obj1_fun4; base = v0; args = v1; call_type = Function };
      CFunEnter { f = obj1_fun4; this = v0; args = v1 };
      CFunExit { ret = vtrue; exc = vundef };
      CFunPost  { f = obj1_fun4; base = v0; args = v1; result = vtrue; call_type = Function };
      CFunPre { f = obj1_fun4; base = v0; args = v1; call_type = Function };
      CFunEnter { f = obj1_fun4; this = v0; args = v1 };
      CFunPre { f = obj1_fun2; base = v0; args = v1; call_type = Method };
      CFunEnter { f = obj1_fun2; this = v0; args = v1 };
      CFunExit { ret = vnull; exc = vundef };
      CFunPost  { f = obj1_fun2; base = v0; args = v1; result = vundef; call_type = Method };
      CFunExit { ret = vtrue; exc = vundef };
      CFunPost  { f = obj1_fun4; base = v0; args = v1; result = vtrue; call_type = Function };
      ] cleantrace)
      
let _ =
  Test.run_tests [ test1; test2; test3; test4 ]