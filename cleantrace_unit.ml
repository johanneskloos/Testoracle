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
                Assert.make_equal (=) (Misc.to_string pp_clean_trace) cleantrace1 cleantrace)

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
      CFunPost  { f = obj1_fun2; base = v0; args = v1; result = vnull; call_type = Method };
      CFunExit { ret = vtrue; exc = vundef };
      CFunPost  { f = obj1_fun4; base = v0; args = v1; result = vtrue; call_type = Function };
      ] cleantrace)
      
let _ =
  Test.run_tests [ test1; test2; test3; test4 ]