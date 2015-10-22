(** Basic data for tests. *)
open Types

(** Common instances of JavaScript values - base instances. *)
let vundef = OUndefined
let vnull = ONull
let vtrue = OBoolean true
let vfalse = OBoolean false
let v0 = ONumberInt 0
let v1 = ONumberInt 1
let v2 = ONumberInt 2
let vpi = ONumberFloat 3.14
let vsqrt2 = ONumberFloat 1.41
let valpha = OString "alpha"
let vbeta = OString "beta"
let vmain = OSymbol "main"

(** Three versions of function tables, two equivalent but permuted, and one different. *)
let funcin1 = Local { from_toString = "func 1 toString"; from_jalangi = Some "func 1 jalangi" }
let funcin2 = Local { from_toString = "func 2 toString"; from_jalangi = Some "func 2 jalangi" }
let funcin3 = Local { from_toString = "func 3 toString"; from_jalangi = Some "func 3 jalangi" }
let funcstd = Local { from_toString = "func std toString"; from_jalangi = None }
let funcext1 = External 1
let funcext2 = External 2
let funcapply = External 100
let funccall = External 101
let functostring = External 102

let functab1 = [| funcapply; funccall; functostring; funcin1; funcin2; funcstd; funcext1 |]
(** functab2 is a cyclic permutation of functab1 *)
let functab2 = [| funcapply; funccall; functostring; funcin2; funcstd; funcext1; funcin1 |]
(** functab3 is distinct from functab1 and functab2 *)
let functab3 = [| funcapply; funccall; functostring; funcin1; funcin3; funcstd; funcext2 |]

(** Helpers for object table generation *) 
let simplefields fields =
   ("toString", OFunction(0, 2)) :: fields
   |> List.map (fun (k, v) ->
     (k, { value = v;  writable = true; get = None; set = None; enumerable = true; configurable = true }))
   |> List.fold_left (fun m (k, v) -> Misc.StringMap.add k v m) Misc.StringMap.empty

let funcfields = simplefields [ ("call", OFunction(0, 1)); ("apply", OFunction(0, 0)) ]

let objglobal =
  simplefields [
    ("Function", OObject 0); ("prototype", OObject 0);
    ("call", OFunction(0, 1)); ("apply", OFunction(0, 0))
   ]

(** Three versions of object tables, two equivalent but permuted, and one different. *)
(** Object table 1 - it contains one cyclic structure, one list structure, one special object and six
 * simple objects, including the functions defined above. *)
let obj1_cyc1 = OObject 0
let obj1_cyc2 = OObject 1
let obj1_cyc3 = OObject 2
let obj1_list1 = OObject 3
let obj1_list2 = OObject 4
let obj1_list3 = OObject 5
let obj1_fun1 = OFunction (6, 3)
let obj1_fun2 = OFunction (7, 4)
let obj1_fun3 = OFunction (8, 5)
let obj1_fun4 = OFunction (9, 6)
let obj1_simp1 = OObject 10
let obj1_simp2 = OObject 11
let obj1_special = OOther ("special", 12)

let obj1desc_cyc1 =
  simplefields [ ("next", obj1_cyc2); ("val", v0) ]
let obj1desc_cyc2 =
  simplefields [ ("next", obj1_cyc3); ("val", v1) ]
let obj1desc_cyc3 =
  simplefields [ ("next", obj1_cyc1); ("val", v2) ]
let obj1desc_list1 =
  simplefields [ ("next", obj1_list2); ("val", vpi) ]
let obj1desc_list2 =
  simplefields [ ("next", obj1_list3); ("val", vsqrt2) ]
let obj1desc_list3 =
  simplefields [ ("val1", vtrue); ("val2", vfalse) ]
let obj1desc_fun1 = funcfields
let obj1desc_fun2 = funcfields
let obj1desc_fun3 = funcfields
let obj1desc_fun4 = funcfields
let obj1desc_simp1 = simplefields [ ("0", vnull); ("1", vundef) ]
let obj1desc_simp2 = simplefields []
let obj1desc_special =
  Misc.StringMap.add "_getter"
    { value = vundef; get = Some obj1_fun1; set = Some obj1_fun1;
      configurable = false; enumerable = false; writable = true }
    Misc.StringMap.empty
     
let objtab1 = [|
  objglobal;
  obj1desc_cyc1; obj1desc_cyc2; obj1desc_cyc3;
  obj1desc_list1; obj1desc_list2; obj1desc_list3;
  obj1desc_fun1; obj1desc_fun2;
  obj1desc_fun3; obj1desc_fun4;
  obj1desc_simp1; obj1desc_simp2; obj1desc_special
|]

(** Object table 2 - it contains one cyclic structure, one list structure, one special object and six
 * simple objects, including the functions defined above. *)
let obj2_cyc1 = OObject 3
let obj2_cyc2 = OObject 4
let obj2_cyc3 = OObject 5
let obj2_list1 = OObject 0
let obj2_list2 = OObject 1
let obj2_list3 = OObject 2
let obj2_fun1 = OFunction (8, 4)
let obj2_fun2 = OFunction (9, 5)
let obj2_fun3 = OFunction (6, 6)
let obj2_fun4 = OFunction (7, 2)
let obj2_simp1 = OObject 12
let obj2_simp2 = OObject 11
let obj2_special = OOther ("special", 10)

let obj2desc_cyc1 =
  simplefields [ ("next", obj2_cyc2); ("val", v0) ]
let obj2desc_cyc2 =
  simplefields [ ("next", obj2_cyc3); ("val", v1) ]
let obj2desc_cyc3 =
  simplefields [ ("next", obj2_cyc1); ("val", v2) ]
let obj2desc_list1 =
  simplefields [ ("next", obj2_list2); ("val", vpi) ]
let obj2desc_list2 =
  simplefields [ ("next", obj2_list3); ("val", vsqrt2) ]
let obj2desc_list3 =
  simplefields [ ("val1", vtrue); ("val2", vfalse) ]
let obj2desc_fun1 = funcfields
let obj2desc_fun2 = funcfields
let obj2desc_fun3 = funcfields
let obj2desc_fun4 = funcfields
let obj2desc_simp1 = simplefields [ ("0", vnull); ("1", vundef) ]
let obj2desc_simp2 = simplefields []
let obj2desc_special =
  Misc.StringMap.add "_getter"
    { value = vundef; get = Some obj2_fun1; set = Some obj2_fun1;
      configurable = false; enumerable = false; writable = true }
    Misc.StringMap.empty
     
let objtab2 = [|
  objglobal;
  obj2desc_list1; obj2desc_list2; obj2desc_list3;
  obj2desc_cyc1; obj2desc_cyc2; obj2desc_cyc3;
  obj2desc_fun3; obj2desc_fun4;
  obj2desc_fun1; obj2desc_fun2;
  obj2desc_special; obj2desc_simp2; obj2desc_simp1
|]

(** Object table 3 - similar to object table 1, but with subtle differences
 * in the objects themselves. *)
let obj3_cyc1 = OObject 0
let obj3_cyc2 = OObject 1
let obj3_cyc3 = OObject 2
let obj3_list1 = OObject 3
let obj3_list2 = OObject 4
let obj3_list3 = OObject 5
let obj3_fun1 = OFunction (6, 3)
let obj3_fun2 = OFunction (7, 4)
let obj3_fun3 = OFunction (8, 5)
let obj3_fun4 = OFunction (9, 6)
let obj3_simp1 = OObject 10
let obj3_simp2 = OObject 11
let obj3_special = OOther ("special", 12)

let obj3desc_cyc1 =
  simplefields [ ("next", obj3_cyc2); ("val", vtrue) ]
let obj3desc_cyc2 =
  simplefields [ ("next", obj3_cyc3); ("val", valpha) ]
let obj3desc_cyc3 =
  simplefields [ ("next", obj3_cyc1); ("val", v2) ]
let obj3desc_list1 =
  simplefields [ ("next", obj3_list2); ("val", vpi) ]
let obj3desc_list2 =
  simplefields [ ("next", obj3_list3); ("val", vbeta) ]
let obj3desc_list3 =
  simplefields [ ("val1", vtrue); ("val2", vnull) ]
let obj3desc_fun1 = funcfields
let obj3desc_fun2 = funcfields
let obj3desc_fun3 = funcfields
let obj3desc_fun4 = funcfields
let obj3desc_simp1 = simplefields [ ("0", vtrue); ("1", vundef) ]
let obj3desc_simp2 = simplefields []
let obj3desc_special =
  Misc.StringMap.add "_getter"
    { value = vnull; get = Some obj3_fun1; set = Some obj3_fun1;
      configurable = false; enumerable = false; writable = false }
    Misc.StringMap.empty
     
let objtab3 = [|
  objglobal;
  obj3desc_cyc1; obj3desc_cyc2; obj3desc_cyc3;
  obj3desc_list1; obj3desc_list2; obj3desc_list3;
  obj3desc_fun1; obj3desc_fun2;
  obj3desc_fun3; obj3desc_fun4;
  obj3desc_simp1; obj3desc_simp2; obj3desc_special
|]

(** Global maps for the above object arrays. *)
let globals = let open Misc.StringMap in empty |> add "Function" (OObject 0)


(** A all-inclusive, well-bracketed trace exercising all (significant) cases.
 * It is built for object table 1. *)
let trace1 = let open Trace in
  [
    ForIn { iid = 1; value = obj1_simp2 };
    With { iid = 2; value = obj1_simp2 };
    ScriptEnter;
    Throw { iid = 4; value = obj1_simp2 };
    ScriptExc obj1_simp2;
    Declare { iid = 6; name = "e"; value = obj1_simp2; argument = None; isCatchParam = true };
    EndExpression 7;
    Literal { iid = 8; value = vtrue; hasGetterSetter = false };
    Write { iid = 9; name = "x"; lhs = vundef; value = vtrue; isGlobal = true; isScriptLocal = true };
    Read { iid = 10; name = "x"; value = vtrue; isGlobal = true; isScriptLocal = true };
    FunPre { iid = 11; f = obj1_fun1; base = obj1_cyc1; args = obj1_simp1; isConstructor = false; isMethod = true };
    FunEnter { iid = 12; f = obj1_fun1; this = obj1_cyc1; args = obj1_simp1 };
    Declare { iid = 12; name = "arguments"; value = obj1_simp1; argument = (Some (-1)); isCatchParam = false };
    Declare { iid = 12; name = "x"; value = vundef; argument = Some 0; isCatchParam = false };
    Return { iid = 13; value = vfalse };
    FunExit { iid = 14; Trace.ret = vfalse; exc = vundef };
    FunPost { iid = 15; f = obj1_fun1; args = obj1_simp1; isConstructor = false; isMethod = true; result = vfalse; base = obj1_cyc1 };
    ScriptEnter;
    BinPre { iid = 16; op = "+"; left = v0; right = v1; isOpAssign = false; isSwitchCaseComparison = false; isComputed = false };
    BinPost { iid = 16; op = "+"; left = v0; right = v1; isOpAssign = false; isSwitchCaseComparison = false; isComputed = false; result = v1 };
    UnaryPre { iid = 17; op = "-"; arg = v0 };
    UnaryPost { iid = 17; op = "-"; arg = v0; result = v0 };
    ScriptExit;
    GetFieldPre { iid =18; base = obj1_simp1; offset = "marker"; isComputed = false; isOpAssign = false; isMethodCall = false };
    GetField { iid =18; base = obj1_simp1; offset = "marker"; isComputed = false; isOpAssign = false; isMethodCall = false; value = vundef };
    PutFieldPre { iid =19; base = obj1_simp1; offset = "marker"; isOpAssign = false; isComputed =false; value = vundef };
    PutField { iid =19; base = obj1_simp1; offset = "marker"; isOpAssign = false; isComputed =false; value = vundef };
    Literal { iid = 20; value = obj1_simp2; hasGetterSetter = false };
    Declare { iid = 21; name = "y"; value = obj1_simp2; argument = None; isCatchParam = false };
    Conditional { iid = 22; value = vfalse }
  ]

let tracefile1: Trace.tracefile = (functab1, objtab1, trace1, globals, true)
