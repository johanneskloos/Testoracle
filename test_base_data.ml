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
let obj1_cyc1 = OObject 1
let obj1_cyc2 = OObject 2
let obj1_cyc3 = OObject 3
let obj1_list1 = OObject 4
let obj1_list2 = OObject 5
let obj1_list3 = OObject 6
let obj1_fun1 = OFunction (7, 3)
let obj1_fun2 = OFunction (8, 4)
let obj1_fun3 = OFunction (9, 5)
let obj1_fun4 = OFunction (10, 6)
let obj1_simp1 = OObject 11
let obj1_simp2 = OObject 12
let obj1_special = OOther ("special", 13)

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
let obj2_cyc1 = OObject 4
let obj2_cyc2 = OObject 5
let obj2_cyc3 = OObject 6
let obj2_list1 = OObject 1
let obj2_list2 = OObject 2
let obj2_list3 = OObject 3
let obj2_fun1 = OFunction (9, 4)
let obj2_fun2 = OFunction (10, 5)
let obj2_fun3 = OFunction (7, 6)
let obj2_fun4 = OFunction (8, 2)
let obj2_simp1 = OObject 13
let obj2_simp2 = OObject 12
let obj2_special = OOther ("special", 11)

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
let obj3_cyc1 = OObject 1
let obj3_cyc2 = OObject 2
let obj3_cyc3 = OObject 3
let obj3_list1 = OObject 4
let obj3_list2 = OObject 5
let obj3_list3 = OObject 6
let obj3_fun1 = OFunction (7, 3)
let obj3_fun2 = OFunction (8, 4)
let obj3_fun3 = OFunction (9, 5)
let obj3_fun4 = OFunction (10, 6)
let obj3_simp1 = OObject 11
let obj3_simp2 = OObject 12
let obj3_special = OOther ("special", 13)

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

let cleantrace1 = let open Cleantrace in [
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
    ]

let cleantrace1_args = [
    None;
    None;
    None;
    None;
    None;
    None;
    None;
    None;
    None;
    None;
    None;
    Some (get_object obj1_simp1);
    Some (get_object obj1_simp1);
    Some (get_object obj1_simp1);
    Some (get_object obj1_simp1);
    None;
    None;
    None;
    None;
    None;
    None;
    None;
    None;
    None;
    None;
    None
    ]

let ct1_l_e = Reference.reference_of_local_name "e"
and ct1_g_x = Reference.reference_of_name true Misc.StringMap.empty true "x"
and ct1_args = Reference.reference_of_local_name "arguments"
and ct1_arg0 = Reference.reference_of_field obj1_simp1 "0"
and ct1_arg1 = Reference.reference_of_field obj1_simp1 "1"
and ct1_toString = Reference.reference_of_field obj1_simp1 "toString"
and ct1_toString' = Reference.reference_of_field obj1_simp2 "toString"
and ct1_l_y = Reference.reference_of_local_name "y"
and ct1_f_simp1_marker = Reference.reference_of_field obj1_simp1 "marker"
and ct1_this = Reference.reference_of_local_name "this"
  
let cleantrace1_updates = 
  let init = Reference.reference_of_fieldref (Function (0, 2), "toString") in [
    Some (init, 0);
    Some (init, 0);
    Some (init, 0);
    Some (init, 0);
    Some (init, 0);
    Some (ct1_l_e, 0);
    Some (ct1_l_e, 0);
    Some (ct1_l_e, 0);
    Some (ct1_g_x, 0);
    Some (ct1_g_x, 0);
    Some (ct1_g_x, 0);
    Some (ct1_g_x, 0);
    Some (ct1_args, 0);
    Some (ct1_arg0, 0);
    Some (ct1_arg0, 0);
    Some (ct1_arg0, 0);
    Some (ct1_arg0, 0);
    Some (ct1_arg0, 0);
    Some (ct1_arg0, 0);
    Some (ct1_arg0, 0);
    Some (ct1_arg0, 0);
    Some (ct1_arg0, 0);
    Some (ct1_f_simp1_marker, 1);
    Some (ct1_f_simp1_marker, 1);
    Some (ct1_l_y, 0);
    Some (ct1_l_y, 0)
    ]
    
let ct1ver_init =
    let add_field obj fld map =
        Reference.ReferenceMap.add (Reference.reference_of_fieldref (obj, fld)) 0 map
    in let add_basic obj map =
        map
        |> add_field obj "Function"
        |> add_field obj "apply"
        |> add_field obj "call"
        |> add_field obj "prototype"
        |> add_field obj "toString"
    in
    Reference.ReferenceMap.empty
    |> add_basic (Object 0)
    |> add_basic (Function (0,0))
    |> add_basic (Function (0,1))
    |> add_basic (Function (0,2))

let ct1ver_e = Reference.ReferenceMap.add ct1_l_e 0 ct1ver_init
let ct1ver_x = Reference.ReferenceMap.add ct1_g_x 0 ct1ver_e
let ct1ver_funpre = let open Reference.ReferenceMap in
  ct1ver_x |> add ct1_arg0 0 |> add ct1_arg1 0 |> add ct1_toString 0
let ct1ver_enter = let open Reference.ReferenceMap in
  ct1ver_funpre |> add ct1_this 0
let ct1ver_args =  let open Reference.ReferenceMap in
  ct1ver_enter |> add ct1_args 0
let ct1ver_arg0 = Reference.ReferenceMap.add ct1_arg0 0 ct1ver_args
let ct1ver_f = Reference.ReferenceMap.add ct1_f_simp1_marker 0 ct1ver_funpre
let ct1ver_f' = Reference.ReferenceMap.add ct1_f_simp1_marker 1 ct1ver_f
let ct1ver_simp2 = Reference.ReferenceMap.add ct1_toString' 0 ct1ver_f'
let ct1ver_y = Reference.ReferenceMap.add ct1_l_y 0 ct1ver_simp2

let cleantrace1_versions =
    [
      ct1ver_init;
      ct1ver_init;
      ct1ver_init;
      ct1ver_init;
      ct1ver_init;
      ct1ver_e;
      ct1ver_e;
      ct1ver_e;
      ct1ver_x;
      ct1ver_x;
      ct1ver_funpre;
      ct1ver_enter;
      ct1ver_args;
      ct1ver_arg0;
      ct1ver_arg0;
      ct1ver_funpre;
      ct1ver_funpre;
      ct1ver_funpre;
      ct1ver_funpre;
      ct1ver_funpre;
      ct1ver_funpre;
      ct1ver_f;
      ct1ver_f';
      ct1ver_simp2;
      ct1ver_y;
      ct1ver_y
    ]

let ct1al_emp = Misc.StringMap.empty
let ct1al_x = Misc.StringMap.add "x" (objectid_of_jsval obj1_simp1, "0") ct1al_emp

let cleantrace1_aliases = let open Cleantrace in [
    ct1al_emp;
    ct1al_emp;
    ct1al_emp;
    ct1al_emp;
    ct1al_emp;
    ct1al_emp;
    ct1al_emp;
    ct1al_emp;
    ct1al_emp;
    ct1al_emp;
    ct1al_emp;
    ct1al_emp;
    ct1al_emp;
    ct1al_x;
    ct1al_x;
    ct1al_emp;
    ct1al_emp;
    ct1al_emp;
    ct1al_emp;
    ct1al_emp;
    ct1al_emp;
    ct1al_emp;
    ct1al_emp;
    ct1al_emp;
    ct1al_emp;
    ct1al_emp
    ]

let cleantrace1_facts =
  let fact_tuple = 
      let t1 = List.combine cleantrace1_args cleantrace1_updates
      and t2 = List.combine cleantrace1_versions cleantrace1_aliases in
      List.combine t1 t2
  in let open LocalFacts in
  List.map (fun ((la, lu), (v, a)) -> { last_arguments = la; last_update = lu; versions = v; aliases = a }) fact_tuple
 
let argtrace1 = List.combine cleantrace1 cleantrace1_args
let facttrace1 = List.combine cleantrace1 cleantrace1_facts

let trace1_pointsto =
  let open Reference.VersionReferenceMap in
  let add_simple_fields (obj: objectid) (fields: (string * jsval) list) ptm =
    List.fold_left
      (fun ptm (fld, value) -> add (Reference.reference_of_fieldref (obj, fld), 0) value ptm)
      ptm fields in
  let add_func obj ptm =
    add_simple_fields obj
      [ ("Function", OObject 0); ("apply", OFunction(0,0)); ("call", OFunction(0,1)); ("prototype", OObject 0); ("toString", OFunction(0,2)) ]
      ptm in
  empty
  |> add_func (Function (0, 0))
  |> add_func (Function (0, 1)) 
  |> add_func (Function (0, 2))
  |> add_func (Object 0)
  |> add (ct1_l_e, 0) obj1_simp2
  |> add (ct1_g_x, 0) vtrue
  |> add (ct1_args, 0) obj1_simp1
  |> add_simple_fields (Object 11) [ "0", vnull; "1", vundef; "marker", vundef; "toString", (OFunction (0, 2)) ]
  |> add (ct1_f_simp1_marker, 1) vundef
  |> add (ct1_l_y, 0) obj1_simp2
  |> add (Reference.reference_of_local_name "this", 0) (OObject 0)
  |> add_simple_fields (Object 12) [ "toString", (OFunction (0, 2)) ]