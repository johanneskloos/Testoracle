open Types
open Kaputt.Abbreviations
open Test_base_data
open Richtrace
open LocalFacts

let (|>) = Pervasives.(|>)

let facts_init = { last_arguments = None; last_update = None; versions = ct1ver_init; aliases = ct1al_emp }
let facts_catch = { last_arguments = None; last_update = Some (ct1_l_e, 0); versions = ct1ver_e; aliases = ct1al_emp }
let facts_write1 = { last_arguments = None; last_update = Some (ct1_g_x, 0); versions = ct1ver_x; aliases = ct1al_emp }
let facts_funpre = { last_arguments = None; last_update = Some (ct1_args, 0); versions = ct1ver_funpre; aliases = ct1al_emp }
let facts_funenter = { last_arguments = Some (get_object obj1_simp1); last_update = Some (ct1_args, 0); versions = ct1ver_enter; aliases = ct1al_emp }
let facts_arguments = { last_arguments = Some (get_object obj1_simp1); last_update = Some (ct1_args, 0); versions = ct1ver_args; aliases = ct1al_emp }
let facts_arg0 = { last_arguments = Some (get_object obj1_simp1); last_update = Some (ct1_args, 0); versions = ct1ver_arg0; aliases = ct1al_x }
let facts_exit = { last_arguments = None; last_update = Some (ct1_args, 0); versions = ct1ver_funpre; aliases = ct1al_emp }
let facts_post = { last_arguments = None; last_update = Some (ct1_args, 0); versions = ct1ver_funpre; aliases = ct1al_emp }
let facts_write2 = { last_arguments = None; last_update = Some (ct1_f_simp1_marker, 0); versions = ct1ver_f; aliases = ct1al_emp }
let facts_write3 = { last_arguments = None; last_update = Some (ct1_f_simp1_marker, 0); versions = ct1ver_f'; aliases = ct1al_emp }
let facts_literal = { last_arguments = None; last_update = Some (ct1_f_simp1_marker, 0); versions = ct1ver_simp2; aliases = ct1al_emp }
let facts_local = { last_arguments = None; last_update = Some (ct1_l_y, 0); versions = ct1ver_y; aliases = ct1al_emp }

let richtrace1 = let open Richtrace in [
    RForIn obj1_simp2, facts_init;
    RWith obj1_simp2, facts_init; (* FIXME handle with properly *)
    RScriptEnter, facts_init;
    RThrow obj1_simp2, facts_init;
    RScriptExc obj1_simp2, facts_init;
    RCatch { name = "e"; ref = (ct1_l_e, 0) }, facts_catch;
    RWrite { ref = (ct1_l_e, 0); oldref = (ct1_l_e, 0); value = obj1_simp2; success = true }, facts_catch;
    REndExpression, facts_catch;
    RLiteral { value = vtrue; hasGetterSetter = false }, facts_catch;
    RWrite { ref = (ct1_g_x, 0); oldref = (ct1_g_x, 0); value = vtrue; success = true }, facts_write1;
    RRead { ref = (ct1_g_x, 0); value = vtrue }, facts_write1;
    RFunPre { f = obj1_fun1; base = obj1_cyc1; args = obj1_simp1; call_type = Cleantrace.Method }, facts_funpre;
    RFunEnter { f = obj1_fun1; this = obj1_cyc1; args = obj1_simp1 }, facts_funenter;
    RLocal { name = "arguments"; ref = (ct1_args, 0) }, facts_arguments;
    RWrite { ref = (ct1_args, 0); oldref = (ct1_args, 0); value = obj1_simp1; success = true }, facts_arguments;
    RAlias { name = "x"; ref = (ct1_arg0, 0); source = Argument 0 }, facts_arg0;
    RReturn vfalse, facts_arg0;
    RFunExit { ret = vfalse; exc = vundef }, facts_exit;
    RFunPost { f = obj1_fun1; args = obj1_simp1; result = vfalse; base = obj1_cyc1 }, facts_post;
    RScriptEnter, facts_post;
    RBinary { op = "+"; left = v0; right = v1; result = v1 }, facts_post;
    RUnary { op = "-"; arg = v0; result = v0 }, facts_post;
    RScriptExit, facts_post;
    RRead { ref = (ct1_f_simp1_marker, 0); value = vundef }, facts_write2;
    RWrite { ref = (ct1_f_simp1_marker, 1); oldref = (ct1_f_simp1_marker, 0); value = vundef; success = true }, facts_write3;
    RLiteral { value = obj1_simp2; hasGetterSetter = false }, facts_literal;
    RLocal { name = "y"; ref = (ct1_l_y, 0) }, facts_local;
    RWrite { ref = (ct1_l_y, 0); oldref = (ct1_l_y, 0); value = obj1_simp2; success = true }, facts_local;
    RConditional vfalse, facts_local;
    ]

let rop_eq (op1, facts1) (op2, facts2) =
  op1 = op2 &&
  facts1.last_arguments = facts2.last_arguments &&
  Reference.ReferenceMap.equal (=) facts1.versions facts2.versions &&
  Misc.StringMap.equal (=) facts1.aliases facts2.aliases
  
  
let test1 = Test.make_simple_test ~title:"calculate_rich_tracefile" (fun () ->
                let rt = Richtrace.calculate_rich_tracefile (functab1, objtab1, trace1, globals, true) in
                Assert.make_equal (=) (Misc.to_string pp_functions) functab1 rt.funcs;
                Assert.make_equal (=) (Misc.to_string pp_objects) objtab1 rt.objs;
                Assert.make_equal (=) (Misc.to_string pp_globals) globals rt.globals;
                Assert.equal_bool true rt.globals_are_properties;
                let module VRMFmt = FormatHelper.MapFormat(Reference.VersionReferenceMap) in
                Assert.make_equal (Reference.VersionReferenceMap.equal (=))
                  (Misc.to_string (VRMFmt.pp_print_map_default Reference.pp_versioned_reference pp_jsval))
                  trace1_pointsto rt.points_to;
                Assert.make_equal_list rop_eq
                  (Misc.to_string (FormatHelper.pp_print_pair pp_rich_operation LocalFacts.pp_local_facts))
                  richtrace1 rt.trace)

let _ = Test.run_tests [ test1 ]
 