open MatchTypes
open MatchOperations
open Test_base_data
open Kaputt.Abbreviations
open Richtrace

let (|>) = Pervasives.(|>)

let yref = Reference.reference_of_local_name "y"
let default_state = {
	rt1 = test_rt1;
	rt2 = test_rt2;
	objeq = ref Misc.IntIntMap.empty;
	initialisation_data =
		Reference.VersionReferenceSet.empty
		|> Reference.VersionReferenceSet.add (yref, 0);
	toString_data = [];
	nonequivalent_functions = Misc.IntIntSet.empty;
	known_blocked = Misc.IntIntMap.empty
}

let reset_default_state () =
	default_state.objeq := Misc.IntIntMap.empty;
	default_state.nonequivalent_functions <- Misc.IntIntSet.empty;
	default_state.known_blocked <- Misc.IntIntMap.empty
 
let pp_rich_event pp (op, _) = Richtrace.pp_rich_operation pp op

let default_predicate_tester predname pred poslist neglist =
	List.map (fun pos ->
			Test.make_simple_test
				~title:(predname ^ ": " ^ Misc.to_string pp_rich_event pos)
				(fun () -> reset_default_state();
					assert_is_None ~prn:(Misc.to_string MatchTypes.pp_mismatch) (pred default_state pos)))
			poslist @
	List.map (fun neg ->
			Test.make_simple_test
				~title:(predname ^ ": " ^ Misc.to_string pp_rich_event neg)
				(fun () -> reset_default_state();
					assert_is_Some ~prn:(Misc.to_string MatchTypes.pp_mismatch) (pred default_state neg)))
			neglist

let default_simple_predicate_tester predname pred (poslist: Richtrace.rich_event list) (neglist: Richtrace.rich_event list) =
	List.map (fun pos ->
			Test.make_simple_test
				~title:(predname ^ ": " ^ Misc.to_string pp_rich_event pos)
				(fun () -> assert_is_None ~prn:(Misc.to_string MatchTypes.pp_mismatch) (pred pos)))
			poslist @
	List.map (fun neg ->
			Test.make_simple_test
				~title:(predname ^ ": " ^ Misc.to_string pp_rich_event neg)
				(fun () -> assert_is_Some ~prn:(Misc.to_string MatchTypes.pp_mismatch) (pred neg)))
			neglist

let xref = Reference.reference_of_local_name "x"
let test_funpre = (RFunPre { f = obj1_fun1; base = vundef; args = obj1_simp1; call_type = Cleantrace.Method }, local_facts_1) 
let test_funpost = (RFunPost { f = obj1_fun1; base = vundef; args = obj1_simp1; result = v1 }, local_facts_1)
let test_literal = (RLiteral { value = v2; hasGetterSetter = false }, local_facts_1)
let test_forin = (RForIn obj1_cyc1, local_facts_1)
let test_local = (RLocal { name = "x"; ref = (xref, 0) }, local_facts_1)
let test_catch = (RCatch { name = "x"; ref = (xref, 0) }, local_facts_1)
let test_alias = (RAlias { name = "x"; ref = (xref, 0); source = Argument 0 }, local_facts_1)
let test_read = (RRead { ref = (xref, 0); value = v1 }, local_facts_1)
let test_write = (RWrite { oldref = (xref, 0); ref = (xref, 1); value = v1; success = true }, local_facts_1)
let test_return = (RReturn obj1_cyc1, local_facts_1)
let test_throw = (RThrow obj1_cyc1, local_facts_1)
let test_with = (RWith obj1_cyc1, local_facts_1)
let test_enter = (RFunEnter { f = obj1_fun1; this = vundef; args = obj1_simp1 }, local_facts_1)
let test_exit = (RFunExit { ret = v1; exc = vundef }, local_facts_1)
let test_senter = (RScriptEnter, local_facts_1)
let test_sexit = (RScriptExit, local_facts_1)
let test_sexc = (RScriptExc v0, local_facts_1)
let test_binary = (RBinary { op = "+"; left = v1; right = v2; result = v1 }, local_facts_1)
let test_unary = (RUnary { op = "-"; arg = v0; result = v0 }, local_facts_1)
let test_eend = (REndExpression, local_facts_1)
let test_cond = (RConditional vtrue, local_facts_1)

let test_all_ops = [
	test_funpre; test_funpost; test_literal; test_forin; test_local; test_catch; test_alias; test_read; test_write;
	test_return; test_throw; test_with; test_enter; test_exit; test_senter; test_sexit; test_sexc; test_binary;
	test_unary; test_eend; test_cond
	]

let list_delta large small = List.filter (fun x -> not (List.mem x small)) large

let simple_predicate_tester_pos predname pred (poslist: Richtrace.rich_event list) =
	default_predicate_tester predname pred poslist (list_delta test_all_ops poslist)
let simple_predicate_tester_neg predname pred neglist =
	default_predicate_tester predname pred (list_delta test_all_ops neglist) neglist

let simple_simple_predicate_tester_pos predname pred poslist =
	default_simple_predicate_tester predname pred poslist (list_delta test_all_ops poslist)
let simple_simple_predicate_tester_neg predname pred neglist =
	default_simple_predicate_tester predname pred (list_delta test_all_ops neglist) neglist

let tests_may_insert_in_matching_simple =
	simple_simple_predicate_tester_neg
		"may_insert_in_matching_simple"
		may_insert_in_matching_simple
		[ test_funpre; test_exit ]

let tests_may_insert_in_toString_simple = 
	simple_simple_predicate_tester_neg
		"may_insert_in_toString_simple"
		may_insert_in_toString_simple
		[ test_funpre; test_exit; test_write; test_throw ]

let tests_is_fun_literal_simple =
	simple_simple_predicate_tester_pos "is_fun_literal" is_fun_literal
		[ (RLiteral { value = obj1_fun1; hasGetterSetter = false }, local_facts_1) ]

let tests_is_local_decl_simple =
	simple_simple_predicate_tester_pos "is_local_decl" is_local_decl
		[ test_local ]

let tests_is_fun_read_simple =
	simple_simple_predicate_tester_pos "is_fun_read" is_fun_read
		[ (RRead { ref = (xref, 0); value = obj1_fun1 }, local_facts_1) ]

let tests_is_end_of_expr_simple =
	simple_simple_predicate_tester_pos "is_end_of_expr" is_end_of_expr
		[ test_eend ]

let tests_is_unobvserable =
	simple_simple_predicate_tester_neg "is_unobservable" is_unobservable
		[ test_write; test_funpre; test_exit; test_throw ]

let tests_is_toplevel =
	simple_simple_predicate_tester_neg "is_toplevel" is_toplevel
		[ test_exit; test_return; test_enter ]

let tests_is_throw =
	simple_simple_predicate_tester_pos "is_throw" is_throw
		[ test_throw ]
				
let tests_is_write =
	simple_simple_predicate_tester_pos "is_write" is_write
		[ test_write ]
				
let tests_is_exit =
	simple_simple_predicate_tester_pos "is_exit" is_exit
		[ test_exit ]
				
let tests_is_post_exit =
	simple_simple_predicate_tester_pos "is_post_exit" is_post_exit
		[ test_funpost ]
				
let tests_is_enter =
	simple_simple_predicate_tester_pos "is_enter" is_enter
		[ test_enter ]
				
let tests_is_catch =
	simple_simple_predicate_tester_pos "is_catch" is_catch
		[ test_catch ]

let tests_is_not_function =
	simple_simple_predicate_tester_neg "is_not_function" is_not_function
		[ test_funpre; test_return; test_enter; test_exit ]
		
let simple_predicate_tests =
	tests_may_insert_in_matching_simple @
	tests_may_insert_in_toString_simple @
	tests_is_fun_literal_simple @
	tests_is_local_decl_simple @
	tests_is_fun_read_simple @
	tests_is_end_of_expr_simple @
	tests_is_unobvserable @
	tests_is_toplevel @
	tests_is_throw @
	tests_is_write @
	tests_is_exit @
	tests_is_post_exit @
	tests_is_enter @
	tests_is_catch @
	tests_is_not_function

								(*				
val is_not_function : simple_predicate

(* Needs special handling *)
val is_instrumentation_write : predicate
val is_function_update : predicate
val may_insert_in_init : predicate
val may_insert_in_wrap_simple : predicate

			(*
												  
type 'a comparator = matching_state -> 'a -> 'a -> mismatch option
type predicate = matching_state -> Richtrace.rich_event -> mismatch option
type call_comparator = Richtrace.rich_event comparator
type simple_predicate = Richtrace.rich_event -> mismatch option

val is_internal_call_impl : Richtrace.rich_tracefile -> int -> mismatch option
val is_internal_call : Richtrace.rich_tracefile -> Richtrace.rich_event -> mismatch option

val match_operations : Richtrace.rich_event comparator
val is_matching_internal_call : call_comparator
val is_matching_external_call : call_comparator
val is_matching_toString_call : call_comparator
val may_be_wrapper_entry : call_comparator
val is_matching_entry: Richtrace.rich_event comparator

val interpret_cond : matching_state -> Richtrace.rich_event -> Richtrace.rich_event -> match_condition -> mismatch option
*)
*)

let _ = Test.run_tests
	(simple_predicate_tests)