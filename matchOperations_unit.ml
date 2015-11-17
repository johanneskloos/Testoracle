open MatchTypes
open MatchOperations
open Test_base_data
open Kaputt.Abbreviations
open Richtrace

let (|>) = Pervasives.(|>)

let default_state = {
	rt1 = test_rt1;
	rt2 = test_rt2;
	objeq = ref Misc.IntIntMap.empty;
	initialisation_data = Reference.VersionReferenceSet.empty;
	toString_data = [];
	nonequivalent_functions = Misc.IntIntSet.empty;
	known_blocked = Misc.IntIntMap.empty
}

let reset_default_state () =
	default_state.objeq := Misc.IntIntMap.empty;
	default_state.nonequivalent_functions <- Misc.IntIntSet.empty;
	default_state.known_blocked <- Misc.IntIntMap.empty
 
let default_predicate_tester predname pred poslist neglist =
	List.map (fun pos ->
			Test.make_simple_test
				~title:(predname ^ ": " ^ Misc.to_string Richtrace.pp_rich_operation pos)
				(fun () -> reset_default_state();
					assert_is_None ~prn:(Misc.to_string MatchTypes.pp_mismatch) (pred default_state pos)))
			poslist @
	List.map (fun neg ->
			Test.make_simple_test
				~title:(predname ^ ": " ^ Misc.to_string Richtrace.pp_rich_operation neg)
				(fun () -> reset_default_state();
					assert_is_Some ~prn:(Misc.to_string MatchTypes.pp_mismatch) (pred default_state neg)))
			neglist

let default_simple_predicate_tester predname pred poslist neglist =
	List.map (fun pos ->
			Test.make_simple_test
				~title:(predname ^ ": " ^ Misc.to_string Richtrace.pp_rich_operation pos)
				(fun () -> assert_is_None ~prn:(Misc.to_string MatchTypes.pp_mismatch) (pred pos)))
			poslist @
	List.map (fun neg ->
			Test.make_simple_test
				~title:(predname ^ ": " ^ Misc.to_string Richtrace.pp_rich_operation neg)
				(fun () -> assert_is_Some ~prn:(Misc.to_string MatchTypes.pp_mismatch) (pred neg)))
			neglist

(* Needs special handling *)
let tests_is_instrumentation_write =
	default_predicate_tester "is_instrumentation_write" is_instrumentation_write
		[]
		[  ]
		
val is_instrumentation_write : predicate
val is_function_update : predicate
val may_insert_in_init : predicate
val may_insert_in_matching_simple : simple_predicate
val may_insert_in_wrap_simple : predicate
val may_insert_in_toString_simple : simple_predicate
val is_fun_literal : simple_predicate
val is_local_decl : simple_predicate
val is_fun_read : simple_predicate
val is_end_of_expr : simple_predicate
val is_unobservable : simple_predicate
val is_toplevel : simple_predicate
val is_throw : simple_predicate
val is_write : simple_predicate
val is_exit : simple_predicate
val is_post_exit : simple_predicate
val is_enter : simple_predicate
val is_use_strict : simple_predicate
val is_not_function : simple_predicate
val is_catch : simple_predicate

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