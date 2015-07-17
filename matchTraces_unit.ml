open Trace
open MatchTraces
open Richtrace
open Kaputt
open Abbreviations
open Misc
open Reference
open LocalFacts

let (|>>) = (|>)
let (|>) = Pervasives.(|>)

 let funcs = [|
	Local { instrumented = "ar3wraw3eraw3r"; uninstrumented = "function () { stuff }" };
	External 1;
	Local { instrumented = "hsrthytyhjdrr"; uninstrumented = "function () { f() }" }
	|]
let add_field name value =
	StringMap.add name { value; writable = true; get = None; set = None; enumerable = true; configurable = true }
let objs = [|
	(* Default handling for functions *)
	StringMap.empty |> add_field "toString" (OFunction (9999, 0));
	StringMap.empty |> add_field "toString" (OFunction (9999, 0));
	StringMap.empty |> add_field "toString" (OFunction (9999, 0));
	(* base1 *)
	StringMap.empty;
	(* args1 *)
	StringMap.empty |> add_field "0" (ONumberInt 17) |> add_field "1" (ONumberInt 23);
	(* obj1 *)
	StringMap.empty |> add_field "x" ONull
	|]
let func1 = OFunction(0, 0)
let func2 = OFunction(1, 1)
let func3 = OFunction(2, 2)
let base1 = OObject 3
let args1 = OObject 4
let args2 = OObject 5
let obj1 = OObject 5
let val1 = ONumberInt 42
let val2 = ONumberFloat 3.14
let val3 = OBoolean true
let val4 = OString "xyz"
let obj2 = OObject 4
let ref1 = (reference_of_fieldref (4, "0"), 1)
let ref2 = (reference_of_fieldref (4, "0"), 0)
let ref3 = (reference_of_fieldref (5, "x"), 5)
let ref4 = (reference_of_fieldref (5, "x"), 8)
let ref5 = (reference_of_fieldref (0, "toString"), 0)
let ref6 = (reference_of_fieldref (0, "toString"), 1)
let add_pt = VersionReferenceMap.add
let gref1 = (Reference.reference_of_name false StringMap.empty true "a", 0)
let gref2 = (Reference.reference_of_name false StringMap.empty true "b", 0)
let points_to =
	VersionReferenceMap.empty
	|> add_pt ref1 (ONumberInt 17)
	|> add_pt ref2 (ONumberInt 18)
	|> add_pt ref3 (OBoolean true)
	|> add_pt ref4 (OBoolean true)
	|> add_pt ref5 (OFunction (9999, 0))
	|> add_pt ref6 (OFunction (666, 1))
  |> add_pt gref1 OUndefined
  |> add_pt gref2 ONull


let dummy_rt = {
	funcs;
	objs;
	trace = [];
	globals = StringMap.empty |> StringMap.add "a" (OObject 0) |> StringMap.add "b" (ONull);
	globals_are_properties = false;
	points_to
}
let state1_facts = {
	last_arguments = Some 42;
	last_parameters = Some 37;
	last_update = Some ref2;
	versions = ReferenceMap.empty
    |> ReferenceMap.add (fst ref1) 0
    |> ReferenceMap.add (fst ref3) 5
    |> ReferenceMap.add (fst ref5) 0
    |> ReferenceMap.add (fst gref1) 0
    |> ReferenceMap.add (fst gref2) 0;
	aliases = StringMap.empty
}

let add_init = VersionReferenceSet.add
let state1 = {
	rt1 = dummy_rt;
	rt2 = dummy_rt;
	objeq = IntIntMap.empty;
	initialisation_data =
		VersionReferenceSet.empty
		|> add_init ref3;
	facts1 = state1_facts;
	facts2 = state1_facts;
  toString_data = []
}

(* Format:
 * Operation, unobservable, write, exit, instrumentation write,
 * function update, insert in init, insert in matching (simple),
 * internal call, insert in wrap, insert in toString. *) 
let bc op =
	(op, true, false, false, false, false, true, true, false, true, true)
let operation_classification = [
	(RFunPre { f = func1; base = base1; args = args1; call_type = Function },
		false, false, false, false, false, false, false, true, false, false);
	(RFunPre { f = func2; base = base1; args = args1; call_type = Function },
		false, false, false, false, false, false, false, false, false, false);
	(RFunPre { f = func3; base = base1; args = args1; call_type = Function },
		false, false, false, false, false, false, false, true, false, false);
	(RFunPost { f = func1; base = base1; args = args1; result = val1 },
    false, false, false, false, false, false, false, false, false, false);
	(bc (RLiteral { value = val1; hasGetterSetter = false }));
	(bc (RForIn base1));
	(bc (RLocal { name = "x"; ref = (reference_of_local_name "x", 0) }));
	(bc (RAlias { name = "x"; source = Argument 1; ref = ref1 }));
	(bc (RRead { ref = ref1; value = val1 }));
	(RWrite { ref = ref1; oldref = ref2; value = val1; success = true },
		false, true, false, false, false, false, true, false, false, false);
	(RWrite { ref = ref4; oldref = ref3; value = val1; success = true },
		false, true, false, true, false, true, true, false, true, false);
	(RWrite { ref = ref6; oldref = ref5; value = val1; success = true },
		false, true, false, false, true, true, true, false, true, false);
	(* XXX some cases may be missing, but are not needed now:
   * - Local variables in wrapper code.
   *)
	(bc (RReturn val1));
  (RThrow val1, false, false, false, false, false, false, true, false, false, false);
	(bc (RWith val1));
	(bc (RFunEnter { f = func1; this = base1; args = args1 }));
	(RFunExit { ret = val1; exc = val1 },
   true, false, true, false, false, true, true, false, true, true);
	(bc (RScriptEnter));
	(bc (RScriptExit));
	(bc (RScriptExc val1));
	(bc (RBinary { op = "+"; left = val1; right = val1; result = val1 }));
	(bc (RUnary { op = "-"; arg = val1; result = val1 }));
	(bc (REndExpression));
	(bc (RConditional val1))
	];;

let test_operation_classification
		(op, op_unobservable, op_write, op_exit, op_instrumentation_write,
		op_function_update, op_in_init, op_in_matching, op_internal_call,
		op_in_wrap, op_in_tostring) =
	Test.make_simple_test
		~title: (to_string pp_rich_operation op ^ " classification")
		(fun () ->
					Assert.equal_bool
						~msg:"unobservable"
						op_unobservable
						(is_unobservable op);
					Assert.equal_bool ~msg:"write" op_write (is_write op);
					Assert.equal_bool ~msg:"exit" op_exit (is_exit op);
					Assert.equal_bool
						~msg:"instrumentation write"
						op_instrumentation_write
						(is_instrumentation_write state1 op);
					Assert.equal_bool
						~msg:"function update"
						op_function_update
						(is_function_update state1 op);
					Assert.equal_bool
						~msg:"internal call"
						op_internal_call
						(is_internal_call state1.rt1 op);
					Assert.equal_bool
						~msg:"can occur in init code"
						op_in_init
						(may_insert_in_init state1 op);
					Assert.equal_bool
						~msg:"can occur in matching code without stack change"
						op_in_matching
						(may_insert_in_matching_simple op);
					Assert.equal_bool
						~msg:"can occur in wrapping code"
						op_in_wrap
						(may_insert_in_wrap_simple state1 op);
					Assert.equal_bool
						~msg:"can occur in toString code"
						op_in_tostring
						(may_insert_in_toString_simple op));;

let operation_classification_tests =
	List.map test_operation_classification operation_classification

(** Some more small functions. *)

let match_source_test =
	Test.make_simple_test ~title:"match_source" (fun () ->
					Assert.is_true ~msg:"arguments with equal index"
						(match_source state1 (Argument 0) (Argument 0) |> fst);
					Assert.is_false ~msg:"arguments with nonequal index"
						(match_source state1 (Argument 0) (Argument 1) |> fst);
					Assert.is_false ~msg:"argument vs. with"
						(match_source state1 (Argument 0) (With ref1) |> fst)
			(* TODO with cases; with is broken anyway, though. *))

(* Format: * first function, second function, matching internal, *         *)
(* matching external, may be wrapper.                                      *)
let call_test_data = [
	(func1, func1, true, false, false);
	(func2, func2, false, true, false);
	(func1, func3, false, false, true);
	(func2, func1, false, false, false);
	];;

let call_test_maker (f1, f2, match_int, match_ext, match_wrap) =
	let op1 =
		RFunPre { f = f1; base = base1; args = args1; call_type = Method }
	and op2 =
		RFunPre { f = f2; base = base1; args = args1; call_type = Method }
	in
	Test.make_simple_test
		~title: (Format.asprintf "Functions matching: %a vs. %a"
				pp_rich_operation op1 pp_rich_operation op2)
		(fun () ->
					Assert.equal_bool ~msg:"matching internal call"
						match_int
						(is_matching_internal_call state1 op1 op2);
					Assert.equal_bool ~msg:"matching external call"
						match_ext
						(is_matching_external_call state1 op1 op2);
					Assert.equal_bool ~msg:"potential wrapper call"
						match_wrap
						(may_be_wrapper_entry state1 op1 op2))

let call_tests = List.map call_test_maker call_test_data

(** match_operations needs a large number of test cases. *)
let all_ops =
	[ RFunPre { f = func1; base = val1; args = val3; call_type = Method };
	RFunPre { f = func2; base = val1; args = val3; call_type = Method };
	RFunPre { f = func1; base = val2; args = args1; call_type = Method };
	RFunPre { f = func1; base = val1; args = val3; call_type = Method };
	RFunPre { f = func1; base = val1; args = val3; call_type = Function };
	RFunPost { f = func1; base = val1; args = val3; result = val1 };
	RFunPost { f = func2; base = val1; args = val3; result = val1 };
	RFunPost { f = func1; base = val2; args = val3; result = val1 };
	RFunPost { f = func1; base = val1; args = base1; result = val1 };
	RFunPost { f = func1; base = val1; args = val3; result = args1 };
	RLiteral { value = val1; hasGetterSetter = true };
	RLiteral { value = val1; hasGetterSetter = false };
	RLiteral { value = val2; hasGetterSetter = false };
	RForIn val1;
	RForIn args1;
	RLocal { name = "x"; ref = ref1 };
	RLocal { name = "x"; ref = ref3 };
	RLocal { name = "y"; ref = ref1 };
	RAlias { name = "x"; source = Argument 0; ref = ref1 };
	RAlias { name = "y"; source = Argument 0; ref = ref1 };
	RAlias { name = "x"; source = Argument 1; ref = ref1 };
	RAlias { name = "x"; source = Argument 0; ref = ref3 };
	RRead { ref = ref1; value = val2 };
	RRead { ref = ref1; value = val1 };
	RRead { ref = ref3; value = val2 };
	RWrite { ref = ref1; oldref = ref5; value = obj1; success = true };
	RWrite { ref = ref3; oldref = ref5; value = obj1; success = true };
	RWrite { ref = ref1; oldref = ref1; value = obj1; success = true };
	RWrite { ref = ref1; oldref = ref5; value = val1; success = true };
	RWrite { ref = ref1; oldref = ref5; value = obj1; success = false };
	RReturn val1;
	RReturn obj1;
	RThrow val1;
	RThrow obj1;
	RWith val1;
	RWith obj1;
	RFunEnter { f = func1; this = val1; args = args1 };
	RFunEnter { f = func2; this = val1; args = args1 };
	RFunEnter { f = func1; this = val2; args = args1 };
	RFunEnter { f = func1; this = val1; args = func3 };
	RFunExit { ret = val1; exc = val3 };
	RFunExit { ret = args1; exc = val3 };
	RFunExit { ret = val1; exc = val2 };
	RScriptEnter;
	RScriptExit;
	RScriptExc val1;
	RScriptExc base1;
	RBinary { op ="+"; left = val2; right = val1; result = val3 };
	RBinary { op ="-"; left = val2; right = val1; result = val3 };
	RBinary { op ="+"; left = base1; right = val1; result = val3 };
	RBinary { op ="+"; left = val2; right = base1; result = val3 };
	RBinary { op ="+"; left = val2; right = val1; result = val4 };
	RUnary { op ="!"; arg = val2; result = val1 };
	RUnary { op ="~"; arg = val2; result = val1 };
	RUnary { op ="!"; arg = base1; result = val1 };
	RUnary { op ="!"; arg = val2; result = base1 };
	REndExpression;
	RConditional val1;
	RConditional base1 ];;

let test_match_operations_one_case op1 op2 =
	Test.make_simple_test
		~title: (Format.asprintf "Matching %a and %a"
				pp_rich_operation op1 pp_rich_operation op2)
		(fun () ->
					Assert.equal_bool (op1 = op2) (match_operations state1 op1 op2 |> fst))
let test_match_operations_cross =
	List.map (fun op1 ->
					List.map (test_match_operations_one_case op1) all_ops)
		all_ops |> List.flatten

(* XXX we should also test the "sides match" part a bit more... *)

(* add_objeq is so straightforward that we don't test.
 * Ditto for get_state. *)
let test_is_matching_toString_call =
	Test.make_simple_test ~title:"is_matching_toString" (fun () ->
    let state' = { state1 with toString_data = [ func2 ] } in
    Assert.is_false ~msg:"not a call to toString"
      (is_matching_toString_call state'
      (RFunPre { f = func1; args = args1; base = obj1; call_type = Method })
      (RFunPre { f = func1; args = args1; base = obj1; call_type = Method }));
    Assert.is_true ~msg:"should match"
      (is_matching_toString_call state'
      (RFunPre { f = func2; args = args1; base = obj1; call_type = Method })
      (RFunPre { f = func1; args = args1; base = obj1; call_type = Method }));
    Assert.is_false ~msg:"base doesn't match"
      (is_matching_toString_call state'
      (RFunPre { f = func2; args = args1; base = obj1; call_type = Method })
      (RFunPre { f = func1; args = args1; base = val1; call_type = Method }));
    Assert.is_false ~msg:"not a known toString call"
      (is_matching_toString_call state1
      (RFunPre { f = func2; args = args1; base = obj1; call_type = Method })
      (RFunPre { f = func1; args = args1; base = obj1; call_type = Method }));
    Assert.is_false ~msg:"not a call"
      (is_matching_toString_call state' RScriptEnter RScriptEnter)
					)

let trace_init =
	List.map (fun op -> (op, state1_facts))
		[ RWrite { ref = ref6; oldref = ref5;
			value = val1; success = true };
		RRead { ref = ref1; value = val1 };
		RForIn val1;
		RScriptExit
		]

let can_be_added_as_initialisation_tests =
	[Test.make_simple_test ~title:"can_be_added_as_initialisation: trace and stack ok"
		(fun () -> Assert.is_true ~msg:"all ok"
						(can_be_added_as_initialisation state1 trace_init []));
	Test.make_simple_test ~title:"can_be_added_as_initialisation: bad trace"
		(fun () -> Assert.is_false ~msg:"all ok"
						(can_be_added_as_initialisation state1 [
								(RWrite { ref = ref3; oldref = ref4; value = val1; success = true }, state1_facts)
								] []));
	Test.make_simple_test ~title:"can_be_added_as_initialisation: bad stack"
		(fun () -> Assert.is_false ~msg:"all ok"
						(can_be_added_as_initialisation state1 trace_init [Regular]))]

type stack_delta = Keep | Pop | Push of mode
type classification = CInit | CWrap | CPair

let make_test_adapt_first_stack_and_extend_matching =
	List.map (fun (title, op, keep_first, stack_delta, classification) ->
					Test.make_simple_test ~title: title (fun () ->
									let trace1 = [ (RScriptEnter, state1_facts); (RScriptExit, state1_facts) ]
									and op1 = RForIn val1
									and facts1 = state1_facts
									and stack = [ Regular; Regular ]
									and op2 = RForIn val2 in
									let (trace1', stack', matching) =
										(adapt_first op op1 facts1 trace1,
											adapt_stack op stack,
											extend_matching op op1 op2 []) in
									Assert.equal ~msg:"Traces match"
										(if keep_first then (op1, facts1) :: trace1 else trace1) trace1';
									Assert.equal ~msg:"Stacks match"
										(match stack_delta with
											| Keep -> stack
											| Pop -> [ Regular ]
											| Push x -> x :: stack) stack';
									Assert.equal ~msg:"Matching ok"
										(match classification with
											| CInit -> [ Init op2 ]
											| CWrap -> [ Wrap op2 ]
											| CPair -> [ Pair(op1, op2) ]) matching))

let tests_adapt_first_stack_and_extend_matching =
	make_test_adapt_first_stack_and_extend_matching [
		("MatchSimple", MatchSimple, false, Keep, CPair);
		("MatchPush", MatchPush Regular, false, Push Regular, CPair);
		("MatchPop", MatchPop, false, Pop, CPair);
		("Initialization", Initialization, true, Keep, CInit);
		("WrapperSimple", WrapperSimple, true, Keep, CWrap);
		("WrapperPush", WrapperPush Regular, true, Push Regular, CWrap);
		("WrapperPop", WrapperPop, true, Pop, CWrap)
		]

let matching_state1 = {
	rt1 = dummy_rt;
	rt2 = dummy_rt;
	facts1 = state1_facts;
	facts2 = state1_facts;
	objeq = IntIntMap.empty;
	initialisation_data = VersionReferenceSet.empty;
  toString_data = []
}

let matching_state2 = {
	matching_state1 with
	initialisation_data =
		VersionReferenceSet.add ref1 VersionReferenceSet.empty
}

module VRSetFormat = FormatHelper.SetFormat(VersionReferenceSet)
let pp_vrset = VRSetFormat.pp_print_gen_set Reference.pp_versioned_reference

let check_matching_state id state =
	Assert.same ~msg:"Same rt1" matching_state1.rt1 state.rt1;
	Assert.same ~msg:"Same rt2" matching_state1.rt2 state.rt2;
	Assert.same ~msg:"Same facts1" matching_state1.facts1 state.facts1;
	Assert.same ~msg:"Same facts2" matching_state1.facts2 state.facts2;
	Assert.same ~msg:"Same objeq" matching_state1.objeq state.objeq;
	Assert.make_equal ~msg:"Same initialisation data"
		VersionReferenceSet.equal
		(Misc.to_string pp_vrset)
		id state.initialisation_data;
  Assert.same ~msg:"Same toString data" matching_state1.toString_data state.toString_data

let addref = VersionReferenceSet.add
let vrset = VersionReferenceSet.empty

let test_pid1 =
	Test.make_simple_test
		~title:"Perpetuate initialisation data: write"
		(fun () ->
					let id =
						vrset
						|> addref ref1
						|> addref ref2 in
					check_matching_state id
						(perpetuate_initialisation_data matching_state2
								(RWrite { ref = ref2; oldref = ref1; value = val1; success = true })))

let test_pid2 =
	Test.make_simple_test
		~title:"Perpetuate initialisation data: write of non-id object"
		(fun () ->
					let id =
						vrset
						|> addref ref1 in
					check_matching_state id
						(perpetuate_initialisation_data matching_state2
								(RWrite { ref = ref2; oldref = ref3; value = val1; success = true })))

let test_pid3 =
	Test.make_simple_test
		~title:"Perpetuate initialisation data: literal"
		(fun () ->
					let id =
						vrset
						|> addref ref3 in
					check_matching_state id
						(perpetuate_initialisation_data matching_state1
								(RLiteral { value = obj1; hasGetterSetter = false })))

let test_pid4 =
	Test.make_simple_test
		~title:"Perpetuate initialisation data: other"
		(fun () ->
					check_matching_state matching_state2.initialisation_data
						(perpetuate_initialisation_data matching_state2
								(RScriptEnter)))

let tests_pid = [test_pid1; test_pid2; test_pid3; test_pid4]

let tests_adapt_matching_state =
	List.map (fun (title, op, should_perpetuate) ->
					Test.make_simple_test ~title:("adapt_matching_state -- " ^ title) (fun () ->
									let id =
										if should_perpetuate then
											addref ref2 matching_state2.initialisation_data
										else
											matching_state2.initialisation_data in
									check_matching_state id
										(adapt_matching_state op
												RScriptEnter
												(RWrite { ref = ref2; oldref = ref1; value = val1; success = true })
                        matching_state2)))
		[ ("MatchSimple", MatchSimple, false);
		("MatchPop", MatchPop, false);
		("MatchPush", MatchPush Regular, false);
		("WrapSimple", WrapperSimple, true);
		("WrapPop", WrapperPop, true);
		("WrapPush", WrapperPush Regular, true);
		("Initialization", Initialization, true) ]

let test_adapt_matching_state_toString = 
  Test.make_simple_test ~title:"adapt_matching_state - toString handling"
   (fun () ->
    Assert.equal [val1]
      (adapt_matching_state MatchSimple
        (RRead { ref = (reference_of_fieldref (0, "toString"), 0); value = val1 })
        RScriptEnter state1).toString_data
    )
(* Candidate creation tests. This is quite extensive because of the many
 * required cases. Note that we use our knowledge of match_operation
 * being (reasonably) correct here.
 *
 * Format:
 * Title, op1, op2, T1, T2, R1, R2, W1, W2, S1, S2, E1, E2
 * where xy stands for "in mode x, with  state y".
 * Modes: T = toplevel, R = regular, W = wrap, S = toString, E = external
 * States: matching_state{y} is used.
 *
 * Each set is a list of operations. *)

let simple_expectation msg init op opneg =
	let top = if init then [ MatchSimple; Initialization ] else [ MatchSimple ]
	and top' = if init then [ Initialization ] else [] in
	[(msg ^ ", matching", op, op, top, top,
		[ MatchSimple ], [ MatchSimple ],
		[ WrapperSimple ], [ WrapperSimple ],
		[ WrapperSimple ], [ WrapperSimple ],
		[], []);
	(msg ^ ", non-matching", opneg, op, top', top',
		[], [],
		[ WrapperSimple ], [ WrapperSimple ],
		[ WrapperSimple ], [ WrapperSimple ],
		[], [])]

let make_simple_expectations exps =
	List.map
		(fun (msg, init, op1, op2) -> simple_expectation msg init op1 op2)
		exps |> List.flatten

(* This excludes rwrite, rfunpre, rfunpost and rfunexit *)
let simple_expectations = make_simple_expectations [
  ("RLiteral", true,
   RLiteral { value=val1; hasGetterSetter = true },
  RLiteral { value=val2; hasGetterSetter = false });
	("RForIn", true, RForIn obj1, RForIn val1);
	("RLocal", true, RLocal { name = "x"; ref =ref1 }, RLocal { name = "y"; ref=ref2 });
	("RAlias", true,
	RAlias { name="x"; source=Argument 0; ref=ref1},
	RAlias { name = "y"; source=Argument 1; ref=ref2 });
	("RRead", true, RRead { ref=ref1; value=val1 }, RRead { ref=ref2; value=val2 });
  ("RWith", true, RWith val1, RWith val2);
	("RScriptEnter", true, RScriptEnter, RScriptExit);
	("RScriptExit", true, RScriptExit, RScriptEnter);
	("RScriptExc", true, RScriptExc val1, RScriptExc val2);
	("RBinary", true,
	 RBinary { op="+"; left=obj1; right=obj2; result=val1 },
	 RBinary { op="*"; left=obj1; right=obj2; result=val1 });
	("RUnary", true,
	 RUnary { op = "-"; arg=val1; result=val1 },
	 RUnary { op = "!"; arg=val1; result=val1 });
	("REndExpression", true, REndExpression, RScriptEnter);
	("RConditional", true, RConditional val1, RConditional val2);
]

let simple_non_toplevel_expectations =
  List.map (fun (msg, op, opneg) ->
     [(msg ^ ", matching", op, op, [], [],
		[ MatchSimple ], [ MatchSimple ],
		[ WrapperSimple ], [ WrapperSimple ],
		[ WrapperSimple ], [ WrapperSimple ],
		[], []);
	(msg ^ ", non-matching", opneg, op, [], [],
		[], [],
		[ WrapperSimple ], [ WrapperSimple ],
		[ WrapperSimple ], [ WrapperSimple ],
		[], [])])
  [
	("RReturn", RReturn val1, RReturn val2);
  ("RFunEnter",
   RFunEnter { f = func1; this = obj1; args = obj2 },
   RFunEnter { f = func2; this = obj1; args = obj1 })
  ] |> List.flatten

let rthrow_expectations =
  [("RThrow, matching",
  RThrow val1, RThrow val1,
  [ MatchSimple ], [ MatchSimple ],
  [ MatchSimple ], [ MatchSimple ],
  [], [],
  [], [],
  [], []);
  ("RThrow, non-matching",
  RThrow val1, RThrow val2,
  [], [],
  [], [],
  [], [],
  [], [],
  [], [])]

let gref1 = (Reference.reference_of_name false StringMap.empty true "a", 0)
let gref2 = (Reference.reference_of_name false StringMap.empty true "b", 0)

let rwrite_expectations = [
  ("RWrite, matching, local, non-init, non-toString",
   RWrite { ref = ref3; oldref = ref4; value = val1; success = true },
   RWrite { ref = ref3; oldref = ref4; value = val1; success = true },
   [ MatchSimple ], [ MatchSimple ],
   [ MatchSimple ], [ MatchSimple ],
   [], [],
   [], [],
   [], []);
  ("RWrite, non-matching, local, non-init, non-toString",
   RWrite { ref = ref3; oldref = ref4; value = val1; success = true },
   RWrite { ref = ref3; oldref = ref4; value = val1; success = false },
   [], [],
   [], [],
   [], [],
   [], [],
   [], []);
  ("RWrite, matching, global, non-toString",
   RWrite { ref = gref1; oldref = gref2; value = val1; success = true },
   RWrite { ref = gref1; oldref = gref2; value = val1; success = true },
   [ MatchSimple ], [ MatchSimple ],
   [ MatchSimple ], [ MatchSimple ],
   [], [],
   [], [],
   [], []);
  ("RWrite, non-matching, global, non-toString",
   RWrite { ref = gref1; oldref = gref2; value = val1; success = true },
   RWrite { ref = gref1; oldref = gref2; value = val1; success = false },
   [], [],
   [], [],
   [], [],
   [], [],
   [], []);
  ("RWrite, matching, local, init, non-toString",
   RWrite { ref = ref2; oldref = ref1; value = val1; success = true },
   RWrite { ref = ref2; oldref = ref1; value = val1; success = true },
   [ MatchSimple ], [ MatchSimple; Initialization ],
   [ MatchSimple ], [ MatchSimple ],
   [], [ WrapperSimple ],
   [], [],
   [], []);
  ("RWrite, non-matching, local, init, non-toString",
   RWrite { ref = ref2; oldref = ref1; value = val1; success = true },
   RWrite { ref = ref2; oldref = ref1; value = val1; success = false },
   [], [ Initialization ],
   [], [],
   [], [ WrapperSimple ],
   [], [],
   [], []);
  ("RWrite, matching, local, toString",
   RWrite { ref = ref5; oldref = ref6; value = val1; success = true },
   RWrite { ref = ref5; oldref = ref6; value = val1; success = true },
   [ MatchSimple; Initialization ], [ MatchSimple; Initialization ],
   [ MatchSimple ], [ MatchSimple ],
   [ WrapperSimple ], [ WrapperSimple ],
   [], [],
   [], []);
  ("RWrite, non-matching, local, toString",
   RWrite { ref = ref5; oldref = ref6; value = val1; success = true },
   RWrite { ref = ref5; oldref = ref6; value = val1; success = false },
   [ Initialization ], [ Initialization ],
   [], [],
   [ WrapperSimple ], [ WrapperSimple ],
   [], [],
   [], []);
   (* TODO we're missing global toString detection here! *)
]

let rfunpost_expectations = [
  ("RFunPost, matching",
   RFunPost { f = func1; base = val1; args = val2; result = obj1 },
   RFunPost { f = func1; base = val1; args = val2; result = obj1 },
   [], [],
   [ MatchPop ], [ MatchPop ],
   [ WrapperPop ], [ WrapperPop ],
   [ WrapperPop ], [ WrapperPop ],
   [ MatchPop ], [ MatchPop ]);
  ("RFunPost, non-matching",
   RFunPost { f = func1; base = val1; args = val2; result = obj1 },
   RFunPost { f = func2; base = val1; args = val2; result = obj1 },
   [], [],
   [], [],
   [ WrapperPop ], [ WrapperPop ],
   [ WrapperPop ], [ WrapperPop ],
   [], []);
]

let rfunpre_expectations = [
  ("RFunPre, equivalent, local",
   RFunPre { f = func1; base = obj1; args = obj2; call_type = Method },
   RFunPre { f = func1; base = obj1; args = obj2; call_type = Method },
   [ MatchPush Regular ], [ MatchPush Regular ],
   [ MatchPush Regular ], [ MatchPush Regular ],
   [ WrapperPush Regular; WrapperPush Wrapper ], [ WrapperPush Regular; WrapperPush Wrapper ],
   [ WrapperPush ToString ], [ WrapperPush ToString ],
   [], []);
  ("RFunPre, possibly equivalent, local",
   RFunPre { f = func1; base = obj1; args = obj2; call_type = Method },
   RFunPre { f = func3; base = obj1; args = obj2; call_type = Method },
   [ MatchPush Wrapper ], [ MatchPush Wrapper ],
   [ MatchPush Wrapper ], [ MatchPush Wrapper ],
   [ WrapperPush Regular; WrapperPush Wrapper ], [ WrapperPush Regular; WrapperPush Wrapper ],
   [ WrapperPush ToString ], [ WrapperPush ToString ],
   [], []);
  ("RFunPre, not equivalent, local",
   RFunPre { f = func1; base = obj1; args = obj2; call_type = Method },
   RFunPre { f = func1; base = obj1; args = obj2; call_type = Constructor },
   [], [],
   [], [],
   [ WrapperPush Regular; WrapperPush Wrapper ], [ WrapperPush Regular; WrapperPush Wrapper ],
   [ WrapperPush ToString ], [ WrapperPush ToString ],
   [], []);
  ("RFunPre, equivalent, global",
   RFunPre { f = func2; base = obj1; args = obj2; call_type = Method },
   RFunPre { f = func2; base = obj1; args = obj2; call_type = Method },
   [ MatchPush External ], [ MatchPush External ],
   [ MatchPush External ], [ MatchPush External ],
   [], [],
   [], [],
   [], []);
  ("RFunPre, not equivalent, global",
   RFunPre { f = func2; base = obj1; args = obj2; call_type = Method },
   RFunPre { f = func2; base = obj1; args = obj2; call_type = Constructor },
   [], [],
   [], [],
   [], [],
   [], [],
   [], [])
]
  
let rfunexit_expectation = [
  ("RFunExit, matching",
   RFunExit { ret = val1; exc = OUndefined },
   RFunExit { ret = val1; exc = OUndefined },
   [], [],
   [ MatchSimple ], [ MatchSimple ],
   [ WrapperSimple ], [ WrapperSimple ],
   [ WrapperSimple ], [ WrapperSimple ],
   [], []);
  ("RFunExit, non-matching",
   RFunExit { ret = val1; exc = OUndefined },
   RFunExit { ret = val1; exc = ONull },
   [], [],
   [], [],
   [ WrapperSimple ], [ WrapperSimple ],
   [ WrapperSimple ], [ WrapperSimple ],
   [], [])  
] 

let all_expectations =
  simple_expectations @
  simple_non_toplevel_expectations @
  rthrow_expectations @
  rwrite_expectations @
  rfunpost_expectations @
  rfunpre_expectations @
  rfunexit_expectation

let pp_mode pp = function
  | Regular -> Format.pp_print_string pp "regular"
  | Wrapper -> Format.pp_print_string pp "wrapper"
  | ToString -> Format.pp_print_string pp "toString"
  | External -> Format.pp_print_string pp "External"
  
let pp_op pp = function
  | MatchPop -> Format.pp_print_string pp "match-pop"
  | MatchSimple -> Format.pp_print_string pp "match"
  | MatchPush s -> Format.fprintf pp "match-push %a" pp_mode s
  | WrapperSimple -> Format.pp_print_string pp "wrap"
  | WrapperPush s -> Format.fprintf pp "wrap-push %a" pp_mode s
  | WrapperPop -> Format.pp_print_string pp "wrap-pop"
  | Initialization -> Format.pp_print_string pp "init"
   
let list_same_contents fmt =
  Assert.make_equal
    (fun l1 l2 -> List.length l1 = List.length l2 &&
     List.for_all (fun x -> List.mem x l1) l2)
    (to_string (FormatHelper.pp_print_list fmt))
    
let make_candidate_tests_helper cgname tcname candgen state op1 op2 exp =
  Test.make_simple_test ~title:(tcname ^ " -- " ^ cgname)
    (fun () ->
      let (got, _) = candgen state op1 op2 in
      list_same_contents pp_op exp got)

let make_candidate_test
  (tcname, op1, op2, exp_top_1, exp_top_2, exp_gen_1, exp_gen_2,
  exp_wrap_1, exp_wrap_2, exp_ts_1, exp_ts_2, exp_ext_1, exp_ext_2) = [
    make_candidate_tests_helper "Top 1"
      tcname toplevel_candidates matching_state1 op1 op2 exp_top_1;
    make_candidate_tests_helper "Top 2"
      tcname toplevel_candidates matching_state2 op1 op2 exp_top_2;
    make_candidate_tests_helper "Regular 1"
      tcname regular_candidates matching_state1 op1 op2 exp_gen_1;
    make_candidate_tests_helper "Regular 2"
      tcname regular_candidates matching_state2 op1 op2 exp_gen_2;
    make_candidate_tests_helper "Wrapper 1"
      tcname wrap_candidates matching_state1 op1 op2 exp_wrap_1;
    make_candidate_tests_helper "Wrapper 2"
      tcname wrap_candidates matching_state2 op1 op2 exp_wrap_2;
    make_candidate_tests_helper "ToString 1"
      tcname toString_candidates matching_state1 op1 op2 exp_ts_1;
    make_candidate_tests_helper "ToString 2"
      tcname toString_candidates matching_state2 op1 op2 exp_ts_2;
    make_candidate_tests_helper "External 1"
      tcname external_candidates matching_state1 op1 op2 exp_ext_1;
    make_candidate_tests_helper "External 2"
      tcname external_candidates matching_state2 op1 op2 exp_ext_2
    ]

let candidate_tests =
  List.map make_candidate_test all_expectations |> List.flatten

(* The remaining functions are tested in their own file. *)
    
let () =
   Test.run_tests
		(operation_classification_tests @
			[match_source_test] @
			call_tests @ test_match_operations_cross @
			test_is_matching_toString_call ::
			tests_adapt_first_stack_and_extend_matching @
			tests_pid @ tests_adapt_matching_state @
      test_adapt_matching_state_toString :: candidate_tests
		);;
