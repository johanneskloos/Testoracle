open Kaputt.Abbreviations
open MatchTypes

let pp_match_state pp = let open Format in function
	| InToplevel -> pp_print_string pp "InToplevel"
	| InRegular -> pp_print_string pp "InRegular"
	| InRegularEnter -> pp_print_string pp "InRegularEnter"
	| InWrap -> pp_print_string pp "InWrap"
	| InToString -> pp_print_string pp "InToString"
	| InExternal -> pp_print_string pp "InExternal"
	| InInit -> pp_print_string pp "InInit"
	| InWrapperEnter -> pp_print_string pp "InWrapperEnter"
	| InIndirectDefinitionPattern -> pp_print_string pp "InIndirectDefinitionPattern"
	| InExtraFunctionPattern -> pp_print_string pp "InExtraFunctionPattern"
	| InToStringUpdatePattern -> pp_print_string pp "InToStringUpdatePattern"

let test_get_state_inner (name, stack, state) =
	Test.make_simple_test ~title:name (fun () ->
		Assert.make_equal (=) (Misc.to_string pp_match_state) (get_state stack) state)

let tests =(List.map test_get_state_inner [
		("get_state - InToplevel", [], InToplevel);
		("get_state - InRegular", [Regular], InRegular);
		("get_state - InRegularEnter", [RegularEnter], InRegularEnter);
		("get_state - InWrap", [Wrapper], InWrap);
		("get_state - InToString", [ToString], InToString);
		("get_state - InExternal", [External], InExternal);
		("get_state - InInit", [Init], InInit);
		("get_state - InWrapperEnter", [WrapperEnter], InWrapperEnter);
		("get_state - InIndirectDefinitionPattern", [IndirectDefinitionPattern], InIndirectDefinitionPattern);
		("get_state - InExtraFunctionPattern", [ExtraFunctionPattern], InExtraFunctionPattern);
		("get_state - InToStringUpdatePattern", [ToStringUpdatePattern], InToStringUpdatePattern);
		])
