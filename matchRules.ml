open MatchTypes

type match_rule = match_condition list * match_operation
type match_rules = match_rule list

let rules_toplevel =
  [
    ([MatchSides; MayMatchSimple; IsToplevel], MatchSimple);
    ([MatchSides; MatchCallInt], MatchPush RegularEnter);
    ([MatchSides; MatchCallExt], MatchPush RegularEnter);
    ([MatchCallToString], MatchPush ToString);
    ([MatchSides; MatchCallWrap], MatchPush WrapperEnter);
    ([MayInit; IsToplevel; IsNotFunction], Initialization);
    ([IsCallInt (* TODO add "local function" check *)], InitializationPush Init)
  ]

let rules_regular =
  [
    ([MatchSides; MayMatchSimple], MatchSimple);
    ([MatchSides; MatchCallInt], MatchPush RegularEnter);
    ([MatchSides; MatchCallExt], MatchPush RegularEnter);
    ([MatchCallToString], MatchPush ToString);
    ([MatchSides; MatchCallWrap], MatchPush Wrapper);
    ([MatchSides; IsExit], MatchPop);
    ([UseStrictRHS], MatchDroppable);
    ([MatchSides; IsFunLiteral], MatchPush IndirectDefinitionPattern);
    ([IsFunLiteral], WrapperPush ExtraFunctionPattern);
    ([IsFunRead], WrapperPush ToStringUpdatePattern);
    ([IsAliasMatch], MatchReplace AliasMatchPattern);
  ]

let rules_alias_match_pattern =
  [
    ([MatchAliasWrites], MatchReplace Regular)
  ]

let rules_regular_enter =
  [
    ([MatchEnter], MatchReplace Regular);
    ([MatchSides; IsPostExit], MatchPop);
    ([MatchSides; IsCatch], MatchPop);
  ]

let rules_wrapper_enter =
  [
    ([IsEnter], WrapperReplace Wrapper);
    ([IsPostExit], WrapperPop);
    ([IsCatch], WrapperPop);
  ]

let rules_wrap =
  [
    ([IsCallInt], WrapperPush RegularEnter);
    ([IsCallInt], WrapperPush WrapperEnter);
    ([IsExit], WrapperPop);
    ([MayInsertInWrapSimple], WrapperSimple)
  ]

let rules_toString =
  [
    ([IsCallInt], WrapperPush ToString);
    ([IsExit], WrapperPop);
    ([IsUnobservable], WrapperSimple)
  ]

let rules_external =
  [ ([MatchSides; IsExit], MatchPop) ]

let rules_init =
  [ ([MayInit], Initialization);
    ([IsCallInt], InitializationPush Init);
    ([IsExit], InitializationPop) ]

let rules_indirect_definition =
  [ ([IsCallInt], WrapperPush Wrapper);
    ([IsPostExit], WrapperPop) ]

let rules_extra_function =
  [ ([IsFunLiteral], WrapperSimple);
    ([IsLocalDecl], WrapperSimple);
    ([MayInit], WrapperPop);
  ]

let rules_tostring_update =
  [ ([MayInit], WrapperSimple);
    ([IsCallInt], WrapperPush Init);
    ([IsEndOfExpr], WrapperPop)
  ]

let find_rules = function
  | InToplevel -> rules_toplevel
  | InRegular -> rules_regular
  | InRegularEnter -> rules_regular_enter
  | InWrap -> rules_wrap
  | InToString -> rules_toString
  | InExternal -> rules_external
  | InInit -> rules_init
  | InWrapperEnter -> rules_wrapper_enter
  | InIndirectDefinitionPattern -> rules_indirect_definition
  | InExtraFunctionPattern -> rules_extra_function
  | InToStringUpdatePattern -> rules_tostring_update
  | InAliasMatchPattern -> rules_alias_match_pattern
