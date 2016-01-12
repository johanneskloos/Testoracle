open MatchTypes

type match_rule = match_condition list * match_operation
type match_rules = match_rule list

val find_rules: match_state -> match_rules