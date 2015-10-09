open Richtrace
open MatchTypes


type 'a comparator = matching_state -> 'a -> 'a -> mismatch option
type predicate = matching_state -> rich_event -> mismatch option
type call_comparator = rich_event comparator
type simple_predicate = rich_event -> mismatch option

val is_internal_call_impl : rich_tracefile -> int -> mismatch option
val is_internal_call : rich_tracefile -> rich_event -> mismatch option

val match_operations : rich_event comparator
val is_instrumentation_write : predicate
val is_function_update : predicate
val may_insert_in_init : predicate
val may_insert_in_matching_simple : simple_predicate
val may_insert_in_wrap_simple : predicate
val may_insert_in_toString_simple : simple_predicate
val is_matching_internal_call : call_comparator
val is_matching_external_call : call_comparator
val is_matching_toString_call : call_comparator
val may_be_wrapper_entry : call_comparator
val is_matching_entry: rich_event comparator
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

val interpret_cond : matching_state -> rich_event -> rich_event -> match_condition -> mismatch option