open Richtrace
open MatchTypes

val is_unobservable : rich_operation -> bool
val is_toplevel : rich_operation -> bool
val is_throw : rich_operation -> bool
val is_write : rich_operation -> bool
val is_exit : rich_operation -> bool
val is_post_exit : rich_operation -> bool
val is_enter : rich_operation -> bool
val is_use_strict : rich_operation -> bool
val is_not_function : rich_operation -> bool
val is_catch : rich_operation -> bool

val explain: mismatch -> bool -> mismatch option

type 'a comparator = matching_state -> 'a -> 'a -> objeq * mismatch option
type predicate = matching_state -> rich_operation -> mismatch option
type call_comparator = matching_state -> rich_operation -> rich_operation -> mismatch option
type simple_predicate = rich_operation -> mismatch option

val is_internal_call_impl : rich_tracefile -> int -> mismatch option
val is_internal_call : rich_tracefile -> rich_operation -> mismatch option

val match_operations : rich_operation comparator
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
val is_matching_entry: rich_operation comparator
val is_fun_literal : simple_predicate
val is_local_decl : simple_predicate
val is_fun_read : simple_predicate
val is_end_of_expr : simple_predicate