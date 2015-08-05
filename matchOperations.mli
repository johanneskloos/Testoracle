open Richtrace

val is_unobservable : rich_operation -> bool
val is_toplevel : rich_operation -> bool
val is_throw : rich_operation -> bool
val is_write : rich_operation -> bool
val is_exit : rich_operation -> bool
val is_post_exit : rich_operation -> bool

type matching_state = {
  rt1 : rich_tracefile;
  rt2 : rich_tracefile;
  facts1 : LocalFacts.local_facts;
  facts2 : LocalFacts.local_facts;
  objeq : MatchObjects.objeq;
  initialisation_data : Reference.VersionReferenceSet.t;
  toString_data : Trace.jsval list;
}
type mismatch =
  | DifferentType
  | DifferentObjects of string * MatchObjects.obj_match_failure
  | DifferentArguments
  | DifferentValues of string
  | DifferentOperations
  | OtherOperation
  | NotToString
  | NotInitData
  | NotFunctionUpdate
  | NotInitCode
  | NotSimpleMatchable
  | NotWrapCode
  | NotToStringCode
  | ExternalCall
  | InternalCall
  | NotLiterallyEqual
  | LiterallyEqual
  | NotToplevel
  | NotFunction
  | NotExit
  | Observable
  | NotAtToplevel

val explain: mismatch -> bool -> mismatch option

type 'a comparator = matching_state -> 'a -> 'a -> MatchObjects.objeq * mismatch option
type predicate = matching_state -> rich_operation -> mismatch option
type call_comparator = matching_state -> rich_operation -> rich_operation -> mismatch option

val match_source : alias_source comparator
val match_operations : rich_operation comparator
val is_instrumentation_write : predicate
val is_function_update : predicate
val may_insert_in_init : predicate
val may_insert_in_matching_simple : rich_operation -> mismatch option
val may_insert_in_wrap_simple : predicate
val may_insert_in_toString_simple : rich_operation -> mismatch option
val is_internal_call_impl : rich_tracefile -> int -> mismatch option
val is_internal_call :
  rich_tracefile -> rich_operation -> mismatch option
val is_matching_internal_call : call_comparator
val is_matching_external_call : call_comparator
val is_matching_toString_call : matching_state -> Richtrace.rich_operation -> Richtrace.rich_operation -> mismatch option
val may_be_wrapper_entry : call_comparator
val is_not_function : rich_operation -> bool
