open Richtrace
open MatchTypes

(** The trace matcher is built in two parts,
* the match candidate finder and the matching engine.
* They communicate using the following protocol:
* The matching engine calls the appropriate candidate finder
* for its current state with the head operations of both traces,
* and the candidate finder returns a list of matching operations
* that describe how the traces can be matched at this point.
*
* Example:
* Suppose we are in top - level mode, and the head operations
* are Read(ref1, "x") and Read(ref2, "x").
* If ref1 and ref2 match, then two operations are possible,
* namely MatchSimple, matching both operations together and continuing
* with the trail of the traces on both said, and
* Init, which treats the second operation as initialization code,
* and continues matching with the full trace on the left and the tail
* on the right.
*)

(** Get the operating state by examining the stack. *)

val get_state : mode list -> state

val add_objeq :
  rich_operation ->
  MatchObjects.objeq -> 'a -> 'a * MatchObjects.objeq

val interpret_rules :
  (condition list * match_operation) list ->
  MatchOperations.matching_state ->
  rich_operation ->
  rich_operation ->
  (match_operation list * MatchObjects.objeq) *
  ((condition * MatchOperations.mismatch) list * match_operation) list

val build_candidates :
  MatchOperations.matching_state ->
  rich_operation ->
  rich_operation ->
  state ->
  (match_operation list * MatchObjects.objeq) *
  ((condition * MatchOperations.mismatch) list * match_operation) list
  

(**
* Special - case handling for a trace ending in initialisation code.
* This is legal, but probably not very useful, except in the degenerate
* case of empty code.
*)
val can_be_added_as_initialisation :
  MatchOperations.matching_state ->
  (rich_operation * LocalFacts.local_facts) list ->
  mode list -> MatchOperations.mismatch option

(**
* Effects of the matching operations on various bits of state.
*
* [adapt_first op op1 facts1 trace1]
* computes the original code trace to use for further matching,
* given a matching operation [op].
*)
val adapt_first :
  match_operation -> 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list

(** [adapt_stack op stack]
* applies the required stack manipulation for [op]. *)
val adapt_stack : match_operation -> mode list -> mode list

(** [extend_matching op op1 op2 matching]
* extends the given matching according to [op]. *)
val extend_matching :
  match_operation ->
  rich_operation ->
  rich_operation -> match_type list -> match_type list

(** Collect the references belonging to an object value. *)
val collect_object_references :
  MatchOperations.matching_state -> int -> Reference.versioned_reference list
(** Collect the references belonging to a value. *)
val collect_references :
  MatchOperations.matching_state ->
  Trace.jsval -> Reference.versioned_reference list
(** Perpetuate initialisation-produced data. *)
val perpetuate_initialisation_data :
  MatchOperations.matching_state ->
  rich_operation -> MatchOperations.matching_state
(** Detect calls to "toString" *)
val detect_toString :
  rich_operation ->
  MatchOperations.matching_state -> MatchOperations.matching_state
(** Adapt the matching state according to the given operation *)
val adapt_matching_state :
  match_operation ->
  rich_operation ->
  rich_operation ->
  MatchOperations.matching_state -> MatchOperations.matching_state
(** The matching engine.
*
* It consists of two mutually - recursive functions:
* [matching_engine matching_state trace1 trace2 stack]
* handles the various cases of empty and non - empty traces
* [trace1] and [trace2], using
* [apply_first_working matching_state op1 op2 trace1 trace2 stack]
* to handle the list of operations generated by the candidate
* generator when there is an operation to match on both sides. *)
val matching_engine :
  MatchOperations.matching_state ->
  rich_trace -> rich_trace -> mode list -> match_type list option
val apply_first_working :
  int ->
  MatchOperations.matching_state ->
  rich_operation ->
  rich_operation ->
  rich_trace ->
  rich_trace ->
  mode list -> match_operation list -> match_type list option
(** The main entry point for trace matching. *)
val match_traces :
  rich_tracefile -> rich_tracefile -> match_type list option
