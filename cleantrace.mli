open Trace

(** * Cleaned-up traces. *)
(** A cleaned-up trace is a version of a trace that has
 * unneccesary detail removed, and contains proper information
 * on whether a variable access goes to a global or a local variable. *)

(** Classification of the different types of function calls. This just enumerates
 * the combinations of flags. *)
type call_type = Function | Method | Constructor | ConstructorMethod

(** The different possible types of variable declarations. *)
type declaration_type =
  | Var (** A regular local variable. *)
  | ArgumentArray (** The argument array [arguments] that is implicitly bound when entering a function. *)
  | ArgumentBinding of int (** The variable binds to the [i]-th argument. Note that this is an alias. *)
  | CatchParam (** The variable binds a caught exception. *)
(** Structures that contain information about the different possible events on a trace.
 * 
 * These structures are pruned-down versions of those in [Trace].
 *)
type funpre = {
  f : jsval;
  base : jsval;
  args : jsval;
  call_type: call_type
}
type funpost = {
  f : jsval;
  base : jsval;
  args : jsval;
  result : jsval;
  call_type: call_type
}
type literal = { value : jsval; hasGetterSetter : bool; }
type declare = {
  name : string;
  value : jsval;
  declaration_type: declaration_type
}
type accessfield = {
  base : jsval;
  offset : string;
  value : jsval
}
type read = {
  name : string;
  value : jsval;
  isGlobal: bool
}
type write = {
  name : string;
  lhs : jsval;
  value : jsval;
  isGlobal : bool;
  isSuccessful: bool
}
type binary = {
  op : string;
  left : jsval;
  right : jsval;
  result : jsval
}
type unary = { op : string; arg : jsval; result : jsval; }
type funenter = { f : jsval; this : jsval; args : jsval; }
type funexit = { ret : jsval; exc : jsval; }

(** Events that can occur in a cleaned-up trace. Note that certain
 * events in a [Trace.trace] are redundant for out task, so we drop them. *)
type clean_operation =
  | CFunPre of funpre
  | CFunPost of funpost
  | CLiteral of literal
  | CForIn of jsval
  | CDeclare of declare
  | CGetField of accessfield
  | CPutField of accessfield
  | CRead of read
  | CWrite of write
  | CReturn of jsval
  | CThrow of jsval
  | CWith of jsval
  | CFunEnter of funenter
  | CFunExit of funexit
  | CScriptEnter
  | CScriptExit
  | CScriptExc of jsval
  | CBinary of binary
  | CUnary of unary
  | CEndExpression
  | CConditional of jsval

(** A clean trace is a list of cleaned-up events. *)
type clean_trace = clean_operation list
(** A clean trace file is like a trace file, only it contains a clean trace. *)
type clean_tracefile = functions * objects * clean_trace * globals * bool
(** Pretty-printers. *)
val pp_call_type : Format.formatter -> call_type -> unit
val pp_clean_operation : Format.formatter -> clean_operation -> unit
val pp_clean_trace : Format.formatter -> clean_trace -> unit
val pp_clean_tracefile : Format.formatter -> clean_tracefile -> unit
(** Transform a trace to a clean trace. *)
val clean_trace: trace -> clean_trace
(** Transform a trace file to a clean trace file. *)
val clean_tracefile: tracefile -> clean_tracefile
