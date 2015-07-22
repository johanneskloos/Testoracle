open Trace

type call_type = Function | Method | Constructor | ConstructorMethod
type declaration_type =
  | Var
  | ArgumentArray
  | ArgumentBinding of int
  | CatchParam
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

type clean_trace = clean_operation list
type clean_tracefile = functions * objects * clean_trace * globals * bool
val pp_clean_operation : Format.formatter -> clean_operation -> unit
val pp_clean_trace : Format.formatter -> clean_trace -> unit
val pp_clean_tracefile : Format.formatter -> clean_tracefile -> unit
val clean_trace: trace -> clean_trace
val clean_tracefile: tracefile -> clean_tracefile
