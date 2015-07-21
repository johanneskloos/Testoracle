open Trace

type call_type = Function | Method | Constructor | ConstructorMethod
type declaration_type =
  | Var
  | ArgumentArray
  | ArgumentBinding of int
  | CatchParam
type funpre = {
  f : objid;
  base : objid;
  args : objid;
  call_type: call_type
}
type funpost = {
  f : objid;
  base : objid;
  args : objid;
  result : objid;
  call_type: call_type
}
type literal = { value : objid; hasGetterSetter : bool; }
type declare = {
  name : string;
  value : objid;
  declaration_type: declaration_type
}
type accessfield = {
  base : objid;
  offset : string;
  value : objid
}
type read = {
  name : string;
  value : objid;
  isGlobal: bool
}
type write = {
  name : string;
  lhs : objid;
  value : objid;
  isGlobal : bool;
  isSuccessful: bool
}
type binary = {
  op : string;
  left : objid;
  right : objid;
  result : objid
}
type unary = { op : string; arg : objid; result : objid; }
type funenter = { f : objid; this : objid; args : objid; }
type funexit = { ret : objid; exc : objid; }
type clean_operation =
  | CFunPre of funpre
  | CFunPost of funpost
  | CLiteral of literal
  | CForIn of objid
  | CDeclare of declare
  | CGetField of accessfield
  | CPutField of accessfield
  | CRead of read
  | CWrite of write
  | CReturn of objid
  | CThrow of objid
  | CWith of objid
  | CFunEnter of funenter
  | CFunExit of funexit
  | CScriptEnter
  | CScriptExit
  | CScriptExc of objid
  | CBinary of binary
  | CUnary of unary
  | CEndExpression
  | CConditional of objid
type clean_trace = clean_operation list
type clean_tracefile = functions * objects * clean_trace * globals * bool
val pp_clean_operation : Format.formatter -> clean_operation -> unit
val pp_clean_trace : Format.formatter -> clean_trace -> unit
val pp_clean_tracefile : Format.formatter -> clean_tracefile -> unit
val clean_trace: trace -> clean_trace
val clean_tracefile: tracefile -> clean_tracefile
