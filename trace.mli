type jsval =
    OUndefined
  | ONull
  | OBoolean of bool
  | ONumberInt of int
  | ONumberFloat of float
  | OString of string
  | OSymbol of string
  | OFunction of int * int
  | OObject of int
  | OOther of string * int
type funpre = {
  iid : int;
  f : jsval;
  base : jsval;
  args : jsval;
  isConstructor : bool;
  isMethod : bool;
}
type funpost = {
  iid : int;
  f : jsval;
  base : jsval;
  args : jsval;
  result : jsval;
  isConstructor : bool;
  isMethod : bool;
}
type literal = { iid : int; value : jsval; hasGetterSetter : bool; }
type value = { iid : int; value : jsval; }
type declare = {
  iid : int;
  name : string;
  value : jsval;
  argument : int option;
  isCatchParam : bool;
}
type getfieldpre = {
  iid : int;
  base : jsval;
  offset : string;
  isComputed : bool;
  isOpAssign : bool;
  isMethodCall : bool;
}
type getfieldpost = {
  iid : int;
  base : jsval;
  offset : string;
  value : jsval;
  isComputed : bool;
  isOpAssign : bool;
  isMethodCall : bool;
}
type putfield = {
  iid : int;
  base : jsval;
  offset : string;
  value : jsval;
  isComputed : bool;
  isOpAssign : bool;
}
type access = {
  iid : int;
  name : string;
  value : jsval;
  isGlobal : bool;
  isScriptLocal : bool;
}
type writeaccess = {
  iid : int;
  name : string;
  lhs : jsval;
  value : jsval;
  isGlobal : bool;
  isScriptLocal : bool;
}
type binpre = {
  iid : int;
  op : string;
  left : jsval;
  right : jsval;
  isOpAssign : bool;
  isSwitchCaseComparison : bool;
  isComputed : bool;
}
type binpost = {
  iid : int;
  op : string;
  left : jsval;
  right : jsval;
  result : jsval;
  isOpAssign : bool;
  isSwitchCaseComparison : bool;
  isComputed : bool;
}
type unpre = { iid : int; op : string; arg : jsval; }
type unpost = { iid : int; op : string; arg : jsval; result : jsval; }
type funenter = { iid : int; f : jsval; this : jsval; args : jsval; }
type funexit = { iid : int; ret : jsval; exc : jsval; }
type operation =
  | FunPre of funpre
  | FunPost of funpost
  | Literal of literal
  | ForIn of value
  | Declare of declare
  | GetFieldPre of getfieldpre
  | GetField of getfieldpost
  | PutFieldPre of putfield
  | PutField of putfield
  | Read of access
  | Write of writeaccess
  | Return of value
  | Throw of value
  | With of value
  | FunEnter of funenter
  | FunExit of funexit
  | ScriptEnter
  | ScriptExit
  | ScriptExc of jsval
  | BinPre of binpre
  | BinPost of binpost
  | UnaryPre of unpre
  | UnaryPost of unpost
  | EndExpression of int
  | Conditional of value
type trace = operation list
type fieldspec = {
    value: jsval;
    writable: bool;
    get: jsval option;
    set: jsval option;
    enumerable: bool;
    configurable: bool
}
type objectspec = fieldspec Misc.StringMap.t
type objects = objectspec array
type local_funcspec = { instrumented : string; uninstrumented : string }
type funcspec = Local of local_funcspec | External of int
type functions = funcspec array
type globals = jsval Misc.StringMap.t
type tracefile = functions * objects * trace * globals * bool
val parse_tracefile : in_channel -> tracefile
val dump_tracefile: out_channel -> tracefile -> unit
val pp_jsval : Format.formatter -> jsval -> unit
val pp_operation : Format.formatter -> operation -> unit
val pp_trace : Format.formatter -> operation list -> unit
val pp_fieldspec : Format.formatter -> fieldspec -> unit
val pp_objectspec : Format.formatter -> objectspec -> unit
val pp_objects : Format.formatter -> objects -> unit
val pp_local_funcspec : Format.formatter -> local_funcspec -> unit
val pp_funcspec : Format.formatter -> funcspec -> unit
val pp_functions : Format.formatter -> functions -> unit
val pp_globals : Format.formatter -> globals -> unit
val pp_tracefile : Format.formatter -> tracefile -> unit

exception NotAnObject
val get_object: jsval -> int

