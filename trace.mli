(** A JavaScript value. *)
type jsval =
    OUndefined
  | ONull
  | OBoolean of bool
  | ONumberInt of int
  | ONumberFloat of float
  | OString of string
  | OSymbol of string
  | OFunction of int * int (** [OFunction(id, fid)] stands for a function belonging to object [id], with function id [fid]. *)
  | OObject of int (** [OObject id] stands for the object with object id [id]. *)
  | OOther of string * int (** [OOther (ty, id) stands for a special object with type [ty] and object id [id]. *)

(** * Structures that contain operation data *)
(** Thanks to these structures, we do not carry around huge tuples. The fields are exactly
 * the properties that can be read off from the JSON trace representation, unless otherwise noted. *)
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
(** The type of operations in a trace. This covers exactly the possible cases in the JSON trace. *)
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
(** A trace is a sequence of events. *)
type trace = operation list
(** The description of a field in an object. *)
type fieldspec = {
    value: jsval;
    writable: bool;
    get: jsval option;
    set: jsval option;
    enumerable: bool;
    configurable: bool
}
(** An object can be described by a description of all its fields.
 *
 * Note that complete knowledge of object fields is not always available;
 * this comes up in [MatchObjects]. *)
type objectspec = fieldspec Misc.StringMap.t
(** Description of all object initial states in the program. *)
type objects = objectspec array
(** Description of a local JavaScript function, i.e., a function that
  * consists of JavaScript code and not a native call.
  *
  * The two fields contain the instrumented and the uninstrumented code
  * of the function. In some cases, uninstrumented contains "(unknown)";
  * this happens when the code is outside of the Jalangi-instrumented
  * part of the program. 
  *
  * FIXME: Obviously, it would more sense to have a single strong
  * describing the function body, containing the uninstrumented code
  * only. The cases can be kept apart in parsing.
  *)
type local_funcspec = { instrumented : string; uninstrumented : string option }
(** Description of a Javascript function. It can either be local, with
  * a description as given above, or [External fid], with function
  * identifier [fid]. *)
type funcspec = Local of local_funcspec | External of int
(** Description of all Javascript functions in a trace. *)
type functions = funcspec array
(** Description of a global object. *)
type global_desc = {
  id: jsval;
  obj: objectspec;
  proto: objectspec
  }
(** The values of all (known) global variables. *)
type globals = global_desc Misc.StringMap.t
(** A trace file is a tuple containing the various components defined above. *)
type tracefile = functions * objects * trace * globals * bool
(** [parse_tracefile c] parses a JSON trace file from input channel [c] and returns it. *) 
val parse_tracefile : in_channel -> tracefile
(** [dump_tracefile c t] dumps [t] as a JSON trace file to channel [c]. *)
val dump_tracefile: out_channel -> tracefile -> unit
(** Pretty printers. *)
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

(** Indicated that an operation cannot be performed on some value because it does not have object nature. *)
exception NotAnObject
(** Get the object identifier of a given value, or throw [NotAnObject]. *)
val get_object: jsval -> int

