open Types
(** * Structures that contain event data *)
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
type event =
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
type trace = event list

(** A trace file is a tuple containing the various components defined above. *)
type tracefile = functions * objects * trace * globals * bool
(** [parse_tracefile c] parses a JSON trace file from input channel [c] and returns it. *)
val parse_tracefile : in_channel -> tracefile
(** [dump_tracefile c t] dumps [t] as a JSON trace file to channel [c]. *)
val dump_tracefile: out_channel -> tracefile -> unit
(** Pretty printers. *)
val pp_operation : Format.formatter -> event -> unit
val pp_trace : Format.formatter -> event list -> unit
val pp_tracefile : Format.formatter -> tracefile -> unit
