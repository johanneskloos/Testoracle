(** * A nicer form of trace, with more uniform events. *)

(** This contains an explanation of where an alias comes from. *)
type alias_source = Argument of int | With of Reference.versioned_reference

(** Structures that sum up data about certain operations. *)
type rfunpre = {
    f : Trace.jsval;
    base : Trace.jsval;
    args : Trace.jsval;
    call_type : Cleantrace.call_type;
}
type rfunpost = {
    f : Trace.jsval;
    base : Trace.jsval;
    args : Trace.jsval;
    result : Trace.jsval;
}
type rliteral = { value : Trace.jsval; hasGetterSetter : bool; }
type rlocal = { name : string; ref : Reference.versioned_reference; }
type ralias = {
    name : string;
    source : alias_source;
    ref : Reference.versioned_reference;
}
type rread = { ref : Reference.versioned_reference; value : Trace.jsval; }
type rwrite = {
    ref : Reference.versioned_reference;
    oldref: Reference.versioned_reference;
    value : Trace.jsval;
    success : bool;
}
type rbinary = {
    op : string;
    left : Trace.jsval;
    right : Trace.jsval;
    result : Trace.jsval;
}
type runary = { op : string; arg : Trace.jsval; result : Trace.jsval; }
type rfunenter = { f : Trace.jsval; this : Trace.jsval; args : Trace.jsval; }
type rfunexit = { ret : Trace.jsval; exc : Trace.jsval; }
(** Events that make use of the facts calculated by the [LocalFacts] module
* and consorts to provide a better representation for trace comparison.
* Compare with [clean_operation], and note that variable and field accessed
* have been unified to [RRead] and [RWrite], while [CDeclare] has been split
* into [RAlias] and [RLocal]. *)
type rich_operation =
        RFunPre of rfunpre
    | RFunPost of rfunpost
    | RLiteral of rliteral
    | RForIn of Trace.jsval
    | RLocal of rlocal
    | RCatch of rlocal
    | RAlias of ralias
    | RRead of rread
    | RWrite of rwrite
    | RReturn of Trace.jsval
    | RThrow of Trace.jsval
    | RWith of Trace.jsval
    | RFunEnter of rfunenter
    | RFunExit of rfunexit
    | RScriptEnter
    | RScriptExit
    | RScriptExc of Trace.jsval
    | RBinary of rbinary
    | RUnary of runary
    | REndExpression
    | RConditional of Trace.jsval

(** A rich trace contains rich operations and local facts. *)
type rich_trace = (rich_operation * LocalFacts.local_facts) list
(** A rich trace file contains the original function and object descriptions,
* as well as global object information and the [globals_are_properties] flag.
* Furthermore, it contains a rich trace and a points - to map for the references
* occuring in the program. *)
type rich_tracefile = {
    funcs : Trace.functions;
    objs : Trace.objects;
    trace : rich_trace;
    globals : Trace.globals;
    globals_are_properties : bool;
    points_to : PointsTo.points_to_map;
}
(** Pretty-printers. *)
val pp_alias_source : Format.formatter -> alias_source -> unit
val pp_rich_operation : Format.formatter -> rich_operation -> unit
val pp_rich_trace : Format.formatter -> rich_trace -> unit
val pp_rich_tracefile : Format.formatter -> rich_tracefile -> unit
val dump_facts: bool ref
(** Transform a trace file to a rich trace file. *)
val calculate_rich_tracefile : Trace.tracefile -> rich_tracefile
