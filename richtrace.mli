type alias_source = Argument of int | With of Reference.versioned_reference
type rfunpre = {
  f : Trace.objid;
  base : Trace.objid;
  args : Trace.objid;
  call_type : Cleantrace.call_type;
}
type rfunpost = {
  f : Trace.objid;
  base : Trace.objid;
  args : Trace.objid;
  result : Trace.objid;
}
type rliteral = { value : Trace.objid; hasGetterSetter : bool; }
type rlocal = { name : string; ref : Reference.versioned_reference; }
type ralias = {
  name : string;
  source : alias_source;
  ref : Reference.versioned_reference;
}
type rread = { ref : Reference.versioned_reference; value : Trace.objid; }
type rwrite = {
  ref : Reference.versioned_reference;
  oldref: Reference.versioned_reference;
  value : Trace.objid;
  success : bool;
}
type rbinary = {
  op : string;
  left : Trace.objid;
  right : Trace.objid;
  result : Trace.objid;
}
type runary = { op : string; arg : Trace.objid; result : Trace.objid; }
type rfunenter = { f : Trace.objid; this : Trace.objid; args : Trace.objid; }
type rfunexit = { ret : Trace.objid; exc : Trace.objid; }
type rich_operation =
    RFunPre of rfunpre
  | RFunPost of rfunpost
  | RLiteral of rliteral
  | RForIn of Trace.objid
  | RLocal of rlocal
  | RAlias of ralias
  | RRead of rread
  | RWrite of rwrite
  | RReturn of Trace.objid
  | RThrow of Trace.objid
  | RWith of Trace.objid
  | RFunEnter of rfunenter
  | RFunExit of rfunexit
  | RScriptEnter
  | RScriptExit
  | RScriptExc of Trace.objid
  | RBinary of rbinary
  | RUnary of runary
  | REndExpression
  | RConditional of Trace.objid
type rich_trace = (rich_operation * LocalFacts.local_facts) list
type rich_tracefile = {
  funcs : Trace.functions;
  objs : Trace.objects;
  trace : rich_trace;
  globals : Trace.globals;
  globals_are_properties : bool;
  points_to : PointsTo.points_to_map;
}
val pp_alias_source : Format.formatter -> alias_source -> unit
val pp_rich_operation : Format.formatter -> rich_operation -> unit
val pp_rich_trace : Format.formatter -> rich_trace -> unit
val pp_rich_tracefile : Format.formatter -> rich_tracefile -> unit
val calculate_rich_tracefile : Trace.tracefile -> rich_tracefile
