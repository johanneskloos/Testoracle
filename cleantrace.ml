open Types

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
open Format
let pp_call_type pp = function
    | Function -> fprintf pp "function"
    | Method -> fprintf pp "method"
    | Constructor -> fprintf pp "constructor"
    | ConstructorMethod -> fprintf pp "constructor method"

let pp_declaration_type pp = function
  | Var -> pp_print_string pp "LocalVariable"
  | ArgumentArray -> pp_print_string pp "ArgumentArray"
  | ArgumentBinding i -> fprintf pp "Argument(%d)" i
  | CatchParam -> pp_print_string pp "CatchParameter"

let pp_clean_operation pp = function
    | CFunPre { f; base; args; call_type } ->
        fprintf pp "CFunPre(f=%a, base=%a, args=%a, call_type=%a)"
            pp_jsval f pp_jsval base pp_jsval args pp_call_type call_type
    | CFunPost { f; base; args; result; call_type } ->
        fprintf pp "CFunPost(f=%a, base=%a, args=%a, result=%a, call_type=%a"
            pp_jsval f pp_jsval base pp_jsval args
            pp_jsval result pp_call_type call_type
    | CLiteral { value; hasGetterSetter } ->
        fprintf pp "CLiteral(value=%a,hasGetterSetter=%b" pp_jsval value hasGetterSetter
    | CForIn value ->
        fprintf pp "CForIn(%a)" pp_jsval value
    | CDeclare { name; value; declaration_type } ->
      fprintf pp "CDeclare(name=%s, value=%a, declaration_type=%a"
        name pp_jsval value pp_declaration_type declaration_type
    | CGetField { base; offset; value } ->
        fprintf pp "CGetField(base=%a, offset=%s, value=%a)" pp_jsval base offset pp_jsval value
    | CPutField { base; offset; value } ->
        fprintf pp "CPutField(base=%a, offset=%s, value=%a)" pp_jsval base offset pp_jsval value
    | CRead { name; value; isGlobal } ->
        fprintf pp "CRead(name=%s, global=%B, value=%a)" name isGlobal pp_jsval value
    | CWrite { name; lhs; value; isGlobal; isSuccessful } ->
        fprintf pp "CWrite(name=%s, global=%B, oldValue=%a, newValue=%a, successful=%B)"
            name isGlobal pp_jsval lhs pp_jsval value isSuccessful
    | CReturn value ->
        fprintf pp "CReturn(value=%a)" pp_jsval value
    | CThrow value ->
        fprintf pp "CThrow(value=%a)" pp_jsval value
    | CWith value ->
        fprintf pp "CWith(value=%a)" pp_jsval value
    | CFunEnter { f; this; args } ->
        fprintf pp "CEnter(f=%a, base=%a, args=%a)"
            pp_jsval f pp_jsval this pp_jsval args
    | CFunExit { ret; exc } ->
        fprintf pp "CExit(value=%a, exception=%a)"
            pp_jsval ret pp_jsval exc
    | CScriptEnter ->
        fprintf pp "CScriptEnter"
    | CScriptExit ->
        fprintf pp "CScriptExit"
    | CScriptExc exc ->
        fprintf pp "CScriptExit(value=%a)" pp_jsval exc
    | CBinary { op; left; right; result } ->
        fprintf pp "CBinary(left=%a, op=%s, right=%a, result=%a)" pp_jsval left op pp_jsval right pp_jsval result
    | CUnary { op; arg; result } ->
        fprintf pp "CUnar(op=%s, arg=%a, result=%a)" op pp_jsval arg pp_jsval result
    | CEndExpression ->
        fprintf pp "CEndExpression"
    | CConditional value ->
        fprintf pp "CConditonal(value=%a)" pp_jsval value

let pp_clean_trace = FormatHelper.pp_print_list_lines pp_clean_operation
let pp_clean_tracefile pp (f, o, t, g, gap) =
    fprintf pp "@[<v>Globals are properties: %b@ @[<hov>%a@]@ @[<hov>%a@]@ @[<hov>Globals:@ %a@]@ Trace:@ @[<hov>%a@]@]"
        gap pp_functions f pp_objects o pp_globals g pp_clean_trace t

let encode_type isMethod isConstructor = match isMethod, isConstructor with
    | true, true -> ConstructorMethod
    | true, false -> Method
    | false, true -> Constructor
    | false, false -> Function

let check_global locals globals name =
    if List.mem name locals then (locals, globals, false)
    else if List.mem name globals then (locals, globals, true)
    else (locals, name:: globals, true)

let encode_pre ({ Trace.f; Trace.base; Trace.args; Trace.isMethod; Trace.isConstructor }: Trace.funpre) =
  CFunPre { f; base; args; call_type = encode_type isMethod isConstructor }
let encode_post { Trace.f; Trace.base; Trace.args; Trace.result; Trace.isMethod; Trace.isConstructor } =
  CFunPost { f; base; args; result; call_type = encode_type isMethod isConstructor }
let encode_decl { Trace.name; Trace.value; Trace.argument; Trace.isCatchParam } =
  assert (argument = None || isCatchParam = false);
  CDeclare { name; value; declaration_type =
    match argument with
      | Some i when i >= 0 -> ArgumentBinding i
      | Some _ -> ArgumentArray
      | None -> if isCatchParam then CatchParam else Var }

let rec clean_impl stack locals globals =
    let open Trace in function
    | FunPre fpre :: tr ->
        encode_pre fpre :: clean_impl (locals:: stack) locals globals tr
    | FunPost fpost :: tr ->
        encode_post fpost :: clean_impl (List.tl stack) (List.hd stack) globals tr
    | Literal { value; hasGetterSetter } :: tr ->
        CLiteral { value; hasGetterSetter } :: clean_impl stack locals globals tr
    | ForIn { value } :: tr ->
        CForIn value :: clean_impl stack locals globals tr
    | Declare decl :: tr ->
        encode_decl decl :: clean_impl stack (decl.name :: locals) globals tr
    | GetFieldPre _ :: tr -> clean_impl stack locals globals tr
    | GetField { base; offset; value } :: tr ->
        CGetField { base; offset; value } :: clean_impl stack locals globals tr
    | Read { name; value } :: tr ->
    (* Throw away Jalangi2's isGlobal and isScriptLocal - they turn out to be useless *)
        let (locals', globals', isGlobal) = check_global locals globals name in
        CRead { name; value; isGlobal } ::
        clean_impl stack locals' globals' tr
    | PutFieldPre _ :: tr -> clean_impl stack locals globals tr
    | PutField { base; offset; value } :: tr ->
        CPutField { base; offset; value } ::
        clean_impl stack locals globals tr
    | Write { name; lhs; value } :: tr ->
        let (locals', globals', isGlobal) = check_global locals globals name in
        CWrite { name; lhs; value; isGlobal; isSuccessful = true } ::
        clean_impl stack locals' globals' tr
    | Return { value } :: tr ->
        CReturn value :: clean_impl stack locals globals tr
    | Throw { value } :: tr ->
        CThrow value :: clean_impl stack locals globals tr
    | With { value } :: tr ->
        CWith value :: clean_impl stack locals globals tr
    | FunEnter { f; this; args } :: tr ->
        CFunEnter { f; this; args } :: clean_impl stack locals globals tr
    | FunExit { ret; exc } :: tr ->
        CFunExit { ret; exc } :: clean_impl stack locals globals tr
    | ScriptEnter :: tr ->
        CScriptEnter :: clean_impl stack locals globals tr
    | ScriptExit :: tr ->
        CScriptExit :: clean_impl stack locals globals tr
    | ScriptExc obj :: tr ->
        CScriptExc obj :: clean_impl stack locals globals tr
    | BinPre _ :: tr ->
        clean_impl stack locals globals tr
    | BinPost { op; left; right; result } :: tr ->
        CBinary { op; left; right; result } :: clean_impl stack locals globals tr
    | UnaryPre _ :: tr ->
        clean_impl stack locals globals tr
    | UnaryPost { op; arg; result } :: tr ->
        CUnary { op; arg; result } :: clean_impl stack locals globals tr
    | EndExpression _ :: tr ->
        CEndExpression :: clean_impl stack locals globals tr
    | Conditional { value } :: tr ->
        CConditional value :: clean_impl stack locals globals tr
    | [] -> []

let global_object = OObject 0

let get_object (objects: objects) objval fieldname =
  try
      objects.(get_object objval)
        |> Misc.StringMap.find fieldname
        |> fun { value } -> value
  with Not_found -> OUndefined

let get_object_array objects objval index =
  get_object objects objval (string_of_int index)
  
let lookup globals (objs: objects) path = match path with
    | [] -> global_object
    | objname :: path ->
      List.fold_left (get_object objs)
        (Misc.StringMap.find objname globals)
        path
        
let resolve_call objects function_apply function_call f base args call_type =
  let rec resolve f base args argsidx =
    if f = function_apply then
      resolve base
        (get_object_array objects args argsidx)
        (get_object_array objects args (argsidx + 1))
        0
    else if f = function_call then
      resolve base
        (get_object_array objects args argsidx)
        args
        (argsidx + 1)
    else
      { f=f; base=base; args=args; call_type=call_type }
  in resolve f base args 0  
  
let normalize_calls globals (objs: objects) =
  let function_apply = lookup globals objs ["Function"; "prototype"; "apply"]
  and function_call = lookup globals objs ["Function"; "prototype"; "call"] in 
  List.map (function
  | CFunPre { f; base; args; call_type } when f = function_apply || f = function_call ->
    CFunPre (resolve_call objs function_apply function_call f base args call_type)
  | ev -> ev
  )
  
type 'a stackop = Push of 'a | Keep | Pop | Replace of 'a
let apply_stackop stack = function
  | Push tos -> tos :: stack
  | Keep -> stack
  | Pop -> List.tl stack
  | Replace tos -> tos :: List.tl stack

let is_instrumented funcs f =
  match f with
    | OFunction (_, fid) ->
      begin match funcs.(fid) with
        | Local { from_jalangi = Some _ } -> true
        | _ -> false
      end
    | _ -> false


let pp_funpre pp ({ f; base; args; call_type }: funpre) =
  Format.fprintf pp "f=%a, base=%a, args=%a, call_type=%a"
    pp_jsval f
    pp_jsval base
    pp_jsval args
    pp_call_type call_type
    
let pp_synth_stack = FormatHelper.pp_print_list (FormatHelper.pp_print_option pp_funpre)


let synthesize_events funcs trace =
  List.fold_left (fun ((stack: funpre option list), trace) op ->
    let (stackop, ops') = match op, stack with
      (* drop clearly bad traces *)
      | _, None :: None :: _ ->
        failwith "Trace witnessing function calls in uninstrumented code, should not happen!"
      (* Function exit handling - regular or general exits *)
      (* instrumented -> instrumented *)
      | CFunExit _, Some _ :: Some _ :: _ ->
        (Pop, [ op ])
      (* instrumented -> uninstrumented *)
      | CFunExit { ret = ret; exc =OUndefined}, Some { f; base=this; args; call_type } :: None :: _ ->
        (Pop, [ op; CFunPost { f; base=this; args; call_type; result = ret } ])
      (* bad case *)
      | CFunExit _, None :: _ ->
        failwith "Trace witnessing an exit from a uninstrumented function, should not happen!"
      (* bad case *)
      | CFunExit _, [] ->
        failwith "Trace witnessing an exit from the toplevel, should not happen!"
      (* uninstrumented -> instrumented *)
      | CFunPost { result }, None :: _ ->
        (Pop, [ CFunExit { ret = result; exc = OUndefined }; op ])
      (* post inside instrumented code *)
      | CFunPost _, Some _ :: _ ->
        (Keep, [ op ])
      (* Function call handling *)
      | CFunPre _, None :: _ ->
        failwith "Trace witnessing a call in uninstrumented code, should not happen!"
      | CFunPre ({ f; base=this; args } as e), _ when is_instrumented funcs f ->
        (Push (Some e), [ op ])
      | CFunPre ({ f; base=this; args }), _ when not (is_instrumented funcs f) ->
        (Push None, [ op; CFunEnter { f; this; args } ])
      | CFunEnter _, [] ->
        failwith "Trace witnessing function entry into toplevel code, should not happen!"
      | CFunEnter _, Some _ :: _ ->
        (Keep, [ op ])
      | CFunEnter { f; this; args }, None :: _ ->
        let e = { f; base=this; args; call_type = if this = OObject 0 then Function else Method } in
        (Push (Some e), [ CFunPre e; op ])
      (* Function exit handling - exception exits to instrumented code *)
      | CDeclare { declaration_type = CatchParam; value }, None :: _ ->
        (Pop, [ CFunExit { ret = OUndefined; exc = value }; op ])
      | CScriptExc exc, None :: _ ->
        (Pop, [ CFunExit { ret = OUndefined; exc }; op ])
      (* Function exit handling - exception exits to uninstrumented code *)
      | CFunExit _, Some _ :: None :: _ ->
        failwith "Cannot handle exception exits to higher-order code yet"
      (* All cases handled for uninstrumented code *)
      | _, None :: _ ->
        failwith "Unhandled event in uninstrumented code"
      (* All other cases: non-function handling and inside instrumented code, pass through *)
      | _, _ ->
        (Keep, [ op ])
    in
       (apply_stackop stack stackop, List.rev_append ops' trace))
      ([], []) trace |> snd |> List.rev

let remove_use_strict trace =
	let rec do_remove trace = function
		| CLiteral { value = OString "use strict"; hasGetterSetter = false } ::
			CEndExpression :: rest -> do_remove trace rest
		| ev :: rest -> do_remove (ev:: trace) rest
		| [] -> List.rev trace
	in do_remove [] trace

let clean_trace globals funcs (objs: objects) trace =
    trace
    |> clean_impl [] ["this"] []
		|> remove_use_strict
    |> normalize_calls globals objs
    |> synthesize_events funcs
    
    
let clean_tracefile (funs, objs, rawtr, globals, gap) =
    (funs, objs, clean_trace globals funs objs rawtr, globals, gap)
