open Types
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

let encode_type isMethod isConstructor = match isMethod, isConstructor with
    | true, true -> ConstructorMethod
    | true, false -> Method
    | false, true -> Constructor
    | false, false -> Function

let check_global locals globals name =
    if List.mem name locals then (locals, globals, false)
    else if List.mem name globals then (locals, globals, true)
    else (locals, name:: globals, true)

let encode_pre ({ f; base; args; isMethod; isConstructor }: Trace.funpre) =
  CFunPre { f; base; args; call_type = encode_type isMethod isConstructor }
let encode_post { f; base; args; result; isMethod; isConstructor } =
  CFunPost { f; base; args; result; call_type = encode_type isMethod isConstructor }
let encode_decl { name; value; argument; isCatchParam } =
  assert (argument = None || isCatchParam = false);
  CDeclare { name; value; declaration_type =
    match argument with
      | Some i when i >= 0 -> ArgumentBinding i
      | Some _ -> ArgumentArray
      | None -> if isCatchParam then CatchParam else Var }

let rec clean_impl stack locals globals = function
    (* Special-case handling for from_jalangi function bodies *)
    | FunPre ({ f; base; args } as fpre) :: FunPost ({ result } as fpost) :: tr ->
        encode_pre fpre ::
        CFunEnter { f; this=base; args } ::
        CFunExit { ret=result; exc=OUndefined } ::
        encode_post fpost ::
        clean_impl stack locals globals tr 
    | FunPre ({ f; base; args } as fpre) :: Declare ({ value; isCatchParam=true } as decl) :: tr ->
        encode_pre fpre ::
        CFunEnter { f; this=base; args } ::
        CFunExit { ret=OUndefined; exc=value } ::
        encode_decl decl ::
        clean_impl stack locals globals tr 
    (* Regular cleanup *)
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

let clean_trace = clean_impl [] ["this"] []
let clean_tracefile (funs, objs, rawtr, globals, gap) =
    (funs, objs, clean_trace rawtr, globals, gap)

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
