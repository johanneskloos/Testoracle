open Misc

type objid =
  | OUndefined
  | ONull
  | OBoolean of bool
  | ONumberInt of int
  | ONumberFloat of float
  | OString of string
  | OSymbol of string
  | OFunction of int * int
  | OObject of int
  | OOther of string * int

type funpre = { iid: int; f: objid; base: objid; args: objid; isConstructor: bool; isMethod: bool }
type funpost = { iid: int; f: objid; base: objid; args: objid; result: objid; isConstructor: bool; isMethod: bool }
type literal = { iid: int; value: objid; hasGetterSetter: bool }
type value = { iid: int; value: objid }
type declare = { iid: int; name: string; value: objid; argument: int option; isCatchParam: bool }
type getfieldpre = { iid: int; base: objid; offset: string; isComputed: bool; isOpAssign: bool; isMethodCall: bool }
type getfieldpost = { iid: int; base: objid; offset: string; value: objid; isComputed: bool; isOpAssign: bool; isMethodCall: bool }
type putfield = { iid: int; base: objid; offset: string; value: objid; isComputed: bool; isOpAssign: bool }
type access = { iid: int; name: string; value: objid; isGlobal: bool; isScriptLocal: bool }
type writeaccess = { iid: int; name: string; lhs: objid; value: objid; isGlobal: bool; isScriptLocal: bool }
type binpre = { iid: int; op: string; left: objid; right: objid; isOpAssign: bool; isSwitchCaseComparison: bool; isComputed: bool }
type binpost = { iid: int; op: string; left: objid; right: objid; result: objid; isOpAssign: bool; isSwitchCaseComparison: bool; isComputed: bool }
type unpre = { iid: int; op: string; arg: objid }
type unpost = { iid: int; op: string; arg: objid; result: objid }
type funenter = { iid: int; f: objid; this: objid; args: objid }
type funexit = { iid: int; ret: objid; exc: objid }
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
  | ScriptExc of objid
  | BinPre of binpre
  | BinPost of binpost
  | UnaryPre of unpre
  | UnaryPost of unpost
  | EndExpression of int
  | Conditional of value

type trace = operation list
type fieldspec = {
    value: objid;
    writable: bool;
    get: objid option;
    set: objid option;
    enumerable: bool;
    configurable: bool
}
type objectspec = fieldspec StringMap.t
type objects = objectspec array
type local_funcspec = { instrumented: string; uninstrumented: string }
type funcspec = Local of local_funcspec | External of int
type functions = funcspec array
type globals = objid StringMap.t
type tracefile = functions * objects * trace * globals * bool

open Yojson.Basic;;
open Yojson.Basic.Util;;

let parse_objid json =
  match member "type" json |> to_string with
    | "undefined" -> OUndefined
    | "boolean" -> OBoolean (member "val" json |> to_string |> bool_of_string)
    | "number" ->
        let numstr = member "val" json |> to_string in begin
          try ONumberInt (int_of_string numstr)
          with Failure "int_of_string" ->
            try ONumberFloat (float_of_string numstr)
            with Failure "float_of_string" ->
              failwith ("Strange number here: " ^ Yojson.Basic.to_string json)
        end
    | "string" -> OString (member "val" json |> to_string)
    | "symbol" -> OSymbol (member "val" json |> to_string)
    | "function" -> OFunction (member "id" json |> to_int, member "funid" json |> to_int)
    | "null" -> ONull
    | "object" -> OObject (member "id" json |> to_int)
    | _ as ty -> OOther (ty, member "id" json |> to_int)

let native_pattern = Str.regexp_string "[native code]"
let parse_funcspec json =
  let instr = json |> member "instrumented" |> to_string in
    if (Str.string_match native_pattern instr 0)
    then External (json |> member "obj" |> to_int)
    else Local { instrumented=instr; uninstrumented = json |> member "uninstrumented" |> to_string_option |> Misc.Option.get "(unknown)" }
let parse_fieldspec json =
    let default_to d = function Some x -> x | None -> d
    and fail_none msg = function Some x -> x | None -> failwith (msg ^ "\n" ^ Yojson.Basic.to_string json) in
    { value = json |> member "value" |> to_option parse_objid |> fail_none "No value in field";
      writable = json |> member "writable" |> to_bool_option |> default_to true;
      enumerable = json |> member "enumerable" |> to_bool_option |> default_to true;
      configurable = json |> member "configurable" |> to_bool_option |> default_to true;
      get = json |> member "get" |> to_option parse_objid;
      set = json |> member "set" |> to_option parse_objid
    }
let parse_objectspec json =
  json |> to_assoc |>
    List.fold_left
    (fun spec (name, content) ->
       StringMap.add name (parse_fieldspec content) spec)
    StringMap.empty
let parse_operation json =
  let get_int key = member key json |> to_int
  and get_string key = member key json |> to_string
  and get_objid key = member key json |> parse_objid
  and get_bool key = member key json |> to_bool in
  match member "step" json |> to_string with
    | "funpre" -> FunPre { iid = get_int "iid"; f = get_objid "f";
                           base = get_objid "base"; args = get_objid "args";
                           isConstructor = get_bool "isConstructor";
                           isMethod = get_bool "isMethod" }
    | "funpost" -> FunPost { iid = get_int "iid"; f = get_objid "f";
                             base = get_objid "base"; args = get_objid "args";
                             isConstructor = get_bool "isConstructor";
                             isMethod = get_bool "isMethod";
                             result = get_objid "result" }
    | "literal" -> Literal { iid = get_int "iid"; value = get_objid "val";
                             hasGetterSetter = get_bool "hasGetterSetter" }
    | "forin"  -> ForIn  { iid = get_int "iid"; value = get_objid "val" }
    | "declare" -> Declare { iid = get_int "iid"; name = get_string "name";
                             value = get_objid "val";
                             isCatchParam = get_bool "isCatchParam";
                             argument = if get_bool "isArgument"
                             then Some (get_int "argumentIndex") else None }
    | "getpre" -> GetFieldPre { iid = get_int "iid"; base = get_objid "base";
                                offset = get_string "offset";
                                isComputed = get_bool "isComputed";
                                isOpAssign = get_bool "isOpAssign";
                                isMethodCall = get_bool "isMethodCall" }
    | "getpost" -> GetField { iid = get_int "iid"; base = get_objid "base";
                              offset = get_string "offset";
                              isComputed = get_bool "isComputed";
                              isOpAssign = get_bool "isOpAssign";
                              isMethodCall = get_bool "isMethodCall";
                              value = get_objid "val" }
    | "putpre" -> PutFieldPre { iid = get_int "iid"; base = get_objid "base";
                                offset = get_string "offset";
                                isComputed = get_bool "isComputed";
                                isOpAssign = get_bool "isOpAssign";
                                value = get_objid "val" }
    | "putpost" -> PutField { iid = get_int "iid"; base = get_objid "base";
                              offset = get_string "offset";
                              isComputed = get_bool "isComputed";
                              isOpAssign = get_bool "isOpAssign";
                              value = get_objid "val" }
    | "read" -> Read { iid = get_int "iid"; name = get_string "name";
                       value = get_objid "val"; isGlobal = get_bool "isGlobal";
                       isScriptLocal = get_bool "isScriptLocal" }
    | "write" -> Write { iid = get_int "iid"; name = get_string "name";
                         value = get_objid "val"; lhs = get_objid "lhs";
                         isGlobal = get_bool "isGlobal";
                         isScriptLocal = get_bool "isScriptLocal" }
    | "return" -> Return { iid = get_int "iid"; value = get_objid "val" }
    | "throw" -> Throw { iid = get_int "iid"; value = get_objid "val" }
    | "with" -> With { iid = get_int "iid"; value = get_objid "val" }
    | "funcenter" -> FunEnter { iid = get_int "iid"; f = get_objid "f";
                                this = get_objid "this";
                                args = get_objid "args" }
    | "funcexit" -> FunExit { iid = get_int "iid"; ret = get_objid "ret";
                              exc = get_objid "exc" }
    | "scriptenter" -> ScriptEnter
    | "scriptexit" -> ScriptExit
    | "scriptexc" -> ScriptExc (get_objid "exc")
    | "binarypre" -> BinPre { iid = get_int "iid"; op = get_string "op";
                              left = get_objid "left";
                              right = get_objid "right";
                              isOpAssign = get_bool "isOpAssign";
                              isSwitchCaseComparison =
                                get_bool "isSwitchComparison";
                              isComputed = get_bool "isComputed" }
    | "binarypost" -> BinPost { iid = get_int "iid"; op = get_string "op";
                                left = get_objid "left";
                                right = get_objid "right";
                                isOpAssign = get_bool "isOpAssign";
                                isSwitchCaseComparison =
                                  get_bool "isSwitchComparison";
                                isComputed = get_bool "isComputed";
                                result = get_objid "result" }
    | "unarypre" -> UnaryPre { iid = get_int "iid"; op = get_string "op";
                               arg = get_objid "left" }
    | "unarypost" -> UnaryPost { iid = get_int "iid"; op = get_string "op";
                                 arg = get_objid "left";
                                 result = get_objid "result" }
    | "exprend" -> EndExpression (get_int "iid")
    | "conditional" -> Conditional { iid = get_int "iid";
                                     value = get_objid "result" }
    | _ as op -> failwith ("Unknown operation " ^ op)

let parse_functions json =
  json |> convert_each parse_funcspec |> Array.of_list
let parse_objects json =
  json |> convert_each parse_objectspec |> Array.of_list
let parse_trace json =
  json |> convert_each parse_operation
let parse_globals json =
  json |> to_assoc |>
    List.fold_left
    (fun spec (name, content) ->
       StringMap.add name (parse_objid content) spec)
    StringMap.empty
let parse_tracefile source =
  let json = from_channel source in
    (parse_functions (member "func" json),
     parse_objects (member "obj" json),
     parse_trace (member "trace" json),
     parse_globals (member "globals" json),
     to_bool (member "globals_are_properties" json))

let dump_objid = function
    | OUndefined -> `Assoc [ ("type", `String "undefined") ]
    | OBoolean b -> `Assoc [ ("type", `String "boolean"); ("val", `String (string_of_bool b)) ]
    | ONumberInt n -> `Assoc [ ("type", `String "number"); ("val", `String (string_of_int n)) ]
    | ONumberFloat n -> `Assoc [ ("type", `String "number"); ("val", `String (string_of_float n)) ]
    | OString s ->  `Assoc [ ("type", `String "string"); ("val", `String s) ]
    | OSymbol s ->  `Assoc [ ("type", `String "symbol"); ("val", `String s) ]
    | ONull -> `Assoc [ ("type", `String "null") ]
    | OObject id -> `Assoc [ ("type", `String "object"); ("id", `Int id) ]
    | OOther (ty, id) -> `Assoc [ ("type", `String ty); ("id", `Int id) ]
    | OFunction (id, fid) -> `Assoc [ ("type", `String "function"); ("id", `Int id); ("funid", `Int fid) ]

let dump_funcspec = function
    | External id -> `Assoc [ ("instrumented", `String "[native code]"); ("obj", `Int id) ]
    | Local {instrumented; uninstrumented} ->
            `Assoc [ ("instrumented", `String instrumented); ("uninstrumented", `String uninstrumented) ]
let dump_fieldspec { value; writable; get; set; enumerable; configurable }: json =
    let add name x l = match x with Some id -> (name, dump_objid id) :: l | None -> l in
    `Assoc ([ ("value", dump_objid value); ("writable", `Bool writable);
    ("enumerable", `Bool enumerable); ("configurable", `Bool configurable) ] |>
    add "get" get |> add "set" set);;

let dump_objectspec os = `Assoc (List.map (map22 dump_fieldspec) (StringMap.bindings os))

let dump_operation = function
    | FunPre { iid; f; base; args; isConstructor; isMethod } ->
            `Assoc [ ("step", `String "funpre"); ("iid", `Int iid);
            ("f", dump_objid f); ("base", dump_objid base);
            ("args", dump_objid args);
            ("isConstructor", `Bool isConstructor);
            ("isMethod", `Bool isMethod) ]
    | FunPost { iid; f; base; args; isConstructor; isMethod; result } ->
            `Assoc [ ("step", `String "funpost"); ("iid", `Int iid);
            ("f", dump_objid f); ("base", dump_objid base);
            ("args", dump_objid args); ("result", dump_objid result);
            ("isConstructor", `Bool isConstructor);
            ("isMethod", `Bool isMethod) ]
    | Literal { iid; value; hasGetterSetter } ->
            `Assoc [ ("step", `String "literal"); ("iid", `Int iid);
            ("val", dump_objid value); ("hasGetterSetter", `Bool hasGetterSetter)]
    | ForIn { iid; value } ->
            `Assoc [ ("step", `String "forin"); ("iid", `Int iid);
            ("val", dump_objid value) ]
    | Declare { iid; name; value; isCatchParam; argument=Some arg } ->
            `Assoc [ ("step", `String "declare"); ("iid", `Int iid);
            ("val", dump_objid value); ("name", `String name);
            ("isCatchParam", `Bool isCatchParam);
            ("isArgument", `Bool true); ("argumentIndex", `Int arg) ]
    | Declare { iid; name; value; isCatchParam; argument=None } ->
            `Assoc [ ("step", `String "declare"); ("iid", `Int iid);
            ("val", dump_objid value); ("name", `String name);
            ("isCatchParam", `Bool isCatchParam);
            ("isArgument", `Bool false) ]
    | GetFieldPre { iid; base; offset; isComputed; isOpAssign; isMethodCall } ->
            `Assoc [ ("step", `String "getpre"); ("iid", `Int iid);
            ("base", dump_objid base); ("offset", `String offset);
            ("isComputed", `Bool isComputed); ("isOpAssign", `Bool isOpAssign);
            ("isMethodCall", `Bool isMethodCall) ]
    | GetField { iid; base; offset; isComputed; isOpAssign; isMethodCall; value } ->
            `Assoc [ ("step", `String "getpost"); ("iid", `Int iid);
            ("base", dump_objid base); ("offset", `String offset);
            ("isComputed", `Bool isComputed); ("isOpAssign", `Bool isOpAssign);
            ("isMethodCall", `Bool isMethodCall); ("val", dump_objid value) ]
    | PutFieldPre { iid; base; offset; isComputed; isOpAssign; value } ->
            `Assoc [ ("step", `String "putpre"); ("iid", `Int iid);
            ("base", dump_objid base); ("offset", `String offset);
            ("isComputed", `Bool isComputed); ("isOpAssign", `Bool isOpAssign);
            ("val", dump_objid value) ]
    | PutField { iid; base; offset; isComputed; isOpAssign; value } ->
            `Assoc [ ("step", `String "putpost"); ("iid", `Int iid);
            ("base", dump_objid base); ("offset", `String offset);
            ("isComputed", `Bool isComputed); ("isOpAssign", `Bool isOpAssign);
            ("val", dump_objid value) ]
    | Read { iid; name; value; isGlobal; isScriptLocal } ->
            `Assoc [ ("step", `String "read"); ("iid", `Int iid);
            ("name", `String name); ("val", dump_objid value);
            ("isGlobal", `Bool isGlobal); ("isScriptLocal", `Bool isScriptLocal) ]
    | Write { iid; name; value; isGlobal; lhs; isScriptLocal } ->
            `Assoc [ ("step", `String "write"); ("iid", `Int iid); ("lhs", dump_objid lhs);
            ("name", `String name); ("val", dump_objid value);
            ("isGlobal", `Bool isGlobal); ("isScriptLocal", `Bool isScriptLocal) ]
    | Return { iid; value } ->
            `Assoc [ ("step", `String "return"); ("iid", `Int iid);
            ("val", dump_objid value) ]
    | Throw { iid; value } ->
            `Assoc [ ("step", `String "throw"); ("iid", `Int iid);
            ("val", dump_objid value) ]
    | With { iid; value } ->
            `Assoc [ ("step", `String "throw"); ("iid", `Int iid);
            ("val", dump_objid value) ]
    | FunEnter { iid; f; this; args } ->
            `Assoc [ ("step", `String "funcenter"); ("iid", `Int iid);
            ("f", dump_objid f); ("this", dump_objid this); ("args", dump_objid args) ]
    | FunExit { iid; ret; exc } ->
            `Assoc [ ("step", `String "funcexit"); ("iid", `Int iid);
            ("ret", dump_objid ret); ("exc", dump_objid exc) ]
    | ScriptEnter -> `Assoc [ ("step", `String "scriptenter") ]
    | ScriptExit -> `Assoc [ ("step", `String "scriptexit") ]
    | ScriptExc exc -> `Assoc [ ("step", `String "scriptexc"); ("exc", dump_objid exc) ]
    | BinPre { iid; op; left; right; isOpAssign; isSwitchCaseComparison; isComputed } ->
            `Assoc [ ("step", `String "binarypre"); ("iid", `Int iid); ("op", `String op);
            ("left", dump_objid left); ("right", dump_objid right);
            ("isOpAssign", `Bool isOpAssign); ("isComputed", `Bool isComputed);
            ("isSwitchComparison", `Bool isSwitchCaseComparison) ]
    | BinPost { iid; op; left; right; isOpAssign; isSwitchCaseComparison; isComputed; result } ->
            `Assoc [ ("step", `String "binarypost"); ("iid", `Int iid); ("op", `String op);
            ("left", dump_objid left); ("right", dump_objid right);
            ("isOpAssign", `Bool isOpAssign); ("isComputed", `Bool isComputed);
            ("isSwitchComparison", `Bool isSwitchCaseComparison);
            ("result", dump_objid result) ]
    | UnaryPre { iid; op; arg } ->
            `Assoc [ ("step", `String "unarypre"); ("iid", `Int iid);
            ("op", `String op); ("left", dump_objid arg) ]
    | UnaryPost { iid; op; arg; result } ->
            `Assoc [ ("step", `String "unarypost"); ("iid", `Int iid);
            ("op", `String op); ("left", dump_objid arg); ("result", dump_objid result) ]
    | EndExpression iid ->
            `Assoc [ ("step", `String "exprend"); ("iid", `Int iid) ]
    | Conditional { iid; value } ->
            `Assoc [ ("step", `String "conditional"); ("iid", `Int iid);
                ("result", dump_objid value) ]

let dump_functions funs =
    `List (funs |> Array.map dump_funcspec |> Array.to_list)
let dump_objects objs =
    `List (objs |> Array.map dump_objectspec |> Array.to_list)
let dump_trace trace =
    `List (trace |> List.map dump_operation)
let dump_globals globs: json =
    `Assoc (globs |> StringMap.bindings |> List.map (map22 dump_objid))
let dump_tracefile_json (funs, objs, trace, globals, globals_are_properties) =
    `Assoc [
        ("func", dump_functions funs);
        ("obj", dump_objects objs);
        ("trace", dump_trace trace);
        ("globals", dump_globals globals);
        ("globals_are_properties", `Bool globals_are_properties)
    ]
let dump_tracefile channel tr =
    tr |> dump_tracefile_json |> pretty_to_channel channel

open Format
let pp_objid pp = function
  | OUndefined -> pp_print_string pp "undefined"
  | ONull -> pp_print_string pp "null"
  | OBoolean x -> fprintf pp "bool:%b" x
  | ONumberInt x -> fprintf pp "int:%d" x
  | ONumberFloat x -> fprintf pp "float:%f" x
  | OString x -> fprintf pp "string:%s" x
  | OSymbol x -> fprintf pp "symbol:%s" x
  | OFunction (id, fid) -> fprintf pp "function:%d/%d" id fid
  | OObject id  -> fprintf pp "object:%d" id
  | OOther (ty, id) -> fprintf pp "other:%s:%d" ty id

let pp_operation pp = function
  | FunPre x ->
      if x.isConstructor && x.isMethod then fprintf pp "new %a.%a(%a)" pp_objid x.base pp_objid x.f pp_objid x.args
      else if x.isConstructor then fprintf pp "new %a::%a(%a)" pp_objid x.base pp_objid x.f pp_objid x.args
      else if x.isMethod then fprintf pp "%a.%a(%a)" pp_objid x.base pp_objid x.f pp_objid x.args
      else fprintf pp "%a::%a(%a)" pp_objid x.base pp_objid x.f pp_objid x.args
  | FunPost x ->
      if x.isConstructor && x.isMethod then fprintf pp "new %a.%a(%a) ⇒ %a" pp_objid x.base pp_objid x.f pp_objid x.args pp_objid x.result
      else if x.isConstructor then fprintf pp "new %a::%a(%a) ⇒ %a" pp_objid x.base pp_objid x.f pp_objid x.args pp_objid x.result
      else if x.isMethod then fprintf pp "%a.%a(%a) ⇒ %a" pp_objid x.base pp_objid x.f pp_objid x.args pp_objid x.result
      else fprintf pp "%a::%a(%a) ⇒ %a" pp_objid x.base pp_objid x.f pp_objid x.args pp_objid x.result
  | Literal x -> fprintf pp "literal %a%s" pp_objid x.value (if x.hasGetterSetter then " (has getter and setter)" else "")
  | ForIn x -> fprintf pp "for (… in %a)" pp_objid x.value
  | Declare x ->
      begin match x.argument with
        | Some i -> fprintf pp "argument def: %s from %d = %a" x.name i pp_objid x.value
        | None ->
            if x.isCatchParam then
              fprintf pp "catch %s = %a" x.name pp_objid x.value
            else
              fprintf pp "var %s = %a" x.name pp_objid x.value
      end
  | GetFieldPre x ->
      if x.isOpAssign then
        fprintf pp "%a[%s] (for assign-and-modify)" pp_objid x.base x.offset
      else if x.isMethodCall then
        fprintf pp "%a[%s] (for method call)" pp_objid x.base x.offset
      else
        fprintf pp "%a[%s]" pp_objid x.base x.offset
  | GetField x ->
      if x.isOpAssign then
        fprintf pp "%a[%s] ⇒ %a (for assign-and-modify)" pp_objid x.base x.offset pp_objid x.value
      else if x.isMethodCall then
        fprintf pp "%a[%s] ⇒ %a (for method call)" pp_objid x.base x.offset pp_objid x.value
      else
        fprintf pp "%a[%s] ⇒ %a" pp_objid x.base x.offset pp_objid x.value
  | PutFieldPre x ->
      if x.isOpAssign then
        fprintf pp "%a[%s] = %a (pre, for assign-and-modify)" pp_objid x.base x.offset pp_objid x.value
      else
        fprintf pp "%a[%s] = %a (pre)" pp_objid x.base x.offset pp_objid x.value
  | PutField x ->
      if x.isOpAssign then
        fprintf pp "%a[%s] = %a (for assign-and-modify)" pp_objid x.base x.offset pp_objid x.value
      else
        fprintf pp "%a[%s] = %a" pp_objid x.base x.offset pp_objid x.value
  | Read x ->
      fprintf pp "%s ⇒ %a%s" x.name pp_objid x.value (if x.isGlobal then " (global)"
                                                     else if x.isScriptLocal then " (scriptLocal)"
                                                     else "")
  | Write x ->
      fprintf pp "%s (%a) = %a%s" x.name pp_objid x.lhs pp_objid x.value (if x.isGlobal then " (global)"
                                                     else if x.isScriptLocal then " (scriptLocal)"
                                                     else "")
  | Return x -> fprintf pp "return %a" pp_objid x.value
  | Throw x -> fprintf pp "throw %a" pp_objid x.value
  | With x -> fprintf pp "with %a" pp_objid x.value
  | FunEnter x -> fprintf pp "enter %a@@%a(%a)" pp_objid x.f pp_objid x.this pp_objid x.args
  | FunExit x -> fprintf pp "exit %a/%a" pp_objid x.ret pp_objid x.exc
  | ScriptEnter -> pp_print_string pp "script entry"
  | ScriptExit -> pp_print_string pp "script exit"
  | ScriptExc exc -> fprintf pp "script exception %a" pp_objid exc
  | BinPre x ->
      if x.isOpAssign then
        fprintf pp "%a %s= %a" pp_objid x.left x.op pp_objid x.right
      else if x.isSwitchCaseComparison then
        fprintf pp "case %a %s %a" pp_objid x.left x.op pp_objid x.right
      else
        fprintf pp "%a %s %a" pp_objid x.left x.op pp_objid x.right
  | BinPost x ->
      if x.isOpAssign then
        fprintf pp "%a %s= %a ⇒ %a" pp_objid x.left x.op pp_objid x.right pp_objid x.result
      else if x.isSwitchCaseComparison then
        fprintf pp "case %a %s %a ⇒ %a" pp_objid x.left x.op pp_objid x.right pp_objid x.result
      else
        fprintf pp "%a %s %a ⇒ %a" pp_objid x.left x.op pp_objid x.right pp_objid x.result
  | UnaryPre x -> fprintf pp "%s %a" x.op pp_objid x.arg
  | UnaryPost x -> fprintf pp "%s %a ⇒ %a" x.op pp_objid x.arg pp_objid x.result
  | EndExpression iid -> pp_print_string pp "(end of expression)"
  | Conditional v -> fprintf pp "end of conditional, %a" pp_objid v.value

let pp_trace pp trace =
  pp_open_vbox pp 0;
  List.iter (fprintf pp "@[<h>%a@];@ " pp_operation) trace;
  pp_close_box pp ()

let pp_fieldspec pp { value; set; get; writable; enumerable; configurable } =
    (* Special-case the most common case *)
    if writable && enumerable && configurable && set = None && get = None then
        pp_objid pp value
    else if set = None && get = None then
        fprintf pp "[%s%s%s] %a"
          (if writable then "W" else "-")
          (if enumerable then "E" else "-")
          (if configurable then "C" else "-")
          pp_objid value
    else
        fprintf pp "[%s%s%s] %a { get = %a, set = %a }"
          (if writable then "W" else "-")
          (if enumerable then "E" else "-")
          (if configurable then "C" else "-")
          pp_objid value
          (FormatHelper.pp_print_option pp_objid) get
          (FormatHelper.pp_print_option pp_objid) set

let pp_objectspec pp spec =
  pp_open_hovbox pp 0;
  pp_print_string pp "{";
  StringMap.iter (fun fld value -> fprintf pp "@[<hov>%s → %a;@]" fld pp_fieldspec value) spec;
  pp_print_string pp "}";
  pp_close_box pp ()

let pp_objects pp arr =
  pp_open_vbox pp 0;
  Array.iteri (fun i s -> fprintf pp "%i ↦ %a;@ " i pp_objectspec s) arr;
  pp_close_box pp ()
let pp_local_funcspec pp s =
  fprintf pp "@[<hov>@ uninstrumented code: @[<hov>%s@]@]" s.uninstrumented
let pp_funcspec pp = function
  | Local s -> pp_local_funcspec pp s
  | External id -> fprintf pp "(external code, id=%d)" id
let pp_functions pp arr =
  pp_open_vbox pp 0;
  Array.iteri (fun i s -> fprintf pp "%i ↦ %a;@ " i pp_funcspec s) arr;
  pp_close_box pp ()
let pp_globals pp spec =
  pp_open_hovbox pp 0;
  pp_print_string pp "{";
  StringMap.iter (fun fld value -> fprintf pp "@[<hov>%s → %a;@]" fld pp_objid value) spec;
  pp_print_string pp "}";
  pp_close_box pp ()

let pp_tracefile pp (f, o, t, g, gap) =
  fprintf pp "@[<v>Globals are properties: %b@ @[<hov>%a@]@ @[<hov>%a@]@ @[<hov>Globals:@ %a@]@ Trace:@ @[<hov>%a@]@]"
    gap pp_functions f pp_objects o pp_globals g pp_trace t 

exception NotAnObject
let get_object = function
    | OObject id -> id
    | OOther (_, id) -> id
    | OFunction (id, _) -> id
    | _ -> raise NotAnObject


