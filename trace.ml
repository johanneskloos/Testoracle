open Misc

type jsval =
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

type funpre = { iid: int; f: jsval; base: jsval; args: jsval; isConstructor: bool; isMethod: bool }
type funpost = { iid: int; f: jsval; base: jsval; args: jsval; result: jsval; isConstructor: bool; isMethod: bool }
type literal = { iid: int; value: jsval; hasGetterSetter: bool }
type value = { iid: int; value: jsval }
type declare = { iid: int; name: string; value: jsval; argument: int option; isCatchParam: bool }
type getfieldpre = { iid: int; base: jsval; offset: string; isComputed: bool; isOpAssign: bool; isMethodCall: bool }
type getfieldpost = { iid: int; base: jsval; offset: string; value: jsval; isComputed: bool; isOpAssign: bool; isMethodCall: bool }
type putfield = { iid: int; base: jsval; offset: string; value: jsval; isComputed: bool; isOpAssign: bool }
type access = { iid: int; name: string; value: jsval; isGlobal: bool; isScriptLocal: bool }
type writeaccess = { iid: int; name: string; lhs: jsval; value: jsval; isGlobal: bool; isScriptLocal: bool }
type binpre = { iid: int; op: string; left: jsval; right: jsval; isOpAssign: bool; isSwitchCaseComparison: bool; isComputed: bool }
type binpost = { iid: int; op: string; left: jsval; right: jsval; result: jsval; isOpAssign: bool; isSwitchCaseComparison: bool; isComputed: bool }
type unpre = { iid: int; op: string; arg: jsval }
type unpost = { iid: int; op: string; arg: jsval; result: jsval }
type funenter = { iid: int; f: jsval; this: jsval; args: jsval }
type funexit = { iid: int; ret: jsval; exc: jsval }
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
type objectspec = fieldspec StringMap.t
type objects = objectspec array
type local_funcspec = { instrumented: string; uninstrumented: string }
type funcspec = Local of local_funcspec | External of int
type functions = funcspec array
type global_desc = {
  id: jsval;
  obj: objectspec;
  proto: objectspec
  }
type globals = global_desc Misc.StringMap.t
type tracefile = functions * objects * trace * globals * bool

open Yojson.Basic;;
open Yojson.Basic.Util;;

let parse_jsval json =
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
    { value = json |> member "value" |> to_option parse_jsval |> fail_none "No value in field";
      writable = json |> member "writable" |> to_bool_option |> default_to true;
      enumerable = json |> member "enumerable" |> to_bool_option |> default_to true;
      configurable = json |> member "configurable" |> to_bool_option |> default_to true;
      get = json |> member "get" |> to_option parse_jsval;
      set = json |> member "set" |> to_option parse_jsval
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
  and get_jsval key = member key json |> parse_jsval
  and get_bool key = member key json |> to_bool in
  match member "step" json |> to_string with
    | "funpre" -> FunPre { iid = get_int "iid"; f = get_jsval "f";
                           base = get_jsval "base"; args = get_jsval "args";
                           isConstructor = get_bool "isConstructor";
                           isMethod = get_bool "isMethod" }
    | "funpost" -> FunPost { iid = get_int "iid"; f = get_jsval "f";
                             base = get_jsval "base"; args = get_jsval "args";
                             isConstructor = get_bool "isConstructor";
                             isMethod = get_bool "isMethod";
                             result = get_jsval "result" }
    | "literal" -> Literal { iid = get_int "iid"; value = get_jsval "val";
                             hasGetterSetter = get_bool "hasGetterSetter" }
    | "forin"  -> ForIn  { iid = get_int "iid"; value = get_jsval "val" }
    | "declare" -> Declare { iid = get_int "iid"; name = get_string "name";
                             value = get_jsval "val";
                             isCatchParam = get_bool "isCatchParam";
                             argument = if get_bool "isArgument"
                             then Some (get_int "argumentIndex") else None }
    | "getpre" -> GetFieldPre { iid = get_int "iid"; base = get_jsval "base";
                                offset = get_string "offset";
                                isComputed = get_bool "isComputed";
                                isOpAssign = get_bool "isOpAssign";
                                isMethodCall = get_bool "isMethodCall" }
    | "getpost" -> GetField { iid = get_int "iid"; base = get_jsval "base";
                              offset = get_string "offset";
                              isComputed = get_bool "isComputed";
                              isOpAssign = get_bool "isOpAssign";
                              isMethodCall = get_bool "isMethodCall";
                              value = get_jsval "val" }
    | "putpre" -> PutFieldPre { iid = get_int "iid"; base = get_jsval "base";
                                offset = get_string "offset";
                                isComputed = get_bool "isComputed";
                                isOpAssign = get_bool "isOpAssign";
                                value = get_jsval "val" }
    | "putpost" -> PutField { iid = get_int "iid"; base = get_jsval "base";
                              offset = get_string "offset";
                              isComputed = get_bool "isComputed";
                              isOpAssign = get_bool "isOpAssign";
                              value = get_jsval "val" }
    | "read" -> Read { iid = get_int "iid"; name = get_string "name";
                       value = get_jsval "val"; isGlobal = get_bool "isGlobal";
                       isScriptLocal = get_bool "isScriptLocal" }
    | "write" -> Write { iid = get_int "iid"; name = get_string "name";
                         value = get_jsval "val"; lhs = get_jsval "lhs";
                         isGlobal = get_bool "isGlobal";
                         isScriptLocal = get_bool "isScriptLocal" }
    | "return" -> Return { iid = get_int "iid"; value = get_jsval "val" }
    | "throw" -> Throw { iid = get_int "iid"; value = get_jsval "val" }
    | "with" -> With { iid = get_int "iid"; value = get_jsval "val" }
    | "funcenter" -> FunEnter { iid = get_int "iid"; f = get_jsval "f";
                                this = get_jsval "this";
                                args = get_jsval "args" }
    | "funcexit" -> FunExit { iid = get_int "iid"; ret = get_jsval "ret";
                              exc = get_jsval "exc" }
    | "scriptenter" -> ScriptEnter
    | "scriptexit" -> ScriptExit
    | "scriptexc" -> ScriptExc (get_jsval "exc")
    | "binarypre" -> BinPre { iid = get_int "iid"; op = get_string "op";
                              left = get_jsval "left";
                              right = get_jsval "right";
                              isOpAssign = get_bool "isOpAssign";
                              isSwitchCaseComparison =
                                get_bool "isSwitchComparison";
                              isComputed = get_bool "isComputed" }
    | "binarypost" -> BinPost { iid = get_int "iid"; op = get_string "op";
                                left = get_jsval "left";
                                right = get_jsval "right";
                                isOpAssign = get_bool "isOpAssign";
                                isSwitchCaseComparison =
                                  get_bool "isSwitchComparison";
                                isComputed = get_bool "isComputed";
                                result = get_jsval "result" }
    | "unarypre" -> UnaryPre { iid = get_int "iid"; op = get_string "op";
                               arg = get_jsval "left" }
    | "unarypost" -> UnaryPost { iid = get_int "iid"; op = get_string "op";
                                 arg = get_jsval "left";
                                 result = get_jsval "result" }
    | "exprend" -> EndExpression (get_int "iid")
    | "conditional" -> Conditional { iid = get_int "iid";
                                     value = get_jsval "result" }
    | _ as op -> failwith ("Unknown operation " ^ op)

let parse_functions json =
  json |> convert_each parse_funcspec |> Array.of_list
let parse_objects json =
  json |> convert_each parse_objectspec |> Array.of_list
let parse_trace json =
  json |> convert_each parse_operation
let parse_global_desc json =
  let id = json |> member "id" |> parse_jsval in
  let obj = json |> member "obj_data" |> parse_objectspec in
  let proto = begin match json |> member "proto_data" with
    |  `Null -> Misc.StringMap.empty
    | data -> parse_objectspec data end in
  { id; obj; proto}

let parse_globals json: globals =
  let module Extra = Misc.MapExtra(Misc.StringMap) in
  json |> to_assoc |> Extra.of_list |>
  Misc.StringMap.map parse_global_desc
  
let parse_tracefile source =
  let json = from_channel source in
    (parse_functions (member "func" json),
     parse_objects (member "obj" json),
     parse_trace (member "trace" json),
     parse_globals (member "globals" json),
     to_bool (member "globals_are_properties" json))

let dump_jsval = function
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
    let add name x l = match x with Some id -> (name, dump_jsval id) :: l | None -> l in
    `Assoc ([ ("value", dump_jsval value); ("writable", `Bool writable);
    ("enumerable", `Bool enumerable); ("configurable", `Bool configurable) ] |>
    add "get" get |> add "set" set);;

let dump_objectspec os = `Assoc (List.map (map22 dump_fieldspec) (StringMap.bindings os))

let dump_operation = function
    | FunPre { iid; f; base; args; isConstructor; isMethod } ->
            `Assoc [ ("step", `String "funpre"); ("iid", `Int iid);
            ("f", dump_jsval f); ("base", dump_jsval base);
            ("args", dump_jsval args);
            ("isConstructor", `Bool isConstructor);
            ("isMethod", `Bool isMethod) ]
    | FunPost { iid; f; base; args; isConstructor; isMethod; result } ->
            `Assoc [ ("step", `String "funpost"); ("iid", `Int iid);
            ("f", dump_jsval f); ("base", dump_jsval base);
            ("args", dump_jsval args); ("result", dump_jsval result);
            ("isConstructor", `Bool isConstructor);
            ("isMethod", `Bool isMethod) ]
    | Literal { iid; value; hasGetterSetter } ->
            `Assoc [ ("step", `String "literal"); ("iid", `Int iid);
            ("val", dump_jsval value); ("hasGetterSetter", `Bool hasGetterSetter)]
    | ForIn { iid; value } ->
            `Assoc [ ("step", `String "forin"); ("iid", `Int iid);
            ("val", dump_jsval value) ]
    | Declare { iid; name; value; isCatchParam; argument=Some arg } ->
            `Assoc [ ("step", `String "declare"); ("iid", `Int iid);
            ("val", dump_jsval value); ("name", `String name);
            ("isCatchParam", `Bool isCatchParam);
            ("isArgument", `Bool true); ("argumentIndex", `Int arg) ]
    | Declare { iid; name; value; isCatchParam; argument=None } ->
            `Assoc [ ("step", `String "declare"); ("iid", `Int iid);
            ("val", dump_jsval value); ("name", `String name);
            ("isCatchParam", `Bool isCatchParam);
            ("isArgument", `Bool false) ]
    | GetFieldPre { iid; base; offset; isComputed; isOpAssign; isMethodCall } ->
            `Assoc [ ("step", `String "getpre"); ("iid", `Int iid);
            ("base", dump_jsval base); ("offset", `String offset);
            ("isComputed", `Bool isComputed); ("isOpAssign", `Bool isOpAssign);
            ("isMethodCall", `Bool isMethodCall) ]
    | GetField { iid; base; offset; isComputed; isOpAssign; isMethodCall; value } ->
            `Assoc [ ("step", `String "getpost"); ("iid", `Int iid);
            ("base", dump_jsval base); ("offset", `String offset);
            ("isComputed", `Bool isComputed); ("isOpAssign", `Bool isOpAssign);
            ("isMethodCall", `Bool isMethodCall); ("val", dump_jsval value) ]
    | PutFieldPre { iid; base; offset; isComputed; isOpAssign; value } ->
            `Assoc [ ("step", `String "putpre"); ("iid", `Int iid);
            ("base", dump_jsval base); ("offset", `String offset);
            ("isComputed", `Bool isComputed); ("isOpAssign", `Bool isOpAssign);
            ("val", dump_jsval value) ]
    | PutField { iid; base; offset; isComputed; isOpAssign; value } ->
            `Assoc [ ("step", `String "putpost"); ("iid", `Int iid);
            ("base", dump_jsval base); ("offset", `String offset);
            ("isComputed", `Bool isComputed); ("isOpAssign", `Bool isOpAssign);
            ("val", dump_jsval value) ]
    | Read { iid; name; value; isGlobal; isScriptLocal } ->
            `Assoc [ ("step", `String "read"); ("iid", `Int iid);
            ("name", `String name); ("val", dump_jsval value);
            ("isGlobal", `Bool isGlobal); ("isScriptLocal", `Bool isScriptLocal) ]
    | Write { iid; name; value; isGlobal; lhs; isScriptLocal } ->
            `Assoc [ ("step", `String "write"); ("iid", `Int iid); ("lhs", dump_jsval lhs);
            ("name", `String name); ("val", dump_jsval value);
            ("isGlobal", `Bool isGlobal); ("isScriptLocal", `Bool isScriptLocal) ]
    | Return { iid; value } ->
            `Assoc [ ("step", `String "return"); ("iid", `Int iid);
            ("val", dump_jsval value) ]
    | Throw { iid; value } ->
            `Assoc [ ("step", `String "throw"); ("iid", `Int iid);
            ("val", dump_jsval value) ]
    | With { iid; value } ->
            `Assoc [ ("step", `String "throw"); ("iid", `Int iid);
            ("val", dump_jsval value) ]
    | FunEnter { iid; f; this; args } ->
            `Assoc [ ("step", `String "funcenter"); ("iid", `Int iid);
            ("f", dump_jsval f); ("this", dump_jsval this); ("args", dump_jsval args) ]
    | FunExit { iid; ret; exc } ->
            `Assoc [ ("step", `String "funcexit"); ("iid", `Int iid);
            ("ret", dump_jsval ret); ("exc", dump_jsval exc) ]
    | ScriptEnter -> `Assoc [ ("step", `String "scriptenter") ]
    | ScriptExit -> `Assoc [ ("step", `String "scriptexit") ]
    | ScriptExc exc -> `Assoc [ ("step", `String "scriptexc"); ("exc", dump_jsval exc) ]
    | BinPre { iid; op; left; right; isOpAssign; isSwitchCaseComparison; isComputed } ->
            `Assoc [ ("step", `String "binarypre"); ("iid", `Int iid); ("op", `String op);
            ("left", dump_jsval left); ("right", dump_jsval right);
            ("isOpAssign", `Bool isOpAssign); ("isComputed", `Bool isComputed);
            ("isSwitchComparison", `Bool isSwitchCaseComparison) ]
    | BinPost { iid; op; left; right; isOpAssign; isSwitchCaseComparison; isComputed; result } ->
            `Assoc [ ("step", `String "binarypost"); ("iid", `Int iid); ("op", `String op);
            ("left", dump_jsval left); ("right", dump_jsval right);
            ("isOpAssign", `Bool isOpAssign); ("isComputed", `Bool isComputed);
            ("isSwitchComparison", `Bool isSwitchCaseComparison);
            ("result", dump_jsval result) ]
    | UnaryPre { iid; op; arg } ->
            `Assoc [ ("step", `String "unarypre"); ("iid", `Int iid);
            ("op", `String op); ("left", dump_jsval arg) ]
    | UnaryPost { iid; op; arg; result } ->
            `Assoc [ ("step", `String "unarypost"); ("iid", `Int iid);
            ("op", `String op); ("left", dump_jsval arg); ("result", dump_jsval result) ]
    | EndExpression iid ->
            `Assoc [ ("step", `String "exprend"); ("iid", `Int iid) ]
    | Conditional { iid; value } ->
            `Assoc [ ("step", `String "conditional"); ("iid", `Int iid);
                ("result", dump_jsval value) ]

let dump_functions funs =
    `List (funs |> Array.map dump_funcspec |> Array.to_list)
let dump_objects objs =
    `List (objs |> Array.map dump_objectspec |> Array.to_list)
let dump_trace trace =
    `List (trace |> List.map dump_operation)
let dump_global_desc { id; obj; proto }: json =
  `Assoc [
     ("id", dump_jsval id);
     ("obj_data", dump_objectspec obj);
     ("proto_data", dump_objectspec proto) ]
let dump_globals globals: json =
  `Assoc (globals |> Misc.StringMap.map dump_global_desc |> Misc.StringMap.bindings)
  
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
let pp_jsval pp = function
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
      if x.isConstructor && x.isMethod then fprintf pp "Calling constructor %a on %a with %a" pp_jsval x.f pp_jsval x.base pp_jsval x.args
      else if x.isConstructor then fprintf pp "Calling constructor %a using base %a on %a" pp_jsval x.f pp_jsval x.base pp_jsval x.args
      else if x.isMethod then fprintf pp "Calling function %a on %a with %a" pp_jsval x.f pp_jsval x.base pp_jsval x.args
      else fprintf pp "Calling function %a using base %a on %a" pp_jsval x.f pp_jsval x.base pp_jsval x.args
  | FunPost x ->
      if x.isConstructor && x.isMethod then fprintf pp "Called constructor %a on %a with %a, returns %a" pp_jsval x.f pp_jsval x.base pp_jsval x.args pp_jsval x.result
      else if x.isConstructor then fprintf pp "Called constructor %a using base %a on %a, returns %a" pp_jsval x.f pp_jsval x.base pp_jsval x.args pp_jsval x.result
      else if x.isMethod then fprintf pp "Called function %a on %a with %a, returns %a" pp_jsval x.f pp_jsval x.base pp_jsval x.args pp_jsval x.result
      else fprintf pp "Called function %a using base %a on %a, returns %a" pp_jsval x.f pp_jsval x.base pp_jsval x.args pp_jsval x.result
  | Literal x -> fprintf pp "Literal %a%s" pp_jsval x.value (if x.hasGetterSetter then " (has getter and setter)" else "")
  | ForIn x -> fprintf pp "for (... in %a)" pp_jsval x.value
  | Declare x ->
      begin match x.argument with
        | Some i -> fprintf pp "argument def: %s from %d = %a" x.name i pp_jsval x.value
        | None ->
            if x.isCatchParam then
              fprintf pp "catch %s = %a" x.name pp_jsval x.value
            else
              fprintf pp "var %s = %a" x.name pp_jsval x.value
      end
  | GetFieldPre x ->
      if x.isOpAssign then
        fprintf pp "Reading %a.%s (for assign-and-modify)" pp_jsval x.base x.offset
      else if x.isMethodCall then
        fprintf pp "Reading %a.%s (for method call)" pp_jsval x.base x.offset
      else
        fprintf pp "Reading %a.%s" pp_jsval x.base x.offset
  | GetField x ->
      if x.isOpAssign then
        fprintf pp "Reading %a.%s gives %a (for assign-and-modify)" pp_jsval x.base x.offset pp_jsval x.value
      else if x.isMethodCall then
        fprintf pp "Reading %a.%s gives %a (for method call)" pp_jsval x.base x.offset pp_jsval x.value
      else
        fprintf pp "Reading %a.%s gives %a" pp_jsval x.base x.offset pp_jsval x.value
  | PutFieldPre x ->
      if x.isOpAssign then
        fprintf pp "Writing %a to %a.%s (pre, for assign-and-modify)" pp_jsval x.value pp_jsval x.base x.offset
      else
        fprintf pp "Writing %a to %a.%s (pre)" pp_jsval x.value pp_jsval x.base x.offset
  | PutField x ->
      if x.isOpAssign then
        fprintf pp "Writing %a to %a.%s (for assign-and-modify)" pp_jsval x.value pp_jsval x.base x.offset
      else
        fprintf pp "Writing %a to %a.%s" pp_jsval x.value pp_jsval x.base x.offset
  | Read x ->
    fprintf pp "Reading %s (global=%B, scriptLocal=%B) gives %a"
      x.name x.isGlobal x.isScriptLocal pp_jsval x.value
  | Write x ->
    fprintf pp "Writing to %s (global=%B, scriptLocal=%B), new value: %a"
      x.name x.isGlobal x.isScriptLocal pp_jsval x.value 
  | Return x -> fprintf pp "return %a" pp_jsval x.value
  | Throw x -> fprintf pp "throw %a" pp_jsval x.value
  | With x -> fprintf pp "with %a" pp_jsval x.value
  | FunEnter x -> fprintf pp "Entering %a with base %a and arguments %a" pp_jsval x.f pp_jsval x.this pp_jsval x.args
  | FunExit x -> fprintf pp "Exiting function, return %a and exception %a" pp_jsval x.ret pp_jsval x.exc
  | ScriptEnter -> pp_print_string pp "script entry"
  | ScriptExit -> pp_print_string pp "script exit"
  | ScriptExc exc -> fprintf pp "script exit with exception %a" pp_jsval exc
  | BinPre x ->
      if x.isOpAssign then
        fprintf pp "%a %s= %a" pp_jsval x.left x.op pp_jsval x.right
      else if x.isSwitchCaseComparison then
        fprintf pp "case %a %s %a" pp_jsval x.left x.op pp_jsval x.right
      else
        fprintf pp "%a %s %a" pp_jsval x.left x.op pp_jsval x.right
  | BinPost x ->
      if x.isOpAssign then
        fprintf pp "%a %s= %a returns‡’ %a" pp_jsval x.left x.op pp_jsval x.right pp_jsval x.result
      else if x.isSwitchCaseComparison then
        fprintf pp "case %a %s %a âreturns %a" pp_jsval x.left x.op pp_jsval x.right pp_jsval x.result
      else
        fprintf pp "%a %s %a â‡’ %a" pp_jsval x.left x.op pp_jsval x.right pp_jsval x.result
  | UnaryPre x -> fprintf pp "%s %a" x.op pp_jsval x.arg
  | UnaryPost x -> fprintf pp "%s %a returns‡’ %a" x.op pp_jsval x.arg pp_jsval x.result
  | EndExpression iid -> pp_print_string pp "(end of expression)"
  | Conditional v -> fprintf pp "end of conditional, %a" pp_jsval v.value

let pp_trace pp trace =
  pp_open_vbox pp 0;
  List.iter (fprintf pp "@[<h>%a@];@ " pp_operation) trace;
  pp_close_box pp ()

let pp_fieldspec pp { value; set; get; writable; enumerable; configurable } =
    (* Special-case the most common case *)
    if writable && enumerable && configurable && set = None && get = None then
        pp_jsval pp value
    else if set = None && get = None then
        fprintf pp "[%s%s%s] %a"
          (if writable then "W" else "-")
          (if enumerable then "E" else "-")
          (if configurable then "C" else "-")
          pp_jsval value
    else
        fprintf pp "[%s%s%s] %a { get = %a, set = %a }"
          (if writable then "W" else "-")
          (if enumerable then "E" else "-")
          (if configurable then "C" else "-")
          pp_jsval value
          (FormatHelper.pp_print_option pp_jsval) get
          (FormatHelper.pp_print_option pp_jsval) set

let pp_objectspec pp spec =
  pp_open_hovbox pp 0;
  pp_print_string pp "{";
  StringMap.iter (fun fld value -> fprintf pp "@[<hov>%s â†’ %a;@]" fld pp_fieldspec value) spec;
  pp_print_string pp "}";
  pp_close_box pp ()

let pp_objects pp arr =
  pp_open_vbox pp 0;
  Array.iteri (fun i s -> fprintf pp "%i â†¦ %a;@ " i pp_objectspec s) arr;
  pp_close_box pp ()
let pp_local_funcspec pp s =
  fprintf pp "@[<hov>@ uninstrumented code: @[<hov>%s@]@]" s.uninstrumented
let pp_funcspec pp = function
  | Local s -> pp_local_funcspec pp s
  | External id -> fprintf pp "(external code, id=%d)" id
let pp_functions pp arr =
  pp_open_vbox pp 0;
  Array.iteri (fun i s -> fprintf pp "%i â†¦ %a;@ " i pp_funcspec s) arr;
  pp_close_box pp ()
let pp_global_spec pp { id; obj; proto } =
  fprintf pp "%a, object=%a, proto=%a" pp_jsval id pp_objectspec obj pp_objectspec proto
let pp_globals pp spec =
  pp_open_hovbox pp 0;
  pp_print_string pp "{";
  StringMap.iter (fun fld value -> fprintf pp "@[<hov>%s â†’ %a;@]" fld pp_global_spec value) spec;
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
