open Misc
open Types

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

type trace = event list
type tracefile = functions * objects * trace * globals * bool

open Yojson.Basic;;
open Yojson.Basic.Util;;

exception ParseError
let report err msg json = Format.eprintf "%s - %s: %s@." err msg (Yojson.Basic.to_string json); raise ParseError
  
let to_string err json =
  try to_string json with Type_error (msg, json) -> report err msg json

let parse_jsval json =
    try
        match member "type" json |> to_string "Value type" with
        | "undefined" -> OUndefined
        | "boolean" -> OBoolean (member "val" json |> to_string "Value" |> bool_of_string)
        | "number" ->
            let numstr = member "val" json |> to_string "Value" in begin
                try ONumberInt (int_of_string numstr)
                with Failure "int_of_string" ->
                    try ONumberFloat (float_of_string numstr)
                    with Failure "float_of_string" ->
                        failwith ("Strange number here: " ^ Yojson.Basic.to_string json)
            end
        | "string" -> OString (member "val" json |> to_string "Value")
        | "symbol" -> OSymbol (member "val" json |> to_string "Value")
        | "function" -> OFunction (member "id" json |> to_int, member "funid" json |> to_int)
        | "null" -> ONull
        | "object" -> OObject (member "id" json |> to_int)
        | _ as ty -> OOther (ty, member "id" json |> to_int)
    with
      | ParseError -> report "Context" "jsval" json

let native_pattern = Str.regexp_string "[native code]"
let parse_funcspec json =
    let instr = member "instrumented" json |> to_string "Function specification" in
    if (Str.string_match native_pattern instr 0)
    then External (json |> member "obj" |> to_int)
    else Local { from_toString = instr; from_jalangi = json |> member "uninstrumented" |> to_string_option }
    
let parse_fieldspec json =
    let default_to d = function Some x -> x | None -> d in try
    { value = json |> member "value" |> to_option parse_jsval |> default_to OUndefined;
        writable = json |> member "writable" |> to_bool_option |> default_to true;
        enumerable = json |> member "enumerable" |> to_bool_option |> default_to true;
        configurable = json |> member "configurable" |> to_bool_option |> default_to true;
        get = json |> member "get" |> to_option parse_jsval;
        set = json |> member "set" |> to_option parse_jsval
    }
    with ParseError -> report "Context" "fieldspec" json
    
    
let parse_objectspec json =
  try
    json |> to_assoc |>
    List.fold_left
        (fun spec (name, content) ->
                StringMap.add name (parse_fieldspec content) spec)
        StringMap.empty
    with ParseError -> report "Context" "objectspec" json

let parse_operation json =
    let get_int key = member key json |> to_int
    and get_string key = member key json |> to_string key
    and get_jsval key = try member key json |> parse_jsval with e -> Format.eprintf "Can't find jsval %s@." key; raise e
    and get_bool key = try member key json |> to_bool with e -> Format.eprintf "Can't find bool %s@." key; raise e in
    try
    match member "step" json |> to_string "Step" with
    | "funpre" -> FunPre {
            iid = get_int "iid";
            f = get_jsval "f";
            base = get_jsval "base";
            args = get_jsval "args";
            isConstructor = get_bool "isConstructor";
            isMethod = get_bool "isMethod"
        }
    | "funpost" -> FunPost {
            iid = get_int "iid";
            f = get_jsval "f";
            base = get_jsval "base";
            args = get_jsval "args";
            isConstructor = get_bool "isConstructor";
            isMethod = get_bool "isMethod";
            result = get_jsval "result"
        }
    | "literal" -> Literal {
            iid = get_int "iid";
            value = get_jsval "val";
            hasGetterSetter = get_bool "hasGetterSetter"
        }
    | "forin" -> ForIn {
            iid = get_int "iid";
            value = get_jsval "val"
        }
    | "declare" -> Declare {
            iid = get_int "iid";
            name = get_string "name";
            value = get_jsval "val";
            isCatchParam = get_bool "isCatchParam";
            argument = if get_bool "isArgument"
                then Some (get_int "argumentIndex") else None
        }
    | "getpre" -> GetFieldPre {
            iid = get_int "iid";
            base = get_jsval "base";
            offset = get_string "offset";
            isComputed = get_bool "isComputed";
            isOpAssign = get_bool "isOpAssign";
            isMethodCall = get_bool "isMethodCall"
        }
    | "getpost" -> GetField {
            iid = get_int "iid";
            base = get_jsval "base";
            offset = get_string "offset";
            isComputed = get_bool "isComputed";
            isOpAssign = get_bool "isOpAssign";
            isMethodCall = get_bool "isMethodCall";
            value = get_jsval "val"
        }
    | "putpre" -> PutFieldPre {
            iid = get_int "iid";
            base = get_jsval "base";
            offset = get_string "offset";
            isComputed = get_bool "isComputed";
            isOpAssign = get_bool "isOpAssign";
            value = get_jsval "val"
        }
    | "putpost" -> PutField {
            iid = get_int "iid";
            base = get_jsval "base";
            offset = get_string "offset";
            isComputed = get_bool "isComputed";
            isOpAssign = get_bool "isOpAssign";
            value = get_jsval "val"
        }
    | "read" -> Read {
            iid = get_int "iid";
            name = get_string "name";
            value = get_jsval "val";
            isGlobal = get_bool "isGlobal";
            isScriptLocal = get_bool "isScriptLocal"
        }
    | "write" -> Write {
            iid = get_int "iid";
            name = get_string "name";
            value = get_jsval "val";
            lhs = get_jsval "lhs";
            isGlobal = get_bool "isGlobal";
            isScriptLocal = get_bool "isScriptLocal"
        }
    | "return" -> Return {
            iid = get_int "iid";
            value = get_jsval "val"
        }
    | "throw" -> Throw {
            iid = get_int "iid";
            value = get_jsval "val"
        }
    | "with" -> With {
            iid = get_int "iid";
            value = get_jsval "val"
        }
    | "funcenter" -> FunEnter {
            iid = get_int "iid";
            f = get_jsval "f";
            this = get_jsval "this";
            args = get_jsval "args"
        }
    | "funcexit" -> FunExit {
            iid = get_int "iid";
            ret = get_jsval "ret";
            exc = get_jsval "exc"
        }
    | "scriptenter" -> ScriptEnter
    | "scriptexit" -> ScriptExit
    | "scriptexc" -> ScriptExc (get_jsval "exc")
    | "binarypre" -> BinPre {
            iid = get_int "iid";
            op = get_string "op";
            left = get_jsval "left";
            right = get_jsval "right";
            isOpAssign = get_bool "isOpAssign";
            isSwitchCaseComparison =
                get_bool "isSwitchComparison";
            isComputed = get_bool "isComputed"
        }
    | "binarypost" -> BinPost {
            iid = get_int "iid";
            op = get_string "op";
            left = get_jsval "left";
            right = get_jsval "right";
            isOpAssign = get_bool "isOpAssign";
            isSwitchCaseComparison =
                get_bool "isSwitchComparison";
            isComputed = get_bool "isComputed";
            result = get_jsval "result"
        }
    | "unarypre" -> UnaryPre {
            iid = get_int "iid";
            op = get_string "op";
            arg = get_jsval "left"
        }
    | "unarypost" -> UnaryPost {
            iid = get_int "iid";
            op = get_string "op";
            arg = get_jsval "left";
            result = get_jsval "result"
        }
    | "exprend" -> EndExpression (get_int "iid")
    | "conditional" -> Conditional {
            iid = get_int "iid";
            value = get_jsval "result"
        }
    | _ as op -> failwith ("Unknown event " ^ op)
    with ParseError -> report "Context" "event" json

let parse_functions json =
    try json |> convert_each parse_funcspec |> Array.of_list
    with ParseError -> report "Context" "functions" json

let parse_objects json =
    try json |> convert_each parse_objectspec |> Array.of_list
    with ParseError -> report "Context" "objects" json

let parse_trace json =
    try json |> convert_each parse_operation
    with ParseError -> report "Context" "trace" json

let parse_global_value json = (* Backwards compatibility! *)
  match member "id" json with
    | `Null -> parse_jsval json
    | `Int _ -> parse_jsval json
    | json' -> parse_jsval json'


let parse_globals json: globals =
    let module Extra = Misc.MapExtra(Misc.StringMap) in
    try
    json |> to_assoc |> Extra.of_list |>
    Misc.StringMap.map parse_global_value
    with ParseError -> report "Context" "globals" json

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
    | OString s -> `Assoc [ ("type", `String "string"); ("val", `String s) ]
    | OSymbol s -> `Assoc [ ("type", `String "symbol"); ("val", `String s) ]
    | ONull -> `Assoc [ ("type", `String "null") ]
    | OObject id -> `Assoc [ ("type", `String "object"); ("id", `Int id) ]
    | OOther (ty, id) -> `Assoc [ ("type", `String ty); ("id", `Int id) ]
    | OFunction (id, fid) -> `Assoc [ ("type", `String "function"); ("id", `Int id); ("funid", `Int fid) ]

let dump_funcspec = function
    | External id -> `Assoc [ ("instrumented", `String "[native code]"); ("obj", `Int id) ]
    | Local { from_toString; from_jalangi = Some from_jalangi } ->
        `Assoc [ ("instrumented", `String from_toString); ("uninstrumented", `String from_jalangi) ]
    | Local { from_toString; from_jalangi = None } ->
        `Assoc [ ("instrumented", `String from_toString) ]
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
    | Declare { iid; name; value; isCatchParam; argument = Some arg } ->
        `Assoc [ ("step", `String "declare"); ("iid", `Int iid);
        ("val", dump_jsval value); ("name", `String name);
        ("isCatchParam", `Bool isCatchParam);
        ("isArgument", `Bool true); ("argumentIndex", `Int arg) ]
    | Declare { iid; name; value; isCatchParam; argument = None } ->
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
let dump_global_desc id: json = dump_jsval id
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

let pp_operation pp = function
    | FunPre { isConstructor; isMethod; f; base; args } ->
        fprintf pp "FunPre(f=%a, base=%a, args=%a, isConstructor=%b, isMethod = %b"
            pp_jsval f pp_jsval base pp_jsval args isConstructor isMethod
    | FunPost { isConstructor; isMethod; f; base; args; result } ->
        fprintf pp "FunPost(f=%a, base=%a, args=%a, result=%a, isConstructor=%b, isMethod = %b"
            pp_jsval f pp_jsval base pp_jsval args pp_jsval result isConstructor isMethod
    | Literal { value; hasGetterSetter } -> fprintf pp "Literal(value=%a, hasGetterSetter=%b)" pp_jsval value hasGetterSetter
    | ForIn { value } -> fprintf pp "ForIn(value=%a)" pp_jsval value
    | Declare { name; value; argument; isCatchParam } ->
        fprintf pp "Declare(name=%s, value=%a, argument=%a, isCatchParam=%b)"
            name pp_jsval value (FormatHelper.pp_print_option Format.pp_print_int) argument isCatchParam
    | GetFieldPre { base; offset; isOpAssign; isMethodCall } ->
        fprintf pp "GetFieldPre(base=%a, offset=%s, isOpAssign=%b, isMethodCall=%b)"
            pp_jsval base offset isOpAssign isMethodCall
    | GetField { base; offset; value; isOpAssign; isMethodCall } ->
        fprintf pp "GetFieldPre(base=%a, offset=%s, result=%a, isOpAssign=%b, isMethodCall=%b)"
            pp_jsval base offset pp_jsval value isOpAssign isMethodCall
    | PutFieldPre { value; base; offset; isOpAssign } ->
        fprintf pp "PutFieldPre(base=%a, offset=%s, value=%a, isOpAssign=%b"
            pp_jsval base offset pp_jsval value isOpAssign
    | PutField { value; base; offset; isOpAssign } ->
        fprintf pp "PutField(base=%a, offset=%s, value=%a, isOpAssign=%b"
            pp_jsval base offset pp_jsval value isOpAssign
    | Read { name; isGlobal; isScriptLocal; value } ->
        fprintf pp "Read(name=%s, value=%a, isGlobal=%b, isScriptLocal=%b"
            name pp_jsval value isGlobal isScriptLocal
    | Write { name; isGlobal; isScriptLocal; value } ->
        fprintf pp "Write(name=%s, value=%a, isGlobal=%b, isScriptLocal=%b"
            name pp_jsval value isGlobal isScriptLocal
    | Return { value } -> fprintf pp "Return(%a)" pp_jsval value
    | Throw { value } -> fprintf pp "Throw(%a)" pp_jsval value
    | With { value } -> fprintf pp "With(%a)" pp_jsval value
    | FunEnter { f; this; args } -> fprintf pp "FunEnter(f=%a, this=%a, args=%a)" pp_jsval f pp_jsval this pp_jsval args
    | FunExit { ret; exc } -> fprintf pp "FunExit(ret=%a, exc=%a)" pp_jsval ret pp_jsval exc
    | ScriptEnter -> pp_print_string pp "ScriptEnter"
    | ScriptExit -> pp_print_string pp "ScriptExit"
    | ScriptExc exc -> fprintf pp "script exit with exception %a" pp_jsval exc
    | BinPre { left; right; op; isOpAssign; isSwitchCaseComparison } ->
        fprintf pp "BinPre(left=%a, op=%s, right=%a, isOpAssign=%b, isSwitchCaseComparison=%b"
            pp_jsval left op pp_jsval right isOpAssign isSwitchCaseComparison
    | BinPost { left; right; op; result; isOpAssign; isSwitchCaseComparison } ->
        fprintf pp "BinPost(left=%a, op=%s, right=%a, result=%a, isOpAssign=%b, isSwitchCaseComparison=%b"
            pp_jsval left op pp_jsval right pp_jsval result isOpAssign isSwitchCaseComparison
    | UnaryPre { op; arg } ->
        fprintf pp "UnaryPre(op=%s, arg=%a)" op pp_jsval arg
    | UnaryPost { op; arg; result } ->
        fprintf pp "UnaryPost(op=%s, arg=%a, result=%a)" op pp_jsval arg pp_jsval result
    | EndExpression iid -> pp_print_string pp "EndExpression"
    | Conditional { value } -> fprintf pp "Conditional(value=%a)" pp_jsval value

let pp_trace pp trace =
    pp_open_vbox pp 0;
    List.iter (fprintf pp "@[<h>%a@];@ " pp_operation) trace;
    pp_close_box pp ()

let pp_tracefile pp (f, o, t, g, gap) =
    fprintf pp "@[<v>Globals are properties: %b@ @[<hov>%a@]@ @[<hov>%a@]@ @[<hov>Globals:@ %a@]@ Trace:@ @[<hov>%a@]@]"
        gap pp_functions f pp_objects o pp_globals g pp_trace t

