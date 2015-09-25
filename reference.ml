open Format;;
open Trace;;

type objectid =
    | Object of int
    | Function of int * int
    | Other of string * int

type fieldref = objectid * string;;

type reference =
    | LocalVariable of string
    | GlobalVariable of string
    | Field of objectid * string;;

let reference_compare r1 r2 = match (r1, r2) with
    | (LocalVariable v1, LocalVariable v2) -> compare v1 v2
    | (LocalVariable v1, GlobalVariable v2) -> -1
    | (LocalVariable v1, Field (o2, f2)) -> -1
    | (GlobalVariable v1, LocalVariable v2) -> 1
    | (GlobalVariable v1, GlobalVariable v2) -> compare v1 v2
    | (GlobalVariable v1, Field (o2, f2)) -> -1
    | (Field (o1, f1), LocalVariable v2) -> 1
    | (Field (o1, f1), GlobalVariable v2) -> 1
    | (Field (o1, f1), Field (o2, f2)) -> match compare o1 o2 with
        | 0 -> compare f1 f2
        | c -> c

let objectid_to_jsval = function
    | Object o -> OObject o
    | Function (o, f) -> OFunction (o, f)
    | Other (t, o) -> OOther (t, o)

let objectid_of_jsval = function
    | OObject o -> Object o
    | OFunction (o, f) -> Function (o, f)
    | OOther (t, o) -> Other (t, o)
    | _ -> failwith "Not an object"

let pp_objectid pp id = pp_jsval pp (objectid_to_jsval id)

let pp_fieldref pp (obj, name) = fprintf pp "%a@%s" pp_objectid obj name

let pp_reference pp = function
    | LocalVariable v -> fprintf pp "%s" v
    | GlobalVariable v -> fprintf pp "global:%s" v
    | Field(obj, name) -> pp_fieldref pp (obj, name)

module Reference = struct
    type t = reference
    let compare = reference_compare
end;;
module ReferenceMap = Map.Make(Reference)
module ReferenceMapFormat = FormatHelper.MapFormat(ReferenceMap)

let pp_reference_map fmt =
    ReferenceMapFormat.pp_print_map "" "" ","
        (fun pp ref data -> fprintf pp "%a: %a" pp_reference ref fmt data)

let global_object = Object 0
let reference_of_name globals_are_properties aliases global name =
    if global then
        if globals_are_properties then
            Field(global_object, name)
        else
            GlobalVariable name
    else if Misc.StringMap.mem name aliases then
        let (obj, fld) = Misc.StringMap.find name aliases in Field(obj, fld)
    else LocalVariable name
let reference_of_field base offset = Field (objectid_of_jsval base, offset)
let reference_of_fieldref (base, offset) = Field (base, offset)
let reference_of_local_name name = LocalVariable name
let get_fieldref = function
    | Field(obj, fld) -> Some (obj, fld)
    | _ -> None
let is_global = function
    | GlobalVariable _ -> true
    | _ -> false
let get_name = function
    | GlobalVariable name | LocalVariable name -> Some name
    | _ -> None

let regex_field = Str.regexp "^\\(.*\\)@\\(.*\\)$"
let regex_global = Str.regexp "^global:\\(.*\\)$"
let regex_function = Str.regexp "^\\([0-9]*\\)/\\([0-9]*\\)$"
let regex_other = Str.regexp "^\\(.*\\)/\\([0-9]*\\)$"

let fieldref_of_string str =
    if Str.string_match regex_function str 0 then
        Function (Str.matched_group 1 str |> int_of_string,
            Str.matched_group 2 str |> int_of_string)
    else if Str.string_match regex_other str 0 then
        Other(Str.matched_group 1 str, Str.matched_group 2 str |> int_of_string)
    else
        Object (Str.matched_group 1 str |> int_of_string)

let parse_reference str =
    if Str.string_match regex_field str 0 then
        Field (Str.matched_group 1 str |> fieldref_of_string,
            Str.matched_group 2 str)
    else if Str.string_match regex_global str 0 then
        GlobalVariable (Str.matched_group 1 str)
    else
        LocalVariable str

type versioned_reference = reference * int
let pp_versioned_reference pp (ref, ver) =
    Format.fprintf pp "%a:%d" pp_reference ref ver

module VersionReference = struct
    type t = versioned_reference
    let compare: t -> t -> int = Pervasives.compare
end;;
module VersionReferenceMap = Map.Make(VersionReference);;
module VersionReferenceSet = Set.Make(VersionReference);;

let get_object_id = function
    | Object id | Function (id, _) | Other (_, id) -> id
