open Trace
open Format
open Types

let (_, objs, _, _, _) = Trace.parse_tracefile (open_in Sys.argv.(1))

type field = Get of string | Data of string * jsval

let fieldspec_translate name { value; get } (vertexspec, edgespec) =
    match get with
    | Some get -> (Get name :: vertexspec, edgespec)
    | None -> match value with
        | OObject id | OFunction (id, _) | OOther (_, id) ->
            (vertexspec, (name, id) :: edgespec)
        | _ ->
            (Data (name, value) :: vertexspec, edgespec)

let objectspec_translate id spec (vertices, edges) =
    let (labels, edges') =
        Misc.StringMap.fold fieldspec_translate spec ([], []) in
    ((id, labels) :: vertices,
        List.map (fun (name, id') -> (id, name, id')) edges' @ edges)

let foldi f a init =
    let acc = ref init in
    Array.iteri (fun i x -> acc := f i x !acc) a;
    !acc

let objects_translate objs =
    foldi objectspec_translate objs ([], [])

let typename = function
    | OUndefined -> "undefined"
    | ONull -> "null"
    | OBoolean _ -> "boolean"
    | ONumberInt _ -> "int"
    | ONumberFloat _ -> "float"
    | OString _ -> "string"
    | OSymbol _ -> "symbol"
    | OObject _ -> "object"
    | OFunction _ -> "function"
    | OOther (ty, _) -> ty

let format_field pp = function
    | Get name -> fprintf pp "getter for %s" name
    | Data (name, value) -> fprintf pp "%s: %s" name (typename value)

let format_vertex pp (id, labels) =
    fprintf pp "v%d [label=\"%d\",tooltip=\"%a\"]@." id id
        (FormatHelper.pp_print_list format_field) labels

let format_edge pp (id, name, id') =
    fprintf pp "%d -> %d [label=\"%s\"]" id id' name

let (vertices, edges) = objects_translate objs;;
printf "@[<v>@[<v 2>digraph objects {@.%a@.%a@]@.}@]"
    (FormatHelper.pp_print_list_lines format_vertex) vertices
    (FormatHelper.pp_print_list_lines format_edge) edges

