open LocalFacts
open Reference
open Types
module StringMap = Misc.StringMap

type saved_variable =
    | Unknown
    | Version of int
    | Alias of fieldref

type version_state = {
    save_stack: saved_variable StringMap.t list;
    versions_bound: int ReferenceMap.t;
    versions_current: int ReferenceMap.t;
    aliases: fieldref StringMap.t;
    last_update: versioned_reference option
}

let save_version state name =
    match state.save_stack with
    | [] -> state (* var at toplevel *)
    | fr :: stack ->
        let lv = reference_of_local_name name in
        let saveval = begin if ReferenceMap.mem lv state.versions_current
                then begin
                    assert (not (StringMap.mem name state.aliases));
                    Version (ReferenceMap.find lv state.versions_current)
                end else if StringMap.mem name state.aliases then
                    let ref = StringMap.find name state.aliases in
                    Alias ref
                else Unknown end in
        { state with
            save_stack = StringMap.add name saveval fr :: stack;
            versions_current = ReferenceMap.remove lv state.versions_current;
            aliases = StringMap.remove name state.aliases }

let push state = { state with save_stack = StringMap.empty :: state.save_stack }
let pop state =
    match state.save_stack with
    | [] -> failwith "Pop on empty stack"
    | frame :: stack ->
        StringMap.fold (fun name save state ->
                    let lv = reference_of_local_name name in
                    match save with
                    | Unknown ->
                        { state with versions_current =
                                ReferenceMap.remove lv state.versions_current;
                            aliases = StringMap.remove name state.aliases }
                    | Alias (obj, fld) ->
                        { state with versions_current =
                                ReferenceMap.remove lv state.versions_current;
                            aliases = StringMap.add name (obj, fld) state.aliases }
                    | Version v ->
                        { state with versions_current =
                                ReferenceMap.add lv v state.versions_current;
                            aliases = StringMap.remove name state.aliases })
            frame { state with save_stack = stack }

let increment_reference state ref =
    let ver =
        try ReferenceMap.find ref state.versions_current with Not_found -> 0
    and ver' =
        try ReferenceMap.find ref state.versions_bound + 1 with Not_found -> 0
    in { state with
        versions_bound = ReferenceMap.add ref ver' state.versions_bound;
        versions_current = ReferenceMap.add ref ver' state.versions_current;
        last_update = Some (ref, ver)
    }

let warnings: string list ref = ref []

let provide_read ref state =
    if ReferenceMap.mem ref state.versions_current then
        state
    else
        increment_reference state ref

let provide_write (objects: objects) ref state =
    if ReferenceMap.mem ref state.versions_current then
        match ref with
        | Field (obj, fld) ->
        (* If the field is not writable, do nothing. *)
            begin try
                let objid = get_object_id obj in
                let fldspec = StringMap.find fld objects.(objid) in
                if fldspec.writable && fldspec.set = None then
                    increment_reference state ref
                else if fldspec.set = None then
                    state
                else begin
                    (* Set handlers can do whatever they like. Assume the
                     * update goes through, but warn about possible
                     * unsoundness. *)
                    let msg =
                        Format.sprintf "Writing to %a@%s with set handler"
                            (fun () -> Misc.to_string pp_objectid) obj fld
                    in warnings := msg :: !warnings;
                    increment_reference state ref
                end
            with Not_found ->
            (* A new field. TODO: Can objects prevent this from happening? *)
                increment_reference state ref
            end
        | GlobalVariable name ->
          (* Apparently, global variables may be read-only (e.g., console in
           * node.js. Since we cannot detect this as of now, just assume
           * it goes through and warn about possible unsoundness. *)
          let msg = Format.sprintf "Writing to global variable %s" name in
          warnings := msg :: !warnings;
          increment_reference state ref
        | LocalVariable _ ->
          increment_reference state ref
    else
        increment_reference state ref

let provide_object (objects: objects) state obj =
    (* Recurse over object structure, initialize all fields that have not
     * been seen yet. *)
    let rec recurse obj state =
        StringMap.fold (fun name field state ->
                    let ref = reference_of_fieldref (obj, name) in
                    if ReferenceMap.mem ref state.versions_current then
                        state
                    else
                        increment_reference state ref |> recurse_value field.Types.value)
            objects.(get_object_id obj) state
    and recurse_value field state = match field with
        | OObject _ | OOther _ | OFunction _ -> recurse (objectid_of_jsval field) state
        | _ -> state in
    recurse obj state

let provide_argument_alias objects state name arguments i =
    let field = string_of_int i in
    match arguments with
    | Some params when StringMap.mem field objects.(params) ->
        { state with aliases =
                StringMap.add name (Object params, field) state.aliases }
    | Some _ ->
    (* Argh. Javascript.
     * arguments reflects the *actual* parameters, while name bindings reflect
     * the *formal* parameters. Of course,  if there are less actual then formal
     * parameters, we cannot possibly name-bind some field in the arguments
     * object, can we? *)
        provide_write objects (reference_of_local_name name) state
    | None -> failwith "No arguments to alias!"
let provide_literal (objs: objects) state = function
    | (OFunction _ | OOther _ | OObject _) as o ->
        provide_object objs state (objectid_of_jsval o)
    | _ -> state

let collect_versions_step (objects: objects) globals_are_properties state arguments op =
    let nameref isGlobal =
        reference_of_name globals_are_properties state.aliases isGlobal in
    let res = let open Cleantrace in match op with
        | CFunPre { args } ->
            provide_literal objects state args
        | CLiteral { value } ->
            provide_literal objects state value
        | CDeclare { name; declaration_type = ArgumentBinding i } ->
            provide_argument_alias objects (save_version state name) name arguments i
        | CDeclare { name } ->
            save_version state name |>
            provide_write objects (reference_of_local_name name)
        | CGetField { base; offset } ->
            provide_read (reference_of_field base offset) state
        | CPutField { base; offset } ->
            provide_write objects (reference_of_field base offset) state
        | CRead { name; isGlobal } ->
            provide_read (nameref isGlobal name) state
        | CWrite { name; isGlobal } ->
            provide_write objects (nameref isGlobal name) state
        | CFunEnter { args } ->
            provide_literal objects (push state) args |>
            fun state -> save_version state "this" |>
                provide_write objects (reference_of_local_name "this")
        | CFunExit _ ->
            pop state
        | _ ->
            state in
    ( { last_arguments = arguments;
        versions = res.versions_current;
        aliases = res.aliases;
        last_update = res.last_update },
        res )

let initial_refs objects globals_are_properties globals =
    let reference_of_global = reference_of_name globals_are_properties StringMap.empty true in
    Misc.StringMap.fold (fun var id refs ->
                let refs = if var = "global" then refs else provide_read (reference_of_global var) refs in
                let refs = provide_object objects refs (objectid_of_jsval id) in
                refs)
        globals {
            save_stack = [];
            versions_bound = ReferenceMap.empty;
            aliases = StringMap.empty;
            versions_current = ReferenceMap.empty;
            last_update = None
        }

let collect_versions objects globals_are_properties globals tr =
    let init_refs = initial_refs objects globals_are_properties globals in
    let res =
        trace_enrich (collect_versions_step objects globals_are_properties)
            init_refs tr
    in if !warnings <> [] then begin
        prerr_endline "Possible unsoundness detected";
        List.iter prerr_endline !warnings
    end; res

let calculate_versions (funs, objs, trace, globals, globals_are_properties) =
    (funs, objs, collect_versions objs globals_are_properties globals trace,
        globals, globals_are_properties)
