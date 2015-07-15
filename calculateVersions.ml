open LocalFacts
open Misc
open Reference
open Trace

type saved_variable =
    | Unknown
    | Version of int
    | Alias of int * string

type version_state = {
    save_stack: saved_variable StringMap.t list;
    versions_bound: int ReferenceMap.t;
    versions_current: int ReferenceMap.t;
    aliases: (int * string) StringMap.t;
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
            let (obj, fld) = StringMap.find name state.aliases in
            Alias (obj, fld)
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
        match get_fieldref ref with
        | Some (obj, fld) ->
            (* If the field is not writable, do nothing. *)
            begin try
                let fldspec = StringMap.find fld objects.(obj) in
                if fldspec.writable && fldspec.set = None then
                    increment_reference state ref
                else if fldspec.set = None then
                    state
                else begin
                    (* Put handlers can do whatever they like. Assume the
                     * update goes through, but warn about possible
                     * unsoundness. *)
                    let msg =
                        Format.sprintf "Writing to %d@%s with set handler"
                        obj fld
                    in warnings := msg :: !warnings;
                    increment_reference state ref
                end
            with Not_found ->
                (* A new field.
                 * TODO: Can objects prevent this from happening? *)
                increment_reference state ref
            end
        | None ->
            (* Apparently, global variables may be read-only
             * (e.g., console in node.js).
             * Since we cannot detect this as of now, just assume it
             * goes through and warn about possible unsoundness. *)
            if is_global ref then begin
                let name = get_name ref |> Misc.Option.some in
                let msg =
                    Format.sprintf "Writing to global variable %s" name in
                warnings := msg :: !warnings
            end;
            increment_reference state ref
    else
        increment_reference state ref
    
let provide_object objects state obj =
    let obj = get_object obj in
    StringMap.fold
        (fun name field state ->
            provide_write objects (reference_of_fieldref (obj, name)) state)
        objects.(obj) state

let provide_argument_alias state name facts i =
    match facts.last_parameters with
    | Some params ->
            { state with aliases =
                StringMap.add name (params, string_of_int i) state.aliases }
    | None -> failwith "No arguments to alias!"
let provide_literal objs state = function
    | (OFunction _|OOther _|OObject _) as o ->
            provide_object objs state o
    | _ -> state

let collect_versions_step objects globals_are_properties state facts op =
    let nameref = reference_of_name globals_are_properties state.aliases in
    let res = match op with
    | FunPre { args } ->
            provide_object objects state args
    | Literal { value } ->
            provide_literal  objects state value
    | Declare { name; argument = Some i } when i >= 0 ->
            provide_argument_alias (save_version state name) name facts i
    | Declare { name } ->
            provide_write objects
                (reference_of_local_name name)
                (save_version state name)
    | GetField { base; offset } ->
            provide_read (reference_of_field base offset) state
    | PutField { base; offset } ->
            provide_write objects (reference_of_field base offset) state 
    | Read { name; isGlobal } ->
            provide_read (nameref isGlobal name) state
    | Write { name; isGlobal } ->
            provide_write objects (nameref isGlobal name) state
    | FunEnter { args } ->
            provide_object objects (push state) args
    | FunExit _ ->
            pop state
    | _ ->
            state in
    ( { facts with
        versions = res.versions_current;
        aliases = res.aliases;
        last_update = res.last_update },
      res )

let collect_versions objects globals_are_properties tr =
    let res =
        trace_enrich (collect_versions_step objects globals_are_properties)
        { save_stack = [];
          versions_bound = ReferenceMap.empty;
          aliases = StringMap.empty; versions_current = ReferenceMap.empty;
          last_update = None
        }
        tr
    in if !warnings <> [] then begin
        prerr_endline "Possible unsoundness detected";
        List.iter prerr_endline !warnings
    end; res

let calculate_versions (funs, objs, trace, globals, globals_are_properties) =
    (funs, objs, collect_versions objs globals_are_properties trace,
     globals, globals_are_properties)
