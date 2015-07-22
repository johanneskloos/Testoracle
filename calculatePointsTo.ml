open Reference;;
open LocalFacts;;
open Misc;;
open Trace;;
open CalculateVersions;;
open PointsTo;;
open Cleantrace

let add_write (facts: local_facts) (state: points_to_map) (ref: reference) (value: jsval): points_to_map =
    let vref = make_versioned facts ref in
    if VersionReferenceMap.mem vref state then
        state (* This write was dropped; most likely, the field was marked "not writable". *)
    else
        VersionReferenceMap.add vref value state

let add_read (facts: local_facts) (state: points_to_map) (ref: reference) (value: jsval): points_to_map =
    let vref = make_versioned facts ref in
    if VersionReferenceMap.mem vref state then begin
        if (value <> VersionReferenceMap.find vref state) then begin
            Format.eprintf "Weirdness detected: In read of %a, expected %a, but got %a@."
                pp_reference ref pp_jsval (VersionReferenceMap.find vref state) pp_jsval value 
        end;
        state
    end else
        VersionReferenceMap.add vref value state

let add_known_new_object (objects: objects) (facts: local_facts) (state: points_to_map) (obj: jsval): points_to_map =
    let id = get_object obj in
    StringMap.fold (fun name (objspec: Trace.fieldspec) state ->
        add_write facts state (reference_of_fieldref (id, name)) objspec.value)
        objects.(id) state

let add_literal (objects: objects) (facts: local_facts) (state: points_to_map) (value: jsval): points_to_map =
    match value with
    | OObject id | OOther (_, id) | OFunction (id, _) ->
            StringMap.fold (fun name (objspec: Trace.fieldspec) state ->
                add_read facts state (reference_of_fieldref (id, name)) objspec.value)
            objects.(id) state
    | _ -> state

let collect_pointsto_step (globals_are_properties: bool) (objects: objects) (state: points_to_map) (facts: local_facts): clean_operation -> points_to_map = function
    | CFunPre { args } ->
            add_known_new_object objects facts state args
    | CLiteral { value } ->
            add_literal objects facts state value
    | CDeclare { declaration_type = ArgumentBinding _ } ->
            state
    | CDeclare { name; value } ->
            add_write facts state (reference_of_local_name name) value
    | CGetField { base; offset; value } ->
            add_read facts state (reference_of_field base offset) value
    | CPutField { base; offset; value } ->
            add_write facts state (reference_of_field base offset) value
    | CRead { name; value; isGlobal } ->
            add_read facts state (reference_of_variable globals_are_properties facts isGlobal name) value
    | CWrite { name; value; isGlobal } ->
            add_write facts state (reference_of_variable globals_are_properties facts isGlobal name) value
    | CFunEnter { args } ->
            add_known_new_object objects facts state args
    | _ -> state
let collect_pointsto globals_are_properties objects =
    trace_fold (collect_pointsto_step globals_are_properties objects) VersionReferenceMap.empty
let calculate_pointsto (funs, objs, trace, globs, gap) = collect_pointsto gap objs trace
