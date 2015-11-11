open Types
module StringMap = Misc.StringMap
module VersionReferenceMap = Reference.VersionReferenceMap
module ReferenceMap = Reference.ReferenceMap
type points_to_map = PointsTo.points_to_map

let add_write (facts: LocalFacts.local_facts) (state: points_to_map) (ref: Reference.reference)
    (value: jsval): points_to_map =
    let vref = LocalFacts.make_versioned facts ref in
    if VersionReferenceMap.mem vref state then begin
        (* This write was dropped; most likely, the field was marked
         * "not writable". *)
        Format.eprintf
          "Weirdness detected: Write of %a failed@."
          Reference.pp_reference ref;
        state
    end else
        VersionReferenceMap.add vref value state

let add_read (facts: LocalFacts.local_facts) (state: points_to_map) (ref: Reference.reference)
    (value: jsval): points_to_map =
    let vref = LocalFacts.make_versioned facts ref in
    if VersionReferenceMap.mem vref state then begin
        if (value <> VersionReferenceMap.find vref state) then begin
            Format.eprintf
                "Weirdness detected: In read of %a, expected %a, but got %a@."
                Reference.pp_reference ref
                pp_jsval (VersionReferenceMap.find vref state)
                pp_jsval value
        end;
        state
    end else
        VersionReferenceMap.add vref value state

let add_known_new_object (objects: objects) (facts: LocalFacts.local_facts)
    (state: points_to_map) (obj: jsval): points_to_map =
    let id = objectid_of_jsval obj in
    StringMap.fold (fun name (objspec: fieldspec) state ->
                add_write facts state
                    (Reference.reference_of_fieldref (id, name))
                    objspec.value)
        objects.(get_object_id id) state

let add_literal (objects: objects) (facts: LocalFacts.local_facts)
    (state: points_to_map) (value: jsval): points_to_map =
    (* HACK use the fact that all references in state.versions should be *)
    (* mapped to find missing fields.  *)
    ReferenceMap.fold (fun ref ver state ->
                let vref = (ref, ver) in
                if VersionReferenceMap.mem vref state then
                    state
                else
                    match ref with
                    | Reference.Field (obj, field) ->
                        let objid = get_object_id obj in
                        let { Types.value } =
                            StringMap.find field objects.(objid)
                        in
                        VersionReferenceMap.add vref value state
                    | _ -> failwith "Unexpected unmapped variable")
        facts.LocalFacts.versions state

let is_alias { LocalFacts.aliases } name = StringMap.mem name aliases

let collect_pointsto_step (globals_are_properties: bool) (objects: objects)
    (state: points_to_map) facts: Cleantrace.clean_operation -> points_to_map = 
    let open Cleantrace in function
    | CFunPre { args } ->
        add_known_new_object objects facts state args
    | CLiteral { value } ->
        add_literal objects facts state value
    | CDeclare { name; declaration_type = ArgumentBinding _ }
    when is_alias facts name ->
        state
    | CDeclare { name; value } ->
    (* Note that this also catches ArgumentBinding cases where the name is *)
    (* not an alias. *)
        add_write facts state (Reference.reference_of_local_name name) value
    | CGetField { base; offset; value } ->
        add_read facts state (Reference.reference_of_field base offset) value
    | CPutField { base; offset; value } ->
        add_write facts state (Reference.reference_of_field base offset) value
    | CRead { name; value; isGlobal } ->
        let ref =
            LocalFacts.reference_of_variable globals_are_properties facts isGlobal name in
        add_read facts state ref value
    | CWrite { name; value; isGlobal } ->
        let ref =
            LocalFacts.reference_of_variable globals_are_properties facts isGlobal name in
        add_write facts state ref value
    | CFunEnter { args; this } ->
        let state = add_known_new_object objects facts state args in
        add_write facts state (Reference.reference_of_local_name "this") this
    | _ -> state

let globals_points_to (objects: objects) globals_are_properties
    (globals: globals) trace pt =
    match trace with
    | [] -> failwith "Empty trace"
    | (_, { LocalFacts.versions }) :: _ ->
        let step ref ver pt =
            let vref = (ref, ver)
            and value = let open Reference in match ref with
                | Field (obj, field) ->
                    begin try (StringMap.find field objects.(get_object_id obj)).value
                    with Not_found -> failwith ("Can't find field " ^ field ^ " of " ^ (Misc.to_string pp_objectid obj)) end
                | GlobalVariable name ->
                    begin try StringMap.find name globals
                    with Not_found -> failwith ("Can't  find global variable "^ name) end
                | LocalVariable _ ->
                    failwith "Unexpected local variable" in
            VersionReferenceMap.add vref value pt
        in
        ReferenceMap.fold step versions pt

let collect_pointsto (globals_are_properties: bool) (globals: globals) (objects: objects) (trace: LocalFacts.facts_trace) =
    begin
        VersionReferenceMap.empty |>
        VersionReferenceMap.add (Reference.reference_of_local_name "this", 0) (OObject 0) |>
        globals_points_to objects globals_are_properties globals trace |>
        LocalFacts.trace_fold (collect_pointsto_step globals_are_properties objects)
    end trace
    
let calculate_pointsto (funs, objs, trace, globs, gap) = collect_pointsto gap globs objs trace
