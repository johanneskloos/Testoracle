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
    (* HACK use the fact that all references in state.versions should be mapped to find missing fields. *)
    ReferenceMap.fold (fun ref ver state ->
        let vref = (ref, ver) in
        if VersionReferenceMap.mem vref state then
           state
        else
          match Reference.get_fieldref ref with
            | Some (obj, field) ->
              let value = (StringMap.find field objects.(obj)).Trace.value in
              VersionReferenceMap.add vref value state
            | None -> failwith "Unexpected unmapped variable")
        facts.versions state

let is_alias { aliases } name = StringMap.mem name aliases

let collect_pointsto_step (globals_are_properties: bool) (objects: objects) (state: points_to_map) (facts: local_facts): clean_operation -> points_to_map = function
    | CFunPre { args } ->
            add_known_new_object objects facts state args
    | CLiteral { value } ->
            add_literal objects facts state value
    | CDeclare { name; declaration_type = ArgumentBinding _ } when is_alias facts name ->
            state
    | CDeclare { name; value } ->
            (* Note that this also catches ArgumentBinding cases where the name is not an alias. *)      
            add_write facts state (reference_of_local_name name) value
    | CGetField { base; offset; value } ->
            add_read facts state (reference_of_field base offset) value
    | CPutField { base; offset; value } ->
            add_write facts state (reference_of_field base offset) value
    | CRead { name; value; isGlobal } ->
            add_read facts state (reference_of_variable globals_are_properties facts isGlobal name) value
    | CWrite { name; value; isGlobal } ->
            add_write facts state (reference_of_variable globals_are_properties facts isGlobal name) value
    | CFunEnter { args; this } ->
            add_known_new_object objects facts state args |>
            fun state -> add_write facts state (reference_of_local_name "this") this
    | _ -> state

let globals_points_to (objects: Trace.objects) globals_are_properties (globals: Trace.globals) trace pt =
  let reference_of_global = reference_of_name globals_are_properties StringMap.empty true in
  match trace with
  | [] -> failwith "Empty trace"
  | (_, { versions }) :: _ ->
    ReferenceMap.fold (fun ref ver pt ->
      let vref = (ref, ver)
      and value = match Reference.get_fieldref ref with
        | Some (obj, field) ->
          begin try (StringMap.find field objects.(obj)).value
          with Not_found -> failwith ("Can't find field " ^ field ^ " of " ^ string_of_int obj) end 
        | None ->
          assert (Reference.is_global ref);
          let name = (Reference.get_name ref |> Misc.Option.some) in
          try StringMap.find name globals
          with Not_found -> failwith ("Can't  find global variable "^ name) in         
      VersionReferenceMap.add vref value pt)
      versions pt

let collect_pointsto (globals_are_properties: bool) (globals: Trace.globals) (objects: Trace.objects) (trace: LocalFacts.facts_trace) =
    begin
          VersionReferenceMap.empty |>
          VersionReferenceMap.add (Reference.reference_of_local_name "this", 0) (OObject 0) |>
          globals_points_to objects globals_are_properties globals trace |>
          trace_fold (collect_pointsto_step globals_are_properties objects)
    end trace
let calculate_pointsto (funs, objs, trace, globs, gap) = collect_pointsto gap globs objs trace
