module VersionReferenceMap = Reference.VersionReferenceMap
module VersionReferenceMapFormat = FormatHelper.MapFormat(VersionReferenceMap)
type points_to_map = Types.jsval VersionReferenceMap.t

let pp_points_to_map =
    VersionReferenceMapFormat.pp_print_map "" "" ","
        (FormatHelper.pp_print_map_entry Reference.pp_versioned_reference Types.pp_jsval)

let find_object_facts id ver pt =
    let vrefs = Reference.ReferenceMap.fold (fun ref ver acc ->
                    match ref with
                    | Reference.Field (id', fld) when id = id' -> ((ref, ver), fld) :: acc
                    | _ -> acc) ver.LocalFacts.versions [] in
    List.fold_left (fun acc (vref, fld) ->
                if VersionReferenceMap.mem vref pt then
                    Misc.StringMap.add fld (VersionReferenceMap.find vref pt) acc
                else begin
                    Format.eprintf "vref not found in points-to: %a@." Reference.pp_versioned_reference vref;
                    raise Not_found
                end)
        Misc.StringMap.empty vrefs


