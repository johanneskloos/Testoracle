open Reference;;
open LocalFacts;;
open Misc;;
open Trace;;
open CalculateVersions;;

module VersionReferenceMapFormat = FormatHelper.MapFormat(VersionReferenceMap)
type points_to_map = objid VersionReferenceMap.t

let pp_points_to_map =
    VersionReferenceMapFormat.pp_print_map "" "" ","
    (FormatHelper.pp_print_map_entry pp_versioned_reference pp_objid)

let find_object_facts id ver pt =
    let vrefs = ReferenceMap.fold (fun ref ver acc ->
        match get_fieldref ref with
        | Some (id', fld) when id = id' -> ((ref, ver), fld) :: acc
        | _ -> acc) ver.versions [] in
    List.fold_left (fun acc (vref, fld) ->
        StringMap.add fld (VersionReferenceMap.find vref pt) acc)
        StringMap.empty vrefs


