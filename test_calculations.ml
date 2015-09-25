open Kaputt;;
open Abbreviations;;
open CalculateVersions;;
open CalculatePointsTo;;
open PointsTo;;
open LocalFacts;;
open Trace;;
open Csv;;
open Misc;;
open Reference;;
open Notations;;

let (|>) = Pervasives.(|>)

module Facts = struct
    type header = Ref of reference | Arg | Param
    type version = Unknown | Alias of fieldref | Version of int | Inconsistent
    type expectation = {
        args: int option list;
        params: int option list;
        versions: version ReferenceMap.t list
    }
    type expectation_row = {
        arg: int option;
        param: int option;
        version_row: version ReferenceMap.t
    }
    let parse_header str =
        if str = "Arg" then Arg
        else if str = "Param" then Param
        else Ref (parse_reference str)
    let parse_int_option: string -> int option = function
        | "-" -> None
        | str -> Some (int_of_string str)
    let regex_alias = Str.regexp_case_fold "^alias \\([0-9]*\\)@\\(.*\\)$"
    let parse_version str =
        if Str.string_match regex_alias str 0 then
            Alias (Str.matched_group 1 str |> int_of_string,
                Str.matched_group 2 str)
        else match parse_int_option str with
            | Some ver -> Version ver
            | None -> Unknown
    let parse_row header { args; params; versions } row: expectation =
        let { arg; param; version_row } = List.fold_left2
                (fun data cell -> function
                        | Ref ref -> { data with version_row =
                                    ReferenceMap.add ref (parse_version cell) data.version_row }
                        | Arg -> { data with arg = parse_int_option cell }
                        | Param -> { data with param = parse_int_option cell })
                { arg = None; param = None; version_row = ReferenceMap.empty }
                row header in
        { args = arg:: args; versions = version_row:: versions; params = param:: params }
    let parse_csv csv: expectation =
        csv |> function [] -> failwith "Bad input format" | raw_header :: data ->
            let header = List.map parse_header raw_header in
            data |>
            List.rev |>
            List.fold_left (parse_row header)
                { args = []; params = []; versions = [] }
    let load_csv path = path |> Csv.load |> parse_csv
end
open Facts

module PointsTo = struct
    let regex_int = Str.regexp_case_fold "^int:\\(-*[0-9]*\\)$"
    let regex_float = Str.regexp_case_fold "^float:\\(-*[0-9]*\\.[0-9]*\\)$"
    let regex_str = Str.regexp_case_fold "^string:\\(.*\\)$"
    let regex_symbol = Str.regexp_case_fold "^symbol:\\(.*\\)$"
    let regex_function = Str.regexp_case_fold "^function:\\([0-9]*\\)/\\([0-9]*\\)$"
    let regex_object = Str.regexp_case_fold "^object:\\([0-9]*\\)$"
    let regex_other = Str.regexp_case_fold "^other:\\(.*\\),\\([0-9]*\\)$"
    let parse_value str =
        if str = "null" then ONull
        else if str = "undefined" then OUndefined
        else if str = "true" then OBoolean true
        else if str = "false" then OBoolean false
        else if Str.string_match regex_int str 0 then
            ONumberInt (Str.matched_group 1 str |> int_of_string)
        else if Str.string_match regex_float str 0 then
            ONumberFloat (Str.matched_group 1 str |> float_of_string)
        else if Str.string_match regex_str str 0 then
            OString (Str.matched_group 1 str)
        else if Str.string_match regex_symbol str 0 then
            OSymbol (Str.matched_group 1 str)
        else if Str.string_match regex_function str 0 then
            OFunction (Str.matched_group 1 str |> int_of_string,
                Str.matched_group 2 str |> int_of_string)
        else if Str.string_match regex_object str 0 then
            OObject (Str.matched_group 1 str |> int_of_string)
        else if Str.string_match regex_other str 0 then
            OOther (Str.matched_group 1 str,
                Str.matched_group 2 str |> int_of_string)
        else failwith ("Can't parse object " ^ str)
    
    let parse_csv csv: points_to_map =
        List.fold_left (fun ptm -> function
                    | [obj; fld; value] ->
                        VersionReferenceMap.add (parse_reference obj, int_of_string fld)
                            (parse_value value) ptm
                    | _ -> failwith "Incorrect input format")
            VersionReferenceMap.empty csv
    let load_csv path = path |> Csv.load |> parse_csv
end;;
open PointsTo

type testdata = {
    trace: tracefile;
    local: Facts.expectation;
    pointsto: points_to_map
}

let get_testdata base =
    let make_name ext = Filename.concat "testdata" (base ^ "." ^ ext) in
    let trace =
        let inchan = Pervasives.open_in (make_name "trace") in
        let trace = parse_tracefile inchan in
        Pervasives.close_in inchan; trace in
    let (_, _, _, _, globals_are_properties) = trace in
    let (suff_facts, suff_pointsto) = if globals_are_properties then
            ("alias.csv", "ptalias.csv") else ("noalias.csv", "ptnoalias.csv") in
    { trace; local = Facts.load_csv (make_name suff_facts);
        pointsto = PointsTo.load_csv (make_name suff_pointsto) }

let string_list_to_string lst =
    List.rev lst |>
    Format.asprintf "@\n%a"
        (FormatHelper.pp_print_list_lines Format.pp_print_string)
let errors = ref ["Uninitialized errors array!"]
let add_error msg = errors := msg :: !errors

let check_list what check_cells got exp =
    let rec check i = function
        | ([], []) -> ()
        | ([], l) ->
            add_error (Format.sprintf "%s: expected %d, got %d entries"
                        what i (i + List.length l))
        | (l, []) ->
            add_error (Format.sprintf "%s: expected %d, got %d entries"
                        what (i + List.length l) i)
        | (x1:: l1, x2:: l2) ->
            check_cells i x1 x2;
            check (i +1) (l1, l2)
    in check 0 (got, exp)

let check_int_opt_list what =
    check_list what (fun i x1 x2 ->
                if x1 <> x2 then
                    let fmt =
                        FormatHelper.pp_print_option Format.pp_print_int in
                    Format.asprintf "%s, entry %d: expected %a, got %a"
                        what i fmt x1 fmt x2 |> add_error)

let check_arguments_equals got exp =
    check_int_opt_list "caller arguments"
        (List.map (fun x -> x.last_arguments) got)
        exp.args

let check_versions_row i (got: local_facts) exp =
    let got' =
        ReferenceMap.map (fun ver -> Version ver) got.versions |>
        StringMap.fold (fun name alias ver ->
                    let ref = reference_of_local_name name in
                    match try ReferenceMap.find ref ver with Not_found -> Unknown with
                    | Unknown -> ReferenceMap.add ref (Alias alias) ver
                    | _ -> ReferenceMap.add ref Inconsistent ver)
            got.aliases
    in ReferenceMap.merge (fun ref got exp ->
                let fmt pp = function
                    | Unknown -> Format.pp_print_string pp "unknown"
                    | Inconsistent -> Format.pp_print_string pp "inconsistent"
                    | (Alias alias) -> Format.fprintf pp "alias %a" pp_fieldref alias
                    | (Version ver) -> Format.fprintf pp "version %d" ver
                in
                let (got, exp) = (Option.get Unknown got, Option.get Unknown exp) in
                if got <> exp then begin
                    Format.asprintf "%a, row %d: got %a, expected %a"
                        pp_reference ref i fmt got fmt exp |> add_error
                end; None) got' exp |> ignore

let check_versions_equals got exp =
    check_list "versions" check_versions_row got exp.versions

let check_points_to_equals got exp =
    VersionReferenceMap.merge (fun vref got exp ->
                if got <> exp then begin
                    let fmt = FormatHelper.pp_print_option pp_jsval in
                    Format.asprintf "%a: got %a, expected %a"
                        pp_versioned_reference vref fmt got fmt exp |> add_error
                end; None) got exp |> ignore

let test_one checker { trace; local = explocs } =
    errors := [];
    let gottrace =
        trace |> calculate_arguments_and_parameters |> calculate_versions in
    let gotlocs = match gottrace with (_, _, trace, _, _) -> List.map snd trace in
    checker gotlocs explocs;
    Assert.make_equal (=) string_list_to_string [] !errors

let test_arguments = test_one check_arguments_equals
let test_versions = test_one check_versions_equals

let test_pointsto { trace; local = explocs; pointsto = exppointsto } =
    errors := [];
    let gottrace =
        trace |> calculate_arguments_and_parameters |> calculate_versions in
    let gotpointsto = calculate_pointsto gottrace in
    check_points_to_equals gotpointsto exppointsto;
    Assert.make_equal (=) string_list_to_string [] !errors

let make_one_test what tester path =
    Test.make_assert_test ~title: (path ^ " -- " ^ what)
        (fun () -> get_testdata path) tester ignore
let make_tests_for path =
    [make_one_test "caller arguments" test_arguments path;
    make_one_test "versions" test_versions path;
    make_one_test "points-to map" test_pointsto path]

let tests = List.map make_tests_for ["facts1"; "facts2"; "facts3"] |> List.flatten

let () = Test.run_tests tests

