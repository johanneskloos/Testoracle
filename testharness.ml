open Format;;
open Arg;;

let test_read_tracefile name =
    let file = ref None
    and cols = ref 80
    and usage = name ^ " [options] [tracefile] where - stands for stdin"
    in let args = [
        ("-c", Set_int(cols), "Output columns")
    ] in
    Arg.parse args (fun arg -> file := Some arg) usage;
    pp_set_margin std_formatter !cols;
    pp_set_margin err_formatter !cols;
    begin match !file with
    | None -> stdin
    | Some "-" -> stdin
    | Some name -> open_in name
    end |> Trace.parse_tracefile
