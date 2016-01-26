open Arg

let default_orig_trace = ".orig.trace"
let default_xfrm_trace = ".xfrm.trace"

let parse_args () =
  let names = ref [] in
  let args = [
    ("-D", Set Oracle.debug, "Debugging mode");
    ("-t", String (MatchTracesObserver.open_observer), "Trace file")
  ]
  and usage_msg =
    "Test oracle for Javascript trace equivalence. Usage:\n\
     oracle [options] path_orig path_xfrm\n\
     oracle [options] path_orig\n\
     where path_orig is the path to the trace file for unmodified code\n\
     and path_xfrm is the path to the trace file for modified code" in
  parse args (fun s -> names := s :: !names) usage_msg;
  begin
    match !names with
      | [path_xfrm; path_orig] -> (path_orig, path_xfrm)
      | [path_orig] when Filename.check_suffix path_orig default_orig_trace ->
          (path_orig, (Filename.chop_suffix path_orig default_orig_trace) ^ default_xfrm_trace)
      | _ -> usage args usage_msg; exit 2
  end

let _ =
  let (path_orig, path_xfrm) = parse_args() in
  let res = Oracle.check_matching path_orig path_xfrm
      (fun _ -> print_endline "OK"; 0)
      (fun _ -> print_endline "FAIL"; 1)
  in MatchTracesObserver.close_observer(); exit res

