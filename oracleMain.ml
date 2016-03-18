open Arg

let default_orig_trace = ".orig.trace"
let default_xfrm_trace = ".xfrm.trace"
let logging_output = ref false

let parse_args () =
  let names = ref [] in
  let args = [
    ("-D", Set Oracle.debug, "Debugging mode");
    ("-t", String (MatchTracesObserver.open_observer), "Trace file");
    ("-f", String (MatchFlags.parse_match_flags), "Matching flags");
    ("-l", Set logging_output, "Output logging line");
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

let log orig xfrm msg  =
    if !logging_output then
        print_endline (orig ^ "," ^ xfrm ^ "," ^ msg)
    else
        print_endline msg

let _ =
  let (path_orig, path_xfrm) = parse_args() in
  let res = Oracle.check_matching path_orig path_xfrm
      (fun _ -> log path_orig path_xfrm "OK"; 0)
      (fun _ -> log path_orig path_xfrm "FAIL"; 1)
  in MatchTracesObserver.close_observer(); exit res

