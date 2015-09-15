open Trace
open Richtrace
open MatchTraces
open Arg

let debug = ref false
let default_orig_trace = ".orig.trace"
let default_xfrm_trace = ".xfrm.trace"
let cert = ref false

let (>>) x f = f x; x

let debug_print msg pp data =
  if !debug then Format.printf "@[<v 2>%s:@ %a@]@."
    msg pp data
    
let rich_tracefile_from_path path =
  let chan = open_in path in
  let rt = parse_tracefile chan
    >> debug_print ("Read trace file " ^ path) pp_tracefile
    |> calculate_rich_tracefile
    >> debug_print "Enrichted trace file" pp_rich_tracefile in
  close_in chan; rt

let parse_args () =
  let names = ref [] in
  let args = [
    ("-D", Set debug, "Debugging mode");
    ("-t", String (MatchTracesObserver.open_observer), "Trace file");
    ("-c", Set cert, "Output certificate");
    ]
   and usage_msg =
     "Test oracle for Javascript trace equivalence. Usage:\n\
    oracle path_orig path_xfrm\n\
    oracle path_orig\n\
    where path_orig is the path to the trace file for unmodified code\n\
    and path_xfrm is the path to the trace file for modified code" in
  parse args (fun s -> names := s :: !names) usage_msg;
  match !names with
    | [path_xfrm; path_orig] -> (path_orig, path_xfrm)
    | [path_orig] when Filename.check_suffix path_orig default_orig_trace ->
      (path_orig, (Filename.chop_suffix path_orig default_orig_trace) ^ default_xfrm_trace)
    | _ -> usage args usage_msg; exit 2
 
let calculate_matching path_orig path_xfrm =
  let rt1 = rich_tracefile_from_path path_orig
  and rt2 = rich_tracefile_from_path path_xfrm in
  match_traces rt1 rt2


let main () =
  let (path_orig, path_xfrm) = parse_args() in
  match calculate_matching path_orig path_xfrm with
    | Some c ->
       print_endline "OK"; MatchTracesObserver.close_observer ();
       begin if !cert then Format.printf "%a" (fun pp -> List.iter (MatchTypes.pp_event_match pp)) c end; 
       exit 0
    | None -> print_endline "FAIL"; MatchTracesObserver.close_observer (); exit 1;;

main ()