open Arg

exception BadName of string * string
let colon = Str.regexp ":"
let dot = Str.regexp "\\."

let get_base filename =
  let dir = Filename.dirname filename
  and base = Filename.basename filename
  in match List.rev (Str.split dot base) with
    | "trace" :: typename :: ((_ :: _) as base) ->
        ((dir, List.fold_left (fun base comp -> comp ^ "." ^ base) "" base), typename)
    | _ :: _ :: _ :: _ ->
        raise (BadName("Name does not end in \".*.trace\"", filename))
    | [ _; _ ] | [_] ->
        raise (BadName("Name does not consist of at least three components", filename))
    | [] ->
        raise (BadName("File name is empty", filename))
          
let build_name (dir, base) suffix = Filename.concat dir (base ^ suffix)

let parse_job line =
  let build_orig base = build_name base "orig.trace"
  and build_cert base typename =
    Some (build_name base (typename ^ ".cert"))
  in match Str.split colon line with
    | [ xfrm; ""; "" ] ->
        let (base, typename) = get_base xfrm in
          (xfrm, build_orig base, build_cert base typename)
    | [ xfrm; ""; certname ] ->
        let (base, typename) = get_base xfrm in
          (xfrm, build_orig base, Some certname)
    | [ xfrm; orig; "-" ] ->
        (xfrm, orig, None)
    | [ xfrm; orig; "" ] ->
        let (base, typename) = get_base xfrm in
          (xfrm, orig, build_cert base typename)
    | [ xfrm; orig; cert ] -> (xfrm, orig, Some cert)
    | [ xfrm; "" ] | [ xfrm ] ->
        let (base, typename) = get_base xfrm in
          (xfrm, build_orig base, None)
    | [ xfrm; orig ] -> (xfrm, orig, None)
    | _ -> raise (Invalid_argument ("Unparsable line: " ^ line))

let run_job xfrm orig () =
  try
    Oracle.check_matching orig xfrm
      (fun _ -> Format.printf "%s:%s:OK@." orig xfrm)
      (fun _ -> Format.printf "%s:%s:FAIL@." orig xfrm)
  with
    | e -> Format.printf "%s:%s:ERROR %s@." orig xfrm (Printexc.to_string e)

let run_job_limited xfrm orig =
  LimitedTask.limit ~time:300L (run_job xfrm orig)
    (fun exit -> Format.eprintf "Matching %s and %s exited with %d!@."
                   xfrm orig exit)
    (fun signal -> Format.printf "%s:%s:SIGNAL %d@." orig xfrm signal)

let handle_job = function
  | (xfrm, orig, Some cert) ->
      begin
        MatchTracesObserver.open_observer cert;
        try
          run_job_limited xfrm orig;
          MatchTracesObserver.close_observer ()
        with e -> (MatchTracesObserver.close_observer (); raise e)
      end
  | (xfrm, orig, None) -> run_job_limited xfrm orig

let job_callback line =
  try
    parse_job line
      |> handle_job
  with
    | BadName (reason, line) ->
        Format.eprintf "Bad name: %s in %s@." reason line
    | Invalid_argument arg ->
        Format.eprintf "Invalid argument: %s" arg

open Arg 
let port = ref 8888
let parse_args () =
  let args = [
    ("-f", String (MatchFlags.parse_match_flags), "Matching flags");
    ("-p", Int (fun p -> port := p), "Job server port")
  ]
  and usage_msg =
    "Test oracle for Javascript trace equivalence, job client version. Usage:\n\
     oracleJobClient [options]\n"
  in parse args (fun _ -> failwith "Unexpected argument") usage_msg

let _ = parse_args(); Jobloop.job_loop !port job_callback
