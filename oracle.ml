let debug = ref false

let (>>>) x f = f x; x

let debug_print msg pp data =
  if !debug then Format.printf "@[<v 2>%s:@ %a@]@."
      msg pp data

let rich_tracefile_from_path path =
  let chan = open_in path in
  let rt = Trace.parse_tracefile chan
    >>> debug_print ("Read trace file " ^ path) TraceTypes.pp_tracefile
           |> RichTrace.tracefile_to_rich_tracefile
    >>> debug_print "Enrichted trace file" TraceTypes.pp_rich_tracefile in
  close_in chan; rt

let calculate_matching path_orig path_xfrm =
  let rt1 = rich_tracefile_from_path path_orig
  and rt2 = rich_tracefile_from_path path_xfrm in
  MatchTraces.match_traces rt1 rt2

let check_matching path_orig path_xfrm ok_handler fail_handler =
  match calculate_matching path_orig path_xfrm with
    | Some c -> ok_handler c
    | None -> fail_handler ()

