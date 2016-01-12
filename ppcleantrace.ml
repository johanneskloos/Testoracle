Array.get Sys.argv 1
|> open_in
|> Trace.parse_tracefile
|> CleanTrace.clean_tracefile
|> Printexc.print (Format.printf "%a@." TraceTypes.pp_clean_tracefile)
