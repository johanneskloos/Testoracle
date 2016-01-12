Array.get Sys.argv 1
|> open_in
|> Trace.parse_tracefile
|> Format.printf "%a@." TraceTypes.pp_tracefile
