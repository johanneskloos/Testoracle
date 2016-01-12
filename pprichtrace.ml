Sys.argv.(1)
|> open_in
|> Trace.parse_tracefile
|> RichTrace.tracefile_to_rich_tracefile
|> Printexc.print (Format.printf "%a@." TraceTypes.pp_rich_tracefile)
