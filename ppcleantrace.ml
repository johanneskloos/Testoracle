Printexc.record_backtrace true;;

Array.get Sys.argv 1
|> open_in
|> Trace.parse_tracefile
|> Cleantrace.clean_tracefile
|> Printexc.print (Format.printf "%a@." Cleantrace.pp_clean_tracefile)
