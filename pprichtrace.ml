Printexc.record_backtrace true;;
let file = ref "";;
Arg.parse [("-f", Arg.Set Richtrace.dump_facts, "dump full information")] (fun name -> file := name) "pprichtrace [-f] file";;

!file
|> open_in
|> Trace.parse_tracefile
|> Richtrace.calculate_rich_tracefile
|> Printexc.print (Format.printf "%a@." Richtrace.pp_rich_tracefile)
