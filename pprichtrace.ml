open Trace;;
open Richtrace;;
open Printexc;;

record_backtrace true;;
let file = ref "";;
Arg.parse [("-f", Arg.Set dump_facts, "dump full information")] (fun name -> file := name) "pprichtrace [-f] file";;

!file
|> open_in
|> parse_tracefile
|> calculate_rich_tracefile
|> print (Format.printf "%a@." pp_rich_tracefile)
