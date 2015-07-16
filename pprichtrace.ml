open Trace;;
open Richtrace;;
open Printexc;;

record_backtrace true;;

Array.get Sys.argv 1
    |> open_in
    |> parse_tracefile
    |> calculate_rich_tracefile
    |> print (Format.printf "%a@." pp_rich_tracefile)
