open Trace;;
open Cleantrace;;
open Printexc;;

record_backtrace true;;

Array.get Sys.argv 1
    |> open_in
    |> parse_tracefile
    |> clean_tracefile
    |> print (Format.printf "%a@." pp_clean_tracefile)
