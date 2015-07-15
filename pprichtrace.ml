open Trace;;
open Richtrace;;
let (|>) x f = f x in
  Array.get Sys.argv 1
    |> open_in
    |> parse_tracefile
    |> calculate_rich_tracefile
    |> Format.printf "%a@." pp_rich_tracefile
