open Trace;;
let (|>) x f = f x in
  Array.get Sys.argv 1
    |> open_in
    |> parse_tracefile
    |> Format.printf "%a@." pp_tracefile
