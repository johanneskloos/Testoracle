let nested_pattern =
  Pcre.regexp ~study: true ~flags:[`MULTILINE] "function.*function\\w*[({]"

let detect_nested name (funcs, _, _, _, _) =
  try
    for i = 0 to Array.length funcs - 1 do
      match funcs.(i) with
      | Types.Local { Types.from_jalangi = Some body } ->
        if Pcre.pmatch ~rex: nested_pattern body then begin
          print_endline name;
          raise Exit
        end
      | _ -> ()
    done
  with Exit -> ()

let check name =
  let chan = open_in name in
  chan |> Trace.parse_tracefile |> detect_nested name;
  close_in chan

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    check Sys.argv.(i)
  done