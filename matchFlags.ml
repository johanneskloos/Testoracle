let lax_args = ref false
let lax_this = ref false

let flag_names = [
  ("lax-args", lax_args);
  ("lax-this", lax_this)
]

let flag_delimiter = Str.regexp_string " *, *"
let parse_match_flags str =
  List.iter (fun name ->
               try List.assoc name flag_names := true
               with Not_found ->
                 Format.eprintf "Can't parse matching flag %s!@." name)
    (Str.split flag_delimiter str);
  Format.eprintf "lax_args: %b@." !lax_args;
