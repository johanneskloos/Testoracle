let (>>) x f = f x; x
let debug fmt x = Format.eprintf "DEBUG: %a@." fmt x
let msg x = Format.eprintf "DEBUG: %s@." x