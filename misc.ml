open Format

(** Standard maps and sets. *)
module IntMap = Map.Make(struct type t = int let compare = compare end)
module StringMap = Map.Make(struct type t = string let compare = compare end)
module IntIntMap = Map.Make(struct type t = int * int let compare = compare end)
module IntIntSet = Set.Make(struct type t = int * int let compare = compare end)

(** Formatting helpers. *)
module IntMapFormat = FormatHelper.MapFormat(IntMap)
module StringMapFormat = FormatHelper.MapFormat(StringMap)
module IntIntMapFormat = FormatHelper.MapFormat(IntIntMap)
module IntIntSetFormat = FormatHelper.SetFormat(IntIntSet)
let pp_print_int_pair = FormatHelper.pp_print_pair pp_print_int pp_print_int

(** Containers. *)
module Option = struct
    let get d = function Some x -> x | None -> d
    let some = function Some x -> x | None -> failwith "option unexpectedly empty"
    let map f = function Some x -> Some (f x) | None -> None
    let bind f = function Some x -> f x | None -> None
    let to_list = function Some x -> [x] | None -> []
end;;
module MapExtra(S: Map.S) = struct
    let get k m = try Some (S.find k m) with Not_found -> None
    let update k f m = S.add k (f (S.find k m)) m
    let fold2 fleft fright fboth m1 m2 =
        S.merge (fun _ v1 v2 -> Some (v1, v2)) m1 m2 |>
        S.fold (fun key valpair acc -> match valpair with
                | (Some val1, Some val2) -> fboth key val1 val2 acc
                | (Some val1, None) -> fleft key val1 acc
                | (None, Some val2) -> fright key val2 acc
                | (None, None) -> assert false)
    let fold2_option fleft fright fboth = fold2
        (fun key val1 -> Option.bind (fleft key val1))
        (fun key val2 -> Option.bind (fright key val2))
        (fun key val1 val2 -> Option.bind (fboth key val1 val2))
end;;
let map12 f (x,y) = (f x, y)
let map22 f (x,y) = (x, f y)
let map13 f (x,y,z) = (f x, y, z)
let map23 f (x,y,z) = (x, f y, z)
let map33 f (x,y,z) = (x, y, f z)
let map14 f (x,y,z,u) = (f x, y, z, u)
let map24 f (x,y,z,u) = (x, f y, z, u)
let map34 f (x,y,z,u) = (x, y, f z, u)
let map44 f (x,y,z,u) = (x, y, z, f u)
let pmap f g (x, y) = (f x, g y)
let bmap f (x, y) = (f x, f y)
let hd_err = function x :: _ -> Some x | [] -> None
module Notations = struct
  let ($?) o d = Option.get d o
  let (>|?) o f = Option.map f o
  let (>=?) o f = Option.bind f o
  let (>|*) l f = List.map f l
  let (>=*) l f = List.map f l |> List.flatten
  let (<+>) f g = pmap f g
  let (<++>) t f = bmap f t
  let (<+->) t f = map12 f t
  let (<-+>) t f = map12 t f
  let (<+-->) t f = map13 f t
  let (<-+->) t f = map23 t f
  let (<--+>) t f = map33 t f
  let (<+--->) t f = map14 f t
  let (<-+-->) t f = map24 t f
  let (<--+->) t f = map34 t f
  let (<---+>) t f = map44 t f
  let (?*) = hd_err
  let (>>) x f = f x; x
end;;

let to_string fmt x =
    let str = Format.asprintf "%a" fmt x in
    ignore (flush_str_formatter ()); str
