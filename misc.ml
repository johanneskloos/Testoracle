open Format

(* BISECT-IGNORE-BEGIN *)
(** Standard maps and sets. *)
module IntMap = Map.Make(struct type t = int let compare = compare end)
module StringMap = Map.Make(String)
module IntIntMap = Map.Make(struct type t = int * int let compare = compare end)
module IntIntSet = Set.Make(struct type t = int * int let compare = compare end)

(** Formatting helpers for standard maps and sets. *)
module IntMapFormat = FormatHelper.MapFormat(IntMap)
module StringMapFormat = FormatHelper.MapFormat(StringMap)
module IntIntMapFormat = FormatHelper.MapFormat(IntIntMap)
module IntIntSetFormat = FormatHelper.SetFormat(IntIntSet)
let pp_print_int_pair = FormatHelper.pp_print_pair pp_print_int pp_print_int

(** Helper functions for options. *)
module Option = struct
    (** Get the content of an option, or a default value if not content exists.
     *
     * Here and below, content means the [x] of [Some x].
     *)
    let get d = function Some x -> x | None -> d
    (** Get the content of an option, or throw an exception if no content exists. *)
    let some = function Some x -> x | None -> failwith "option unexpectedly empty"
    (** [map f x] applies [f] to the content of [x] *)
    let map f = function Some x -> Some (f x) | None -> None
    (** Monadic bind for options *)
    let bind f = function Some x -> f x | None -> None
    (** Turn an option into a list containing the option's content. *)
    let to_list = function Some x -> [x] | None -> []
end;;

(** Helper functions for maps. *)
module MapExtra(S: Map.S) = struct
    (** [get k m] returns [Some v] if [k] maps to [v] in [m], and [None] if
     * no [v] exists.
     *)
    let get k m = try Some (S.find k m) with Not_found -> None
    (** [update k f m] replaces the content [v] of entry [k] of [m] with [f v]. *)  
    let update k f m = S.add k (f (S.find k m)) m
    (** Simultaneously iterate over two maps.
     *
     * Suppose the call is [fold2 fleft fright fboth m1 m2 acc].
     * The iteration is performed by joining the maps on equal keys.
     * Depending on which map(s) contain a given key, one of
     * [fleft] (for [m1]), [fright] (for [m2]) or [fboth] (for [m1] and [m2]) is
     * called. *)
    let fold2 fleft fright fboth m1 m2 =
        S.merge (fun _ v1 v2 -> Some (v1, v2)) m1 m2 |>
        S.fold (fun key valpair acc -> match valpair with
                | (Some val1, Some val2) -> fboth key val1 val2 acc
                | (Some val1, None) -> fleft key val1 acc
                | (None, Some val2) -> fright key val2 acc
                | (None, None) -> assert false)
    (** Simulataneously iterate over two maps, using a monadic operation.
     *
     * This is essentially like [fold2], excpt that the visitor functions
     * may return an option. *)
    let fold2_option fleft fright fboth = fold2
        (fun key val1 -> Option.bind (fleft key val1))
        (fun key val2 -> Option.bind (fright key val2))
        (fun key val1 val2 -> Option.bind (fboth key val1 val2))

    (** Transform an association list to a map *)
    let of_list l =
       List.fold_left (fun map (key, value) -> S.add key value map) S.empty l
end;;
(** Mapping functions for tuples. *)
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
(** Replacement for [List.hd], return [Some x] for lists starting with [x]
 * and [None] for empty lists. *)
let hd_err = function x :: _ -> Some x | [] -> None
(** Notations. *)
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

(** Given a [Format]-style pretty printer, turn a value into a string. *)
let to_string fmt x =
    let str = Format.asprintf "%a" fmt x in
    ignore (flush_str_formatter ()); str
(* BISECT-IGNORE-END *)
