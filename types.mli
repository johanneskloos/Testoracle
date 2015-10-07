(** A JavaScript value. This represents the value, either directly or as a reference to the object and/or function tables.  *)
type jsval =
    | OUndefined
    | ONull
    | OBoolean of bool
    | ONumberInt of int
    | ONumberFloat of float
    | OString of string
    | OSymbol of string
    | OFunction of int * int (** [OFunction(id, fid)] stands for a function belonging to object [id], with function id [fid]. *)
    | OObject of int (** [OObject id] stands for the object with object id [id]. *)
    | OOther of string * int (** [OOther (ty, id) stands for a special object with type [ty] and object id [id]. *)

(** The description of a field in an object. This record reflects
 * the structure of the Property Descriptor described in ECMAScript 6. *)
type fieldspec = {
    value: jsval;
    writable: bool;
    get: jsval option;
    set: jsval option;
    enumerable: bool;
    configurable: bool
}
(** An object can be described by a description of all its fields.
 *
 * Note that complete knowledge of object fields is not always available;
 * this comes up in [MatchObjects]. *)
type objectspec = fieldspec Misc.StringMap.t
(** Description of all object initial states in the program. *)
type objects = objectspec array
(** Description of a local JavaScript function, i.e., a function that
* consists of JavaScript code and not a native call.
*
* The two fields contain the from_toString and the from_jalangi code
* of the function. In some cases, from_jalangi contains "(unknown)";
* this happens when the code is outside of the Jalangi-instrumented
* part of the program.
*
* FIXME: Obviously, it would more sense to have a single string
* describing the function body, containing the uninstrumented code
* only. The cases can be kept apart in parsing.
*)
type local_funcspec = { from_toString : string; from_jalangi : string option }
(** Description of a Javascript function. It can either be local, with
* a description as given above, or [External fid], with function
* identifier [fid]. *)
type funcspec = Local of local_funcspec | External of int
(** Description of all Javascript functions encountered in a trace. *)
type functions = funcspec array
(** The values of all (known) global variables. *)
type globals = jsval Misc.StringMap.t

val pp_jsval : Format.formatter -> jsval -> unit
val pp_fieldspec : Format.formatter -> fieldspec -> unit
val pp_objectspec : Format.formatter -> objectspec -> unit
val pp_objects : Format.formatter -> objects -> unit
val pp_local_funcspec : Format.formatter -> local_funcspec -> unit
val pp_funcspec : Format.formatter -> funcspec -> unit
val pp_functions : Format.formatter -> functions -> unit
val pp_globals : Format.formatter -> globals -> unit

(** Object identifiers *)
type objectid =
    | Object of int
    | Function of int * int
    | Other of string * int

(** A field reference is given by an object identifier and a field name. *)
type fieldref = objectid * string

(** Indicated that an operation cannot be performed on some value because it does not have object nature. *)
exception NotAnObject
(** Get the object identifier of a given value, or throw [NotAnObject]. *)
val get_object: jsval -> int

(** Objectid helpers *)
val get_object_id: objectid -> int
val objectid_of_jsval: jsval -> objectid
val objectid_to_jsval: objectid -> jsval

val pp_objectid: Format.formatter -> objectid -> unit
val pp_fieldref: Format.formatter -> fieldref -> unit
