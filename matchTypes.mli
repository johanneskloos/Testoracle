open Richtrace
(** The mode to switch to, on a push. *)
type match_mode = Wrapper | Regular | External | ToString | Init

(** Matching rules are build from match operations and match conditions.
 * First come the matching operations, which described how trace elements
 * get matched, and how the state stack is modified. *)
type match_operation =
    MatchSimple
  | MatchPush of match_mode
  | MatchPop
  | Initialization
  | InitializationPush of match_mode
  | InitializationPop
  | WrapperSimple
  | WrapperPush of match_mode
  | WrapperPop

(** Next come the match conditions. *)
type match_condition =
    MatchSides
  | MayMatchSimple
  | MatchCallInt
  | MatchCallExt
  | MatchCallToString
  | MatchCallWrap
  | MayInit
  | IsToplevel
  | IsNotFunction
  | IsExit
  | IsCallInt
  | IsUnobservable
  | MayInsertInWrapSimple

(** Description of the current state of matching. *)
type match_state =
    InToplevel
  | InRegular
  | InWrap
  | InToString
  | InExternal
  | InInit

(**
* The entries of the matching certificate.
*
* Pair indicates paired operations, describing the subword
* relationship. All other operations get classified as either wrapper
* or initialisation.
*)
type event_match =
    Pair of rich_operation * rich_operation
  | Wrap of rich_operation
  | Init of rich_operation

(** Pretty-printers *)
val pp_match_mode: Format.formatter -> match_mode -> unit
val pp_match_operation: Format.formatter -> match_operation -> unit
