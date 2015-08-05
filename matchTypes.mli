open Richtrace
(** The mode to switch to, on a push. *)
type mode = Wrapper | Regular | External | ToString | Init

(** Matching rules are build from match operations and match conditions.
 * First come the matching operations, which described how trace elements
 * get matched, and how the state stack is modified. *)
type match_operation =
    MatchSimple
  | MatchPush of mode
  | MatchPop
  | Initialization
  | InitializationPush of mode
  | InitializationPop
  | WrapperSimple
  | WrapperPush of mode
  | WrapperPop

(** Next come the match conditions. *)
type condition =
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
type state =
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
type match_type =
    Pair of rich_operation * rich_operation
  | Wrap of rich_operation
  | Init of rich_operation
