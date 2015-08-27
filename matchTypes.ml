open Richtrace
(** The mode to switch to, on a push. *)
type match_mode = Wrapper | Regular | External | ToString | Init | RegularEnter | WrapperEnter

(** Matching rules are build from match operations and match conditions.
 * First come the matching operations, which described how trace elements
 * get matched, and how the state stack is modified. *)
type match_operation =
    MatchSimple
  | MatchPush of match_mode
  | MatchReplace of match_mode
  | MatchPop
	| MatchDroppable
  | Initialization
  | InitializationPush of match_mode
  | InitializationPop
  | WrapperSimple
  | WrapperPush of match_mode
  | WrapperPop
	| WrapperReplace of match_mode

(** Next come the match conditions. *)
type match_condition =
    MatchSides
  | MayMatchSimple
  | MatchCallInt
  | MatchCallExt
  | MatchCallToString
  | MatchCallWrap
  | MatchEnter
  | MayInit
  | IsToplevel
  | IsNotFunction
  | IsExit
  | IsPostExit
  | IsEnter
  | IsCallInt
  | IsUnobservable
  | MayInsertInWrapSimple
	| UseStrictRHS



(** Description of the current state of matching. *)
type match_state =
    InToplevel
  | InRegular
  | InRegularEnter
  | InWrap
  | InToString
  | InExternal
  | InInit
	| InWrapperEnter

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

(** Pretty-printers for matching state and match operations *)
let pp_match_mode pp = function
    | Regular -> Format.pp_print_string pp "regular"
    | RegularEnter -> Format.pp_print_string pp "regular-enter"
    | Wrapper -> Format.pp_print_string pp "wrap"
    | External -> Format.pp_print_string pp "external"
    | ToString -> Format.pp_print_string pp "toString"
    | Init -> Format.pp_print_string pp "init"
		| WrapperEnter -> Format.pp_print_string pp "wrapper-enter"

let pp_match_operation pp = function
    | Initialization -> Format.pp_print_string pp "init"
    | WrapperSimple -> Format.pp_print_string pp "wrap"
    | WrapperPop -> Format.pp_print_string pp "wrap, pop"
    | WrapperPush m -> Format.fprintf pp "wrap, push %a" pp_match_mode m
		| WrapperReplace m -> Format.fprintf pp "wrap, replace %a" pp_match_mode m
    | MatchSimple -> Format.pp_print_string pp "match"
    | MatchPop -> Format.pp_print_string pp "match, pop"
    | MatchPush m -> Format.fprintf pp "match, push %a" pp_match_mode m
    | MatchReplace m -> Format.fprintf pp "match, replace %a" pp_match_mode m
    | InitializationPush m -> Format.fprintf pp "init, push %a" pp_match_mode m
    | InitializationPop -> Format.pp_print_string pp "init, pop"
		| MatchDroppable -> Format.pp_print_string pp "drop RHS"
