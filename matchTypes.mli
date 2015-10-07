open Types
open Richtrace
(** The mode to switch to, on a push. *)
type match_mode =
    | Wrapper
    | Regular
    | External
    | ToString
    | Init
    | RegularEnter
    | WrapperEnter
    | HigherOrder
    | IndirectDefinitionPattern
    | ExtraFunctionPattern
    | ToStringUpdatePattern

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
    | IsCatch
    | MatchHigherOrder
    | IsFunLiteral
    | IsLocalDecl
    | IsFunRead
    | IsEndOfExpr

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
    | InHigherOrder
    | InIndirectDefinitionPattern
    | InExtraFunctionPattern
    | InToStringUpdatePattern

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

(** Match failure explanation *)
type obj_match_failure =
        NonMatching of string list * jsval * jsval
    | MissingOrig of string * string list
    | MissingXfrm of string * string list
    | Other of string

type fun_match_failure =
    | DifferentBodies of string * string
    | DifferentInstrumentedBodies of string * string
    | InconsistentlyInstrumented
    | DifferentExternal of int * int
    | InternalExternal

type mismatch =
    | DifferentType
    | DifferentObjects of string * obj_match_failure
    | DifferentArguments
    | DifferentValues of string
    | DifferentOperations
    | OtherOperation
    | NotToString
    | NotInitData
    | NotFunctionUpdate
    | NotInitCode
    | NotSimpleMatchable
    | NotWrapCode
    | NotToStringCode
    | ExternalCall
    | InternalCall
    | NotLiterallyEqual
    | LiterallyEqual
    | NotToplevel
    | NotFunction
    | NotExit
    | Observable
    | NotAtToplevel
    | NotEnter
    | FunctionMismatch of fun_match_failure
    | NotUseStrict
    | NotCatch

type failure_trace = obj_match_failure option
type named_failure_trace = (string * obj_match_failure) option
type objeq = failure_trace Misc.IntIntMap.t

(** State information for matching *)
type matching_state = {
    rt1: rich_tracefile;
    rt2: rich_tracefile;
    facts1: LocalFacts.local_facts;
    facts2: LocalFacts.local_facts;
    objeq: objeq;
    initialisation_data: Reference.VersionReferenceSet.t;
    toString_data: jsval list;
    nonequivalent_functions: Misc.IntIntSet.t;
    known_blocked: match_mode list list Misc.IntIntMap.t
}

(** Pretty-printers *)
val pp_match_mode: Format.formatter -> match_mode -> unit
val pp_match_operation: Format.formatter -> match_operation -> unit
val pp_print_stack: Format.formatter -> match_mode list -> unit
val pp_failed: Format.formatter -> (match_condition * mismatch) list * match_operation -> unit
val pp_event_match: Format.formatter -> event_match -> unit

(** Get the operating match_state by examining the stack. *)
val get_state : match_mode list -> match_state

