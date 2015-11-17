(** The mode to switch to, on a push. *)
type match_mode =
    | Wrapper
    | Regular
    | External
    | ToString
    | Init
    | RegularEnter
    | WrapperEnter
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
        Pair of Richtrace.rich_operation * Richtrace.rich_operation
    | Wrap of Richtrace.rich_operation
    | Init of Richtrace.rich_operation

(** Match failure explanation *)
type obj_match_failure =
        NonMatching of string list * Types.jsval * Types.jsval
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

(** State information for matching.
 * There are three kinds of information:
 * - Static ([rt1] and [rt2]): This information is constant throughout matching.
 * - Monotonically changing ([nonequivalent_functions] and [known_blocked]):
 *    These fields only get information added, never removed. Therefore, making
 *    them mutable is reasonable - we never need to restore an old state.
 * - Nonmonotonically chaning (all others).
 *)
type matching_state = {
    rt1: Richtrace.rich_tracefile;
    rt2: Richtrace.rich_tracefile;
    objeq: objeq ref;
    initialisation_data: Reference.VersionReferenceSet.t;
    toString_data: Types.jsval list;
    mutable nonequivalent_functions: Misc.IntIntSet.t;
    mutable known_blocked: match_mode list list Misc.IntIntMap.t
}

(** Pretty-printers *)
val pp_match_mode: Format.formatter -> match_mode -> unit
val pp_match_operation: Format.formatter -> match_operation -> unit
val pp_print_stack: Format.formatter -> match_mode list -> unit
val pp_failed: Format.formatter -> (match_condition * mismatch) list * match_operation -> unit
val pp_event_match: Format.formatter -> event_match -> unit
val pp_fun_match_failure: Format.formatter -> fun_match_failure -> unit

(** Get the operating match_state by examining the stack. *)
val get_state : match_mode list -> match_state

