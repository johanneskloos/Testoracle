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
  | AliasMatchPattern

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
  | IsAliasMatch
  | MatchAliasWrites

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
  | InAliasMatchPattern

(**
 * The entries of the matching certificate.
 *
 * Pair indicates paired operations, describing the subword
 * relationship. All other operations get classified as either wrapper
 * or initialisation.
*)
type event_match =
    Pair of TraceTypes.rich_operation * TraceTypes.rich_operation
  | Wrap of TraceTypes.rich_operation
  | Init of TraceTypes.rich_operation

(** Match failure explanation *)
type obj_match_failure =
    NonMatching of string list * TypesJS.jsval * TypesJS.jsval
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
  | And of mismatch * mismatch

type failure_trace = obj_match_failure option
type named_failure_trace = (string * obj_match_failure) option
type objeq = failure_trace IntIntMap.t

type matching_state = {
  rt1: TraceTypes.rich_tracefile;
  rt2: TraceTypes.rich_tracefile;
  objeq: objeq ref;
  initialisation_data: Reference.VersionedReferenceSet.t;
  toString_data: TypesJS.jsval list;
  mutable nonequivalent_functions: IntIntSet.t;
  mutable known_blocked: match_mode list list IntIntMap.t
}

(** Pretty-printers *)
open Format
let pp_match_mode pp = function
  | Regular -> Format.pp_print_string pp "regular"
  | RegularEnter -> Format.pp_print_string pp "regular-enter"
  | Wrapper -> Format.pp_print_string pp "wrap"
  | External -> Format.pp_print_string pp "external"
  | ToString -> Format.pp_print_string pp "toString"
  | Init -> Format.pp_print_string pp "init"
  | WrapperEnter -> Format.pp_print_string pp "wrapper-enter"
  | IndirectDefinitionPattern -> Format.pp_print_string pp "indirect-def"
  | ExtraFunctionPattern -> Format.pp_print_string pp "extra-func"
  | ToStringUpdatePattern -> Format.pp_print_string pp "tostring-update"
  | AliasMatchPattern -> Format.pp_print_string pp "alias-match"

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

let pp_print_stack pp =
  List.iter (function
      | Regular -> pp_print_char pp 'R'
      | Wrapper -> pp_print_char pp 'W'
      | External -> pp_print_char pp 'E'
      | ToString -> pp_print_char pp 'T'
      | Init -> pp_print_char pp 'I'
      | RegularEnter -> pp_print_char pp 'r'
      | WrapperEnter -> pp_print_char pp 'r'
      | IndirectDefinitionPattern -> pp_print_char pp 'i'
      | ExtraFunctionPattern -> pp_print_char pp 'e'
      | ToStringUpdatePattern -> pp_print_char pp 'u'
      | AliasMatchPattern -> pp_print_char pp 'A'
  )

let pp_cond pp cond = pp_print_string pp (match cond with
      MatchSides -> "match operations"
    | MayMatchSimple -> "may be used in a simple match"
    | MatchCallInt -> "matching internal calls"
    | MatchCallExt -> "matching external calls"
    | MatchCallToString -> "internal/external toString call pair"
    | MatchCallWrap -> "possible call to a wrapper"
    | MayInit -> "may appear in init"
    | IsToplevel -> "is a legal top-level event"
    | IsNotFunction -> "is not an event about function entry and exit"
    | IsExit -> "is a function exit"
    | IsPostExit -> "is a function post-exit"
    | IsCallInt -> "is a call to an internal function"
    | IsUnobservable -> "is an unobservable event"
    | MayInsertInWrapSimple -> "may be inserted in simple wrapper code"
    | IsEnter -> "is a function entry"
    | MatchEnter -> "matching function entries"
    | UseStrictRHS -> "\"use strict\" on RHS"
    | IsCatch -> "is a catch"
    | IsFunLiteral -> "is a function literal"
    | IsLocalDecl -> "is a local variable declaration"
    | IsFunRead -> "is a function read"
    | IsEndOfExpr -> "is end-of-expression"
    | IsAliasMatch -> "is an alias match"
    | MatchAliasWrites -> "are matching writes for aliases")


let pp_path pp = function
  | [] ->
    pp_print_string pp "(top)"
  | p:: l ->
    pp_print_string pp p;
    List.iter (fun p -> pp_print_char pp '.'; pp_print_string pp p) l

let pp_obj_match_failure pp = function
    NonMatching (path, val1, val2) -> fprintf pp "at %a, %a differs from %a"
                                        pp_path path TypesJS.pp_jsval val1 TypesJS.pp_jsval val2
  | MissingOrig (fld, path) -> fprintf pp "%s at %a missing in orig" fld pp_path path
  | MissingXfrm (fld, path) -> fprintf pp "%s at %a missing in xfrm" fld pp_path path
  | Other reason -> pp_print_string pp reason

let pp_fun_match_failure pp = function
  | DifferentBodies (body1, body2) ->
    fprintf pp "@[<v 4>Function body mismatch:@ %s@ @ %s@ @]" body1 body2
  | DifferentInstrumentedBodies (body1, body2) ->
    fprintf pp "@[<v 4>from_toString function body mismatch:@ %s@ @ %s@ @]" body1 body2
  | DifferentExternal (id1, id2) ->
    fprintf pp "External function mismatch: %d vs. %d" id1 id2
  | InconsistentlyInstrumented -> pp_print_string pp "from_toString vs. from_jalangi"
  | InternalExternal -> pp_print_string pp "Internal vs. external"

let rec pp_reason pp = let str = pp_print_string pp in function
    | DifferentType -> str "source types differ"
    | DifferentObjects (where, fail) -> fprintf pp "%s doesn't match: %a"
                                          where pp_obj_match_failure fail
    | DifferentArguments -> str "arguments differ"
    | DifferentValues where -> fprintf pp "values differe at %s" where
    | DifferentOperations -> str "different types of events"
    | OtherOperation -> str "not the right type of event"
    | NotToString -> str "not a toString call"
    | NotInitData -> str "not init data"
    | NotFunctionUpdate -> str "not a function update"
    | NotInitCode -> str "not an event-admissible event"
    | NotSimpleMatchable -> str "not allowed in simple matches"
    | NotWrapCode -> str "not allowed in wrapper code"
    | NotToStringCode -> str "not allowed in toString code"
    | ExternalCall -> str "call is external"
    | InternalCall -> str "call is internal"
    | NotLiterallyEqual -> str "function bodies not literally equal"
    | LiterallyEqual -> str "function bodies are literally equal"
    | NotToplevel -> str "not allowed in top-level code"
    | NotFunction -> str "not function-related"
    | NotExit -> str "not a function exit"
    | Observable -> str "event is observable"
    | NotAtToplevel -> str "not at toplevel"
    | NotEnter -> str "not a function entry"
    | FunctionMismatch reason -> pp_fun_match_failure pp reason
    | NotUseStrict -> str "not \"use strict\""
    | NotCatch -> str "not catch"
    | And (fst, snd) -> Format.fprintf pp "%a and %a" pp_reason fst pp_reason snd

let pp_mismatch = pp_reason

let pp_failed pp (failed_cons, op) =
  fprintf pp "@[<v 2>%a failed because the following conditions don't hold:@ %a@]@ "
    pp_match_operation op
    (Fmt.vbox (Fmt.list (Fmt.pair ~sep:(Fmt.const Fmt.string " because ") pp_cond pp_reason)))
    failed_cons

let get_state = function
  | Wrapper :: _ -> InWrap
  | WrapperEnter :: _ -> InWrapperEnter
  | Regular :: _ -> InRegular
  | RegularEnter :: _ -> InRegularEnter
  | External :: _ -> InExternal
  | ToString :: _ -> InToString
  | Init :: _ -> InInit
  | IndirectDefinitionPattern :: _ -> InIndirectDefinitionPattern
  | ExtraFunctionPattern :: _ -> InExtraFunctionPattern
  | ToStringUpdatePattern :: _ -> InToStringUpdatePattern
  | AliasMatchPattern :: _ -> InAliasMatchPattern
  | [] -> InToplevel

let pp_event_match pp = let open TraceTypes in function
    | Pair(r1, r2) -> fprintf pp "Pair\t%a\t%a@." pp_rich_operation r1 pp_rich_operation r2
    | Init i -> fprintf pp "Init\t\t%a@." pp_rich_operation i
    | Wrap i -> fprintf pp "Wrap\t\t%a@." pp_rich_operation i
