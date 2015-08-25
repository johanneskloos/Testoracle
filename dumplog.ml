open MatchTracesObserver
open Format
open MatchTypes
open Richtrace
open MatchObjects
open MatchOperations
open Trace

let pp_print_stack pp =
  List.iter (function
    | Regular -> pp_print_char pp 'R'
    | Wrapper -> pp_print_char pp 'W'
    | External -> pp_print_char pp 'E'
    | ToString -> pp_print_char pp 'T'
    | Init -> pp_print_char pp 'I'
    | RegularEnter -> pp_print_char pp 'r')

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
  | MatchEnter-> "matching function entries"
	| UseStrictRHS -> "\"use strict\" on RHS")

let pp_path pp = function
  | [] ->
    pp_print_string pp "(top)"
  | p::l ->
    pp_print_string pp p;
    List.iter (fun p -> pp_print_char pp '.'; pp_print_string pp p) l
     
let pp_obj_match_failure pp = function
    NonMatching (path, val1, val2) -> fprintf pp "at %a, %a differs from %a"
    pp_path path pp_jsval val1 pp_jsval val2
  | MissingOrig (fld, path) -> fprintf pp "%s at %a missing in orig" fld pp_path path
  | MissingXfrm (fld, path) -> fprintf pp "%s at %a missing in xfrm" fld pp_path path
  | Other reason -> pp_print_string pp reason

let pp_fun_match_failure pp = function
  | DifferentBodies (body1, body2) ->
    fprintf pp "@[<v 4>Function body mismatch:@ %s@ @ %s@ @]" body1 body2
  | DifferentInstrumentedBodies (body1, body2) ->
    fprintf pp "@[<v 4>Instrumented function body mismatch:@ %s@ @ %s@ @]" body1 body2
  | DifferentExternal (id1, id2) ->
    fprintf pp "External function mismatch: %d vs. %d" id1 id2
  | InconsistentlyInstrumented -> pp_print_string pp "Instrumented vs. uninstrumented"
  | InternalExternal -> pp_print_string pp "Internal vs. external"
  
let pp_reason pp = let str = pp_print_string pp in function
  | DifferentType -> str "source types differ"
  | DifferentObjects (where, fail) -> fprintf pp "%s doesn't match: %a"
    where pp_obj_match_failure fail
  | DifferentArguments -> str "arguments differ"
  | DifferentValues where -> fprintf pp "values differe at %s" where
  | DifferentOperations -> str "different types of events"
  | OtherOperation -> str "not the right type of operation"
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
  
let pp_failed pp (failed_cons, op) =
  fprintf pp "@[<v 2>%a failed because the following conditions don't hold:@ %a@]@ "
    pp_match_operation op
    (FormatHelper.pp_print_list_lines
      (fun pp (cond, reason) -> fprintf pp "%a because %a" pp_cond cond pp_reason reason))
      failed_cons
      
let _ =
  open_vbox 0;
  Sys.argv.(1)
  |> MatchTracesObserver.read
  |> List.iter (function
    | RNode (id, op1, op2, stack) ->
      printf "node: %d where %a vs. %a with stack %a@ "
        id pp_rich_operation op1 pp_rich_operation op2
        pp_print_stack stack
    | REdge (src, dst, op) ->
      printf "edge: %d -> %d using %a@ "
       src dst pp_match_operation op
    | RFailure (id, failed) ->
      printf "@[<v 4>failed matches on %d:@ %a@]@ "
        id (FormatHelper.pp_print_list_lines pp_failed) failed
    | RXfrmConsumed (id, trace) ->
      printf "@[<v 4>node: %d, xfrm consumed before orig, left-over:@ %a@]@ "
        id (FormatHelper.pp_print_list_lines pp_rich_operation) trace
    | ROrigConsumedOk (id, trace, stack) ->
      printf "@[<v 4>node: %d, orig consumed, ok, stack: %a@ left-over:@ %a@]@ "
        id pp_print_stack stack (FormatHelper.pp_print_list_lines pp_rich_operation) trace
    | ROrigConsumedFailure (id, trace, stack) ->
      printf "@[<v 4>node: %d, orig consumed, failed, stack: %a@ left-over:@ %a@]@ "
        id pp_print_stack stack (FormatHelper.pp_print_list_lines pp_rich_operation) trace
    | RBlockedShared (id, len1, len2, stack) ->
      printf "node %d: blocked for length pair (%d, %d) and stack %a@ " id len1 len2 pp_print_stack stack);
  close_box ()
