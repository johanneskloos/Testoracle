open MatchTracesObserver
open Format
open MatchTypes
open Richtrace
open MatchObjects
open MatchOperations
open Trace

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
