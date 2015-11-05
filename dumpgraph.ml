open CertifierData
open Richtrace

let dump_cert path =
	let open Format in
	let { tree; nodes } = MatchTracesObserver.read path |> CertifierData.extract_data in
	printf "@[<v>";
	TraceTree.iter_vertex (fun v -> match TraceNodes.find v with
	| FinalNodeData { op1; op2; stack; trace_trace } ->
		printf "@[<hov 2>Node %d: final! %a vs. %a, stack %a, failed: @[<v>@ %a@]@]@ "
			v pp_rich_operation op1 pp_rich_operation op2
			MatchTypes.pp_print_stack stack
			(FormatHelper.pp_print_list_lines MatchTypes.pp_failed) trace_trace
	| NodeData { op1; op2; stack; trace_trace } ->
		printf "@[<hov 2>Node %d: %a vs. %a, stack %a, failed: @[<v>@ %a@]@]@ "
			v pp_rich_operation op1 pp_rich_operation op2
			MatchTypes.pp_print_stack stack
			(FormatHelper.pp_print_list_lines MatchTypes.pp_failed) trace_trace
	| EndTrace ops ->
		printf "@[<v 2>Node %d: remaining trace data:@ %a@]@ "
			v (FormatHelper.pp_print_list_lines pp_rich_operation) ops
	| InitTailtraceData (ops, stack) ->
		printf "@[<v 2>Node %d: init tail not acceptable, stack: %a, trace:@ %a@]@ "
			v MatchTypes.pp_print_stack stack
			(FormatHelper.pp_print_list_lines pp_rich_operation) ops
	| SuccessNode -> printf "Node %d: Success!@ " v
	| BlockedData (len1, len2, stack) ->
		printf "Node %d: blocked by memoization (%d, %d, %a)"
		v len1 len2 MatchTypes.pp_print_stack stack)
		tree
	(* Also print edges? *)

dump_cert Sys.argv.(1) 