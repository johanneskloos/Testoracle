open Richtrace
open MatchTypes

let observer: out_channel option ref = ref None
let close_observer () =
  match !observer with
    | Some chan -> close_out chan; observer := None
    | None -> ()

let open_observer name =
  close_observer();
  observer := Some (open_out_bin name)

let node_id = ref 0
let with_chan f =
  match !observer with Some chan -> f chan | None -> ()
let next_node () = let res = !node_id in incr node_id; res

type record =
  | RNode of int * rich_operation * rich_operation * mode list
  | REdge of int * int * match_operation
  | RFailure of int * ((condition * MatchOperations.mismatch) list * match_operation) list
  | RXfrmConsumed of int * rich_operation list
  | ROrigConsumedOk of int * rich_operation list * mode list
  | ROrigConsumedFailure of int * rich_operation list * mode list

let write_record (data: record) chan =
  let sdata = Marshal.to_bytes data [] in
  output_binary_int chan (Bytes.length sdata);
  output_bytes chan sdata

let log_node (op1: rich_operation) (op2: rich_operation) (stack: mode list) =
  let res = next_node () in
  with_chan (write_record (RNode (res, op1, op2, stack)));
  res
let log_edge (parent: int) (op: match_operation) =
  with_chan (write_record (REdge (parent, !node_id, op)))
let log_failure (id: int) (info: ((condition * MatchOperations.mismatch) list * match_operation) list) =
  with_chan (write_record (RFailure (id, info)))
let log_xfrm_consumed tr =
  let node = next_node () in
  with_chan (write_record (RXfrmConsumed (node, tr)))
let log_orig_consumed_ok trace stack =
  let node = next_node () in
  with_chan (write_record (ROrigConsumedOk (node, trace, stack)))
let log_orig_consumed_failed trace stack =
  let node = next_node () in
  with_chan (write_record (ROrigConsumedFailure (node, trace, stack)))


let read filename =
  let chan = open_in_bin filename in
  let rec read_entries seen =
    try
      let len = input_binary_int chan in
      let mdata = Bytes.create len in
      really_input chan mdata 0 len;
      read_entries ((Marshal.from_bytes mdata 0: record) :: seen)
    with End_of_file -> List.rev seen
  in let entries = read_entries [] in close_in chan; entries    