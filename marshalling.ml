open Richtrace
open MatchTraces

let marshal name (x: rich_tracefile * rich_tracefile * result) =
    let chan = open_out_bin name in
    Marshal.to_channel chan x [];
    close_out chan

let unmarshal name: rich_tracefile * rich_tracefile * result =
    let chan = open_in_bin name in
    let res = (Marshal.from_channel chan: rich_tracefile * rich_tracefile * result) in
    close_in chan; res