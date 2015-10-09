open Format

(* BISECT-IGNORE-BEGIN *)
(** * Helper functions. *)
(** These helper functions should probably go to stdlib. *)
(** [curry] and [uncurry] transform between functions
with regular arguments and tuple arguments. *)
let curry f (x, y) = f x y
let uncurry f x y = f (x, y)
(** Iterate over an array, giving the array index and the value.
 * This is a companion to mapi. *)
let iteri f a =
    for i = 0 to Array.length a - 1 do
        f i a.(i)
    done

(** * Formatters for standard library types. *)
(** Simple formatters for various straightforward types. *)
let pp_print_unit pp _ = pp_print_string pp "()"
let pp_print_complex pp x = let open Complex in fprintf pp "@[<h>%f,@ %f@]" x.re x.im
let pp_print_buffer pp buf = pp_print_string pp (Buffer.contents buf)

(** ** Formatters for exceptions. *)
(** [pp_print_exception] just prints the exception,
 * while [pp_print_exception_backtrace] also prints a backtrace
 * if available. *)
let pp_print_exception pp e = pp_print_string pp (Printexc.to_string e)
let pp_print_exception_backtrace pp e =
    if Printexc.backtrace_status () then
        fprintf pp "%s (no backtrace available)" (Printexc.to_string e)
    else
        fprintf pp "@[<v>%s@ Backtrace: %s@]"
            (Printexc.to_string e) (Printexc.get_backtrace ())

(** ** Higher-order formatters. *)
(** These functions format polymorphic data types.
 * For this, they take an argument [fmt] to format the
 * contained data. *)
let pp_print_option fmt pp = function
    | Some x -> fmt pp x
    | None -> pp_print_string pp "(none)"
let pp_print_ref fmt pp r = fmt pp !r
(** Pair formatter. It takes two extra argument that format the
* pair components. *)
let pp_print_pair fmt1 fmt2 pp (x, y) =
    fprintf pp "@[<hv>%a,@ %a@]" fmt1 x fmt2 y

(** ** Generic formatters for iterable data structures. *)
(** There are various "list-like" and "map-like" data
 * structures available, so we provide generic formatters
 * for such data structures. They are then instantiated
 * for the various types.
 *
 * [pp_print_iterable iter sstart send ssep fmt]
 * formats list - like data structures,
 * i.e., structures that can be seen as some specific
 * subtype of lists (lists, arrays, sets, queues and stacks).
 * The only requirement is that the type must provide a
 * function [iter: ('a -> unit) -> 'a t -> unit].
 * Values of this type are then printed in the following
 * layout: [sstart] x1 [ssep] ... [ssep] xn [send].
 * The contents are formatted using [fmt].
 *)
let pp_print_iterable (iter: ('a -> unit) -> 'b -> unit)
    sstart send ssep (fmt: formatter -> 'a -> unit) pp x =
    pp_open_hovbox pp (String.length sstart);
    pp_print_string pp sstart;
    let first = ref true in begin
        iter (fun x ->
                    if !first then first := false else fprintf pp "%s@ " ssep;
                    fmt pp x) x;
        pp_print_string pp (" " ^ send);
        pp_close_box pp ()
    end
(** An alternative way to format iterable structures is
 * a "one per line" approach.
 *
 * [pp_print_iterable_lines iter indent fmt]
 * formats list - like data structures
 * in the alternative line - by - line format. *)
let pp_print_iterable_lines (iter: ('a -> unit) -> 'b -> unit)
    indent (fmt: formatter -> 'a -> unit) pp x =
    pp_open_vbox pp indent;
    iter (fun x -> fmt pp x; pp_print_cut pp ()) x;
    pp_close_box pp ()

(** [pp_print_map_iterable iter sstart send ssep fmt]
 * formats map - like data structures. It is a
 * wrapper around [pp_print_iterable] to allow the
 * use of map iterators
 * [iter: ('a -> 'b -> unit) -> ('a, 'b) t -> unit]
 * and map entry formatters that take separate key
 * and value arguments. *)
let pp_print_map_iterable iter sstart send ssep fmtkv =
    pp_print_iterable (fun f -> iter (uncurry f))
        sstart send ssep (fun pp -> curry (fmtkv pp))
(** [pp_print_map_iterable_lines iter fmt]
 * formats map - like data structures in the
 * "one per line" format. *)
let pp_print_map_iterable_lines iter indent fmtkv =
    pp_print_iterable_lines (fun f -> iter (uncurry f))
        indent (fun pp -> curry (fmtkv pp))

(** ** Higher-order formatters for list-like data. *)
(** For each data type, we provide two functions:
 * [pp_print_gen_ < type >] allows control over the
 * punctuation used, while [pp_print_ < type >] uses
 * a standard layout. *)
(* XXX Why can't we just say [pp_print_iterable List.iter]? *)
let pp_print_gen_list sstart send ssep fmt =
    pp_print_iterable List.iter sstart send ssep fmt
let pp_print_list fmt = pp_print_gen_list "[" "]" ";" fmt
let pp_print_gen_list_lines indent fmt =
    pp_print_iterable_lines List.iter indent fmt
let pp_print_list_lines fmt = pp_print_gen_list_lines 0 fmt
let pp_print_queue sstart send ssep fmt =
    pp_print_iterable Queue.iter sstart send ssep fmt
let pp_print_gen_queue fmt = pp_print_queue "[" "]" ";" fmt
let pp_print_gen_queue_lines indent fmt =
    pp_print_iterable_lines Queue.iter indent fmt
let pp_print_queue_lines fmt = pp_print_gen_queue_lines 0 fmt
let pp_print_stack sstart send ssep fmt =
    pp_print_iterable Stack.iter sstart send ssep fmt
let pp_print_gen_stack fmt = pp_print_stack "[" "]" ";" fmt
let pp_print_gen_stack_lines indent fmt =
    pp_print_iterable_lines Stack.iter indent fmt
let pp_print_stack_lines fmt = pp_print_gen_stack_lines 0 fmt
let pp_print_gen_array sstart send ssep fmt =
    pp_print_iterable Array.iter sstart send ssep fmt
let pp_print_array fmt = pp_print_gen_array "[|" "|]" ";" fmt
let pp_print_gen_array_lines indent fmt =
    pp_print_iterable_lines Array.iter indent fmt
let pp_print_array_lines fmt = pp_print_gen_array_lines 0 fmt
(** Arrays can also be seen as a kind of map from
 * integers to values. The following four functions
 * are alternate formatters that also provide the index. *)
let pp_print_array_idx sstart send ssep fmt =
    pp_print_map_iterable iteri sstart send ssep fmt
let pp_print_gen_array_idx fmt = pp_print_array_idx "[|" "|]" ";" fmt
let pp_print_array_idx_gen_lines indent fmt =
    pp_print_map_iterable_lines iteri indent fmt
let pp_print_array_idx_lines fmt =
    pp_print_array_idx_gen_lines 0 fmt
(** [pp_print_array_default fmt pp a] formats an array as
 * "[|" 1: x1 ; .. ; n: xn "|]". *)
let pp_print_array_default fmt =
    pp_print_gen_array_idx
        (fun pp i x -> fprintf pp "@[<hov>%d:@ %a@]" i fmt x)
(** [pp_print_array_default_lines fmt pp a] formats an array
 * as
 * 1: x1
 * 2: x2
 * ...
 * n: xn
 *)
let pp_print_array_default_lines fmt =
    pp_print_array_idx_lines
        (fun pp i x -> fprintf pp "@[<hov 2>%d:@ %a@]" i fmt x)

(** ** Higher-order formatters for map-like data. *)
(** Again, we provide generic and standard formatters.
 * For actual maps, there is also the formatter
 * [pp_print_ < type > _default] that prints the entries
 * of the map in the form "key => value".
 *)
let pp_print_map_entry fmtk fmtv pp k v =
    fprintf pp "@[<hov>%a =>@ %a@]" fmtk k fmtv v
let pp_print_hashtbl sstart send ssep fmt =
    pp_print_map_iterable Hashtbl.iter sstart send ssep fmt
let pp_print_gen_hashtbl fmt = pp_print_hashtbl "{" "}" ";" fmt
let pp_print_hashtbl_default fmtk fmtv =
    pp_print_gen_hashtbl (pp_print_map_entry fmtk fmtv)
let pp_print_hashtbl_lines indent fmt =
    pp_print_map_iterable_lines Hashtbl.iter indent fmt
let pp_print_gen_hashtbl_lines fmt = pp_print_hashtbl_lines 0 fmt
let pp_print_hashtbl_lines_default fmtk fmtv =
    pp_print_gen_hashtbl_lines (pp_print_map_entry fmtk fmtv)
(** ** Formatter functors. *)
(** For functional data types, we provide functors that
 * generate the appropriate formatting functions. The
 * naming conventions are as above. *)
module MapFormat(M: Map.S) = struct
    let pp_print_map sstart send ssep fmt =
        pp_print_map_iterable M.iter sstart send ssep fmt
    let pp_print_gen_map fmt = pp_print_map "{" "}" ";" fmt
    let pp_print_map_default fmtk fmtv =
        pp_print_gen_map (pp_print_map_entry fmtk fmtv)
    let pp_print_map_lines indent fmt =
        pp_print_map_iterable_lines M.iter indent fmt
    let pp_print_gen_map_lines fmt = pp_print_map_lines 0 fmt
    let pp_print_map_default_lines fmtk fmtv =
        pp_print_gen_map_lines (pp_print_map_entry fmtk fmtv)
end;;
module SetFormat(S: Set.S) = struct
    let pp_print_set sstart send ssep fmt =
        pp_print_iterable S.iter sstart send ssep fmt
    let pp_print_gen_set fmt = pp_print_set "{" "}" ";" fmt
    let pp_print_set_lines indent fmt =
        pp_print_iterable_lines S.iter indent fmt
    let pp_print_gen_set_lines fmt = pp_print_set_lines 0 fmt
end;;
module HashtblFormat(T: Hashtbl.S) = struct
    let pp_print_map sstart send ssep fmt =
        pp_print_map_iterable T.iter sstart send ssep fmt
    let pp_print_gen_map fmt = pp_print_map "{" "}" ";" fmt
    let pp_print_map_default fmtk fmtv =
        pp_print_gen_map (pp_print_map_entry fmtk fmtv)
    let pp_print_map_lines indent fmt =
        pp_print_map_iterable_lines T.iter indent fmt
    let pp_print_gen_map_lines fmt = pp_print_map_lines 0 fmt
    let pp_print_map_default_lines fmtk fmtv =
        pp_print_gen_map_lines (pp_print_map_entry fmtk fmtv)
end;;

(* BISECT-IGNORE-END *)