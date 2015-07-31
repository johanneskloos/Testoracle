open Lwt
open Cohttp
open Cohttp_lwt_unix
open Trace
open Richtrace
open MatchTraces
open Cleantrace
open Reference
open Graph

(** Part 1: Infrastructure. *)
let output_ref (ref, ver) =
  match get_fieldref ref, get_name ref, is_global ref with
    | Some (obj, field), None, _ -> <:html<obj#$int:obj$.$str:field$@$int:ver$>>
    | None, Some name, false -> <:html<$str:name$@$int:ver$>>
    | None, Some name, true -> <:html<global:$str:name$@$int:ver$>>
    | _ -> assert false

let output_val = function
  | OUndefined -> <:html<(undefined)>>
  | ONull -> <:html<(null)>>
  | OBoolean x -> <:html<$str:string_of_bool x$>>
  | ONumberInt x -> <:html<$int:x$>>
  | ONumberFloat x -> <:html<$flo:x$>>
  | OString x -> <:html<"$str:x$">>
  | OSymbol x -> <:html<symbol:$str:x$>>
  | OObject id -> <:html<obj#$int:id$>>
  | OFunction(id, fid) -> <:html<fun#$int:id$/$int:fid$>>
  | OOther (ty, id) -> <:html<$str:ty$#$int:id$>>


let output_op =
  let output_call_type = function
    | Function -> <:html<function>>
    | Method -> <:html<method>>
    | Constructor -> <:html<constructor>>
    | ConstructorMethod -> <:html<constructor/method>>
  and output_reason = function
    | Argument i -> <:html<it binds argument $int:i$>>
    | With ref -> <:html<it aliases $output_ref ref$>>  in 
  function
  | RFunPre { f; base; args; call_type } ->
    <:html< Calling $output_call_type call_type$
       $output_val f$ on $output_val base$ with
        argument object $output_val args$
     >> 
  | RFunPost { f; base; args; result } ->
    <:html< Called $output_val f$ on $output_val base$
      with argument object $output_val args$; result is $output_val result$
    >>
  | RLiteral { value; hasGetterSetter } ->
    if hasGetterSetter then
      <:html< Literal $output_val value$ with getter and/or setter >>
    else
      <:html< Literal $output_val value$>>
  | RForIn x ->
    <:html< for/in on $output_val x$>>
  | RLocal { name; ref } ->
    <:html< Declaring local variable $str:name$,
    mapped to reference $output_ref ref$>>
  | RAlias { name; source; ref } ->
    <:html< Declaring alias $str:name$ for
    reference $output_ref ref$ because
    $output_reason source$>>
  | RRead { ref; value } ->
    <:html< Reading reference $output_ref ref$ yields $output_val value$>>
  | RWrite { ref; oldref; value; success } ->
    if success then
      <:html< Updating $output_ref oldref$ to $output_ref ref$ with $output_val value$>>
    else
      <:html< Unsuccessful update from $output_ref oldref$ to $output_ref ref$ with $output_val value$>>
  | RReturn x -> <:html<Returning $output_val x>>
  | RThrow x -> <:html<Throwing $output_val x>>
  | RWith x -> <:html<with $output_val x>>
  | RFunEnter { f; this; args } ->
    <:html<Entering $output_val f$ on $output_val this$ with argument object $output_val args$>>
  | RFunExit { ret; exc } ->
    <:html<Exiting function with return value $output_val ret$ and exception $output_val exc$>>
  | RScriptEnter -> <:html<Entering script>>
  | RScriptExit -> <:html<Exiting script>>
  | RScriptExc x -> <:html<Existing script with exception $output_val x$>>
  | RBinary { op; left; right; result } ->
    <:html<$output_val left$ $str:op$ $output_val right$ gives $output_val result$>>
  | RUnary { op; arg; result } ->
    <:html<$str:op$ $output_val arg$ gives $output_val result$>>
  | REndExpression -> <:html<(discarding result)>>
  | RConditional x -> <:html<condition value: $output_val x$>>

let output_trace tr =
  let rowwrap x = <:html< <tr><td>$x$</td></tr> >> in
  <:html<
  <table class="trace">
  $list:List.map (fun (x, _) -> x |> output_op |> rowwrap) tr$
  </table>
  >>

let output_stack st =
  let mode_to_string = function
    |  Regular -> "R"
    | Init -> "I"
    | Wrapper -> "W"
    | External -> "E"
    | ToString -> "T" in
  let output_mode x = <:html< $str:mode_to_string x$ >> in
   <:html<$list:List.map output_mode st$>>

type output_type = HTML | JSON | CSS

let error_page e base query =
  <:html<
  <html><body><strong>Error</strong></body></html>
  >>

let match_entry_common = function
    | Pair(op1, op2) ->
      <:html<
          <td class="original">$output_op op1$</td>
          <td class="modified">$output_op op2$</td>
          <td class="classification"></td>
      >>
    | Wrap op ->
      <:html<
          <td class="original"></td>
          <td class="modified">$output_op op$</td>
          <td class="classification">Wrap</td>
      >>
    | Init op ->
      <:html<
          <td class="original"></td>
          <td class="modified">$output_op op$</td>
          <td class="classification">Init</td>
      >> 
let match_entry e = <:html< <tr>$match_entry_common e$</tr> >>
let match_entry_ext (link: int -> string) (id, st, e) =
  <:html< <tr><td class="num"><a href="$str:link id$">$int:id$</a></td><td class="stack">$output_stack st$</td>$match_entry_common e$</tr> >>

let shared_css = (CSS, <:css<
        th { text-align: left; }
        .num { width: "7em"; }
        .stack { width: "20em"; }
        .original { width: "80em"; }
        .modified {width: "80em"; }
        .classification { width: "20em"; }
        table.trace {
          table-layout: fixed;
        }
        table.partial_trace {
          display: block;
          table-layout: fixed;
          max-height: 30vh;
          overflow-y: scroll;
          position: relative;
          bottom: 0;
        }
>> |> Cow.Css.to_string)

(** Part 2: Handling of successful matches. *)
type success_data = match_type list

let success_main_page base sd =
  <:html<
  <html>
    <head>
      <title>Successful match for $str:base$</title>
      <link href="stylesheet.css" rel="stylesheet"/>
    </head>
    <body>
      <h1>Match for $str:base$</h1>
      <p>Matching was succesful. Certificate:</p>
      <table class="trace">
        <tr>
          <th class="original">Original trace</th>
          <th class="modified">Modified trace</th>
          <th class="classification">Classification</th>
        </tr>
        $list:List.map match_entry sd$
      </table>
    </body>
  </html>
  >> 

let success_create_data _ _ ml = ml
let success_multiplex base data _ =
  (HTML, success_main_page base data |> Cow.Html.to_string)
 
(** Part 3: Handling of failed matches. *)
module IntSig = struct
  type t = int
  let compare: t -> t -> int = compare
  let hash (x: t) = x
  let equal: int -> int -> bool = (=)
end
module MatchOp = struct
  type t = match_operation
  let compare: t -> t -> int = compare
  let default: t = MatchSimple
end;;
module FailureTree = Graph.Persistent.Digraph.ConcreteLabeled(IntSig)(MatchOp);;
module FailureNodes = Map.Make(IntSig);;

type failure_node =
    FinalNodeData of matching_anti_tree_node
  | NodeData of matching_anti_tree_node
  | EndFailureData of rich_trace
  | InitTailFailureData of rich_trace * mode list

type failure_node_data = { partial_match: (int * mode list * match_type) list; node_class: failure_node }
type failure_data = { tree: FailureTree.t; nodes: failure_node_data FailureNodes.t }
  
let failure_main_page base fd =
  <:html< <html><head><title>Not implemented</title></head><body>Not implemented</body></html> >>

let failure_treedata { tree }: Yojson.Basic.json =
  let open Yojson.Basic in
  let out_label = Misc.to_string pp_match_operation in 
  let out_edge edge: json =
    `Assoc [ ("op", `String (FailureTree.E.label edge |> out_label)); ("node", `Int (FailureTree.E.dst edge)) ] in
  let out_succ_edges v: string * json =
    (string_of_int v, `List (FailureTree.fold_succ_e (fun edge lout -> out_edge edge :: lout) tree v [])) in
  `Assoc (FailureTree.fold_vertex (fun vertex l -> out_succ_edges vertex :: l) tree [])

let failure_details_summary = function
  | FinalNodeData _ -> <:html< <strong>No extensions at inner node available!</strong> >>
  | NodeData _ -> <:html< Regular inner node >>
  | EndFailureData _ -> <:html< <strong>Transformed trace has ended before original trace was exhausted!</strong> >>
  | InitTailFailureData _ -> <:html< <strong>Tail of transformed code cannot be classified as init code!</strong> >>

let output_mode m = <:html< $str:Misc.to_string pp_print_mode m$>>
 
let output_matchop = function
    | MatchSimple -> <:html< Simple match>>
    | MatchPush m -> <:html< Match and push $output_mode m$>>
    | MatchPop -> <:html<Match and pop>>
    | WrapperSimple -> <:html< Simple wrap >>
    | WrapperPush m -> <:html< Wrap and push $output_mode m$>>
    | WrapperPop -> <:html< Wrap and pop >>
    | Initialization -> <:html< Simple init >>
    | InitializationPush m -> <:html< Init and push $output_mode m$>>
    | InitializationPop -> <:html< Init and pop >>

let failure_details_reasons { op1; op2; stack; failure_trace= (high, low) } =
  let open MatchObjects in
  let output_path = function
    | path0 :: path -> List.fold_left (fun x y -> x ^ "," ^ y) path0 path
    | [] -> "the top"
  and output_cond = function
    | MatchSides -> <:html<$output_op op1$ and $output_op op2$ don't match (check below for possible object mismatches)>>
    | MayMatchSimple -> <:html<$output_op op2$ not admissible for a simple match (probably a call or return)>>
    | MatchCallInt -> <:html<$output_op op1$ and $output_op op2$  not matching internal calls (TODO give more details when possible)>>
    | MatchCallExt -> <:html<$output_op op1$ and $output_op op2$ not matching external calls (TODO give more details when possible)>>
    | MatchCallToString -> <:html<$output_op op1$ and $output_op op2$ not matching toString calls (TODO give more details when possible)>>
    | MatchCallWrap -> <:html<$output_op op2$ cannot possible be a wrapper call (TODO give more details)>>
    | MayInit -> <:html<$output_op op2$ may not be inserted in simple init code>>
    | IsToplevel -> <:html<$output_op op2$ may not occur at the top level>>
    | IsNotFunction -> <:html<$output_op op2$ is not a call or return>>
    | IsExit -> <:html<$output_op op2$ is not a return>>
    | IsCallInt -> <:html<$output_op op2$ is not an internal call>>
    | IsUnobservable -> <:html<$output_op op2$ is an observable action>>
    | MayInsertInWrapSimple -> <:html<$output_op op2$ may not occur in simple wrapper code>>
     in
  let output_failcond (conds, op) =
    <:html<
      <dt>$output_matchop op$</dt>
      <dd>$list:List.map output_cond conds$</dd>
    >>
  and output_obj_match_failure = function
    | NonMatching (path, val1, val2) ->
      <:html<At $str:output_path path$, $output_val val1$ does not match $output_val val2$>>
    | MissingOrig path -> <:html< Missing field at $str:output_path path$ in the set of objects for the original trace. >>
    | MissingXfrm path -> <:html< Missing field at $str:output_path path$ in the set of objects for the transformed trace. >>
    | Other reason -> <:html< $str:reason$ >> in
  let output_high = function
    | [] -> <:html< No failed conditions? >>
    | high -> <:html< <h3>Conditions that failed</h3> <dl>$list:List.map output_failcond high$</dl> >>
  and output_low = function
    | None -> <:html< >>
    | Some (reason, match_failure) ->
      <:html<
        <h2>Object match failure</h2>
        <p>Where: $str:reason$</p>
        <p>Reason: $output_obj_match_failure match_failure$</p>
      >>
  in
  <:html<
     <p>Operations being matched: $output_op op1$ versus $output_op op2$</p>
     <p>Current stack: $output_stack stack$</p>
     $output_high high$
     $output_low low$
    >>

let failure_details_cases self tree idx =
  let output_follower e =
    <:html<
      <li><a href="$str:self [("operation", "details"); ("index", string_of_int (FailureTree.E.dst e))]$">$output_matchop (FailureTree.E.label e)$</a></li>
    >>
  in function
  | FinalNodeData data ->
    <:html< <h2>Reasons why no more matching is possible:</h2>$failure_details_reasons data$ >>
  | NodeData data ->
    <:html< <h2>Follower nodes</h2>
      <ul>$list:FailureTree.fold_succ_e (fun e l -> output_follower e :: l) tree idx []$</ul> 
      <h2>Reasons why other matchings have been ruled out:</h2>$failure_details_reasons data$ >>
  | EndFailureData tr ->
    <:html< <h2>Leftover trace</h2>$output_trace tr$>>
  |  InitTailFailureData (tr, st) ->
    <:html<
      <h2>Leftover trace</h2>$output_trace tr$
      <h2>Stack</h2>$output_stack st$
    >>
    
let failure_details self base { tree; nodes } idx =
  let { partial_match; node_class } = FailureNodes.find idx nodes
  and link i = self [("operation", "details"); ("index", string_of_int i)] in
  <:html<
  <html>
    <head>
      <title>Details for $str:base$, node $int:idx$</title>
      <link href="stylesheet.css" rel="stylesheet"/>
    </head>
    <body>
      <h1>Details for $str:base$, node $int:idx$</h1>
      <p>Executive summary: $failure_details_summary node_class$</p>
      <h2>Partial matching</h2>
      <table class="partial_trace">
        <tr>
          <th class="num"></th>
          <th class="stack">Stack</th>
          <th class="original">Original trace</th>
          <th class="modified">Modified trace</th>
          <th class="classification">Classification</th>
        </tr>
        $list:List.map (match_entry_ext link) partial_match$
      </table>
      $failure_details_cases self tree idx node_class$
    </body>
  </html> >>

let extend_pm idx stack pm (tr1: rich_trace) (tr2: rich_trace) op =
    let split = function (x, _):: l -> (x, l) | [] -> failwith "How can this be an empty list?" in
    let (op2, tr2) = split tr2 in
    let stack' = match op with
      | WrapperPush m | MatchPush m | InitializationPush m -> m :: stack
      | WrapperPop | MatchPop | InitializationPop -> List.tl stack
      | WrapperSimple | MatchSimple | Initialization -> stack in 
    match op with
    | WrapperSimple | WrapperPush _ | WrapperPop ->
        (pm @ [idx, stack', Wrap op2], stack', tr1, tr2)
    | MatchSimple | MatchPush _ | MatchPop ->
        let (op1, tr1) = split tr1 in
        (pm @ [idx, stack', Pair (op1, op2)], stack', tr1, tr2)
    | Initialization | InitializationPush _ | InitializationPop ->
        (pm @ [idx, stack', Init op2], stack', tr1, tr2)

let failure_create_data (tr1: rich_trace) (tr2: rich_trace) data =
    let id = ref 0 in
    let rec flatten_mat partial_match stack (tr1: rich_trace) (tr2: rich_trace) { tree; nodes } =
        let here = !id in
        let tree = FailureTree.add_vertex tree here in incr id;
        function
        | Node (dat, []) ->
            { tree;
                nodes = FailureNodes.add here { partial_match; node_class = FinalNodeData dat } nodes }
        | Node (dat, sub) ->
            { tree;
                nodes = FailureNodes.add here { partial_match; node_class = NodeData dat } nodes }
            |> flatten_edges here partial_match stack tr1 tr2 sub
        | EndFailure tr ->
            { tree;
                nodes = FailureNodes.add here { partial_match; node_class = EndFailureData tr } nodes }
        | InitTailFailure (tr, st) ->
            { tree;
                nodes = FailureNodes.add here { partial_match; node_class = InitTailFailureData(tr, st) } nodes }
    and flatten_edges parent partial_match stack (tr1: rich_trace) (tr2: rich_trace) sub data =
        match sub with
        | (op, mat):: rest ->
            let (partial_match', stack', tr1', tr2') = extend_pm !id stack partial_match tr1 tr2 op in
            data
            |> fun { tree; nodes } -> {
                    tree = FailureTree.add_edge_e tree (FailureTree.E.create parent op !id);
                    nodes
                }
            |> fun data -> flatten_mat partial_match' stack' tr1' tr2' data mat
            |> flatten_edges parent partial_match stack tr1 tr2 rest
        | [] -> data
    in flatten_mat [] [] tr1 tr2 { tree = FailureTree.empty; nodes = FailureNodes.empty } data

let failure_multiplex self base data query =
  match query "operation" with
    | Some "treedata" -> (JSON, failure_treedata data |> Yojson.Basic.to_string)
    | Some "details" ->
        begin match query "index" with
          | Some idx -> (HTML, failure_details self base data (int_of_string idx) |> Cow.Html.to_string)
          | None -> raise (Invalid_argument "Needs exactly one index")
        end
    | None ->  (HTML, failure_main_page base data |> Cow.Html.to_string)
    | _ -> raise (Invalid_argument "Unknown operation given")

(** Part 4: The server. *)
type server_data = SuccessData of success_data | FailureData of failure_data

let read_result key =
  Format.eprintf "Data not cached, read from serialized form...@.";
  let (trorig, trxfrm, data) = Marshalling.unmarshal ("." ^ key ^ ".cert") in
  Format.eprintf "Data unserialized.@.";
  match data with
    | Success data -> Format.eprintf "Transforming success data...@."; SuccessData (success_create_data trorig.trace trxfrm.trace data)
    | Failure data -> Format.eprintf "Transforming failure data...@."; FailureData (failure_create_data trorig.trace trxfrm.trace data)


let server_callback cache conn req body =
    Format.eprintf "Received request@.";
    let uri = req |> Request.uri in
    let path = Uri.path uri
    and query key = Uri.get_query_param uri key
    and self query' = Uri.with_query' uri query' |> Uri.to_string in
    Format.eprintf "Parsed request, getting data from cache...@.";
    begin
        match path with
        | "/stylesheet.css" -> shared_css
        | _ ->
            match cache path with
            | SuccessData data -> Format.eprintf "Read success case@."; success_multiplex path data query
            | FailureData data -> Format.eprintf "Read failure case@."; failure_multiplex self path data query
    end |> begin function
        | (HTML, body) -> Format.eprintf "Returning HTML@."; ("text/html", body)
        | (JSON, body) -> Format.eprintf "Returning JSON@."; ("application/json", body)
        | (CSS, body) -> Format.eprintf "Returning CSS@."; ("text/css", body)
    end |> fun (ctype, body) ->
        Server.respond_string ~status:`OK ~headers: (Header.init_with "Content-type" ctype) ~body ()

let () =
  Format.eprintf "Starting...@.";
  let cache = BatCache.lru_cache ~gen:read_result ~cap:10 in 
  Server.create (Server.make ~callback:(server_callback cache) ())
  |> Lwt_main.run
