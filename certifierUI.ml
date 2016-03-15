(*open MatchTypes*)
open TraceTypes

let bad_path_page path =
  (CertifierData.HTML, <:html<
  <html>
    <head><title>Invalid request</title></head>
    <body>
    Request for invalid reqsource $str:path$
    </body>
  </html>
  >> |> Cow.Html.to_string)

let output_val = let open Types in function 
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

let output_objectid obj = output_val (Types.objectid_to_jsval obj)

let output_ref (ref, ver) =
  let open Reference in match ref with
  | Field (obj, field) -> <:html<$output_objectid obj$.$str:field$@$int:ver$>>
  | LocalVariable name -> <:html<$str:name$@$int:ver$>>
  | GlobalVariable name -> <:html<global:$str:name$@$int:ver$>>


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
  | RCatch { name; ref } ->
    <:html< Catching into $str:name$,
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
  $list:List.map (fun x -> x |> output_op |> rowwrap) tr$
  </table>
  >>

let output_stack st =
  let mode_to_string = let open MatchTypes in function
      |  Regular -> "R"
      | Init -> "I"
      | Wrapper -> "W"
      | External -> "E"
      | ToString -> "T"
      | RegularEnter -> "r"
      | WrapperEnter -> "w"
      | IndirectDefinitionPattern -> "i"
      | ExtraFunctionPattern -> "e"
      | ToStringUpdatePattern -> "u"
      | AliasMatchPattern -> "A"
  in
  let output_mode x = <:html< $str:mode_to_string x$ >> in
  <:html<$list:List.map output_mode st$>>

let error_page e base query =
  <:html<
  <html><body><strong>Error</strong></body></html>
  >>

let match_entry_common = let open MatchTypes in function
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

let shared_css = (CertifierData.CSS, <:css<
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

let rec map_with_commas fmt = function
  | [] -> <:html< (empty)>>
  | [x] -> fmt x
  | x::l -> <:html<$fmt x$, $map_with_commas fmt l$>>

let fmt_node_link self node =
  let node' = string_of_int node in
  <:html< <a href="$str:self [("event", "details"); ("index", node')]$">$str:node'$</a> >>

let fmt_node_link_list self nodes = map_with_commas (fmt_node_link self) nodes

let trace_main_page_trace self rtrace =
  let fmt_node (ev, nodes) =
    <:html<
     <tr><td>$output_op ev$</td><td>$fmt_node_link_list self (List.rev nodes)$</td></tr>
    >> in
  <:html<
  <table>
  <tr><th>Event</th><th>Encountered in</th></tr>
  $list:List.map fmt_node rtrace$
  </table> >>

let trace_main_page_leaves self { CertifierData.nodes } =
  CertifierData.TraceNodes.bindings nodes |>
  List.filter (function (_, CertifierData.NodeData _) -> false | _ -> true) |>
  List.partition (function (_, CertifierData.BlockedData _) -> false | _ -> true) |>
  Misc.bmap (List.map fst) |>
  Misc.bmap (List.sort compare) |>
  (fun (l1, l2) -> l1 @ l2) |>
  fmt_node_link_list self

let trace_main_page self base data =
  <:html< <html><head><title>Trace for $str:base$</title></head>
          <body>
          <a href="#trace1">To first trace</a>
          <a href="#trace2">To second trace</a>
          <h1>Leaves</h1>
          $trace_main_page_leaves self data$
          <h1><a id="trace1">Original trace</a></h1>
          $trace_main_page_trace self (CertifierData.reconstruct_first data)$
          <a href="#trace1">To first trace</a>
          <h1><a id="trace2">Modified trace</a></h1>
          $trace_main_page_trace self (CertifierData.reconstruct_second data)$  
          </body></html> >>

let trace_details_summary = let open CertifierData in function
    | FinalNodeData _ -> <:html< <strong>No extensions at inner node available!</strong> >>
    | NodeData _ -> <:html< Regular inner node >>
    | EndtraceData _ -> <:html< <strong>Transformed trace has ended before original trace was exhausted!</strong> >>
    | InitTailtraceData _ -> <:html< <strong>Tail of transformed code cannot be classified as init code!</strong> >>
    | SuccessNode -> <:html< <strong>Match successful</strong> >>
    | BlockedData _ -> <:html< <strong>Shared block</strong> >> 

let output_mode m = <:html< $str:Fmt.to_to_string MatchTypes.pp_match_mode m$>>

let output_matchop = let open MatchTypes in function
    | MatchSimple -> <:html< Simple match>>
    | MatchPush m -> <:html< Match and push $output_mode m$>>
    | MatchReplace m -> <:html< Match and replace $output_mode m$>>
    | MatchPop -> <:html<Match and pop>>
    | WrapperSimple -> <:html< Simple wrap >>
    | WrapperPush m -> <:html< Wrap and push $output_mode m$>>
    | WrapperPop -> <:html< Wrap and pop >>
    | WrapperReplace m -> <:html< Wrap and replace $output_mode m$>>
    | Initialization -> <:html< Simple init >>
    | InitializationPush m -> <:html< Init and push $output_mode m$>>
    | InitializationPop -> <:html< Init and pop >>
    | MatchDroppable -> <:html<Drop RHS>>

let trace_details_reasons { CertifierData.op1; CertifierData.op2; CertifierData.stack; CertifierData.trace_trace } =
  let open MatchObjects in let open MatchTypes in let open MatchOperations in
  let output_path = function
    | path0 :: path -> List.fold_left (fun x y -> x ^ "," ^ y) path0 path
    | [] -> "the top"
  and output_cond = function
    | MatchSides -> <:html<$output_op op1$ and $output_op op2$ don't match>>
    | MayMatchSimple -> <:html<$output_op op2$ not admissible for a simple match>>
    | MatchCallInt -> <:html<$output_op op1$ and $output_op op2$  not matching internal calls>>
    | MatchCallExt -> <:html<$output_op op1$ and $output_op op2$ not matching external calls>>
    | MatchCallToString -> <:html<$output_op op1$ and $output_op op2$ not matching toString calls>>
    | MatchCallWrap -> <:html<$output_op op2$ cannot possible be a wrapper call>>
    | MayInit -> <:html<$output_op op2$ may not be inserted in simple init code>>
    | IsToplevel -> <:html<$output_op op2$ may not occur at the top level>>
    | IsNotFunction -> <:html<$output_op op2$ is not a call or return>>
    | IsExit -> <:html<$output_op op2$ is not a return>>
    | IsCallInt -> <:html<$output_op op2$ is not an internal call>>
    | IsUnobservable -> <:html<$output_op op2$ is an observable action>>
    | MayInsertInWrapSimple -> <:html<$output_op op2$ may not occur in simple wrapper code>>
    | IsPostExit -> <:html<$output_op op2$ is not a post-exit>>
    | IsEnter -> <:html<$output_op op2$ is not an entry>>
    | MatchEnter -> <:html<$output_op op1$ and $output_op op2$ are not matching function entries>>
    | UseStrictRHS -> <:html<RHS not "use strict">>
    | IsCatch -> <:html<$output_op op2$ is not a catch>>
    | IsFunLiteral -> <:html<$output_op op2$ is not a function literal>>
    | IsLocalDecl -> <:html<$output_op op2$ is not a local declaration>>
    | IsFunRead -> <:html<$output_op op2$ is not a function read>>
    | IsEndOfExpr -> <:html<$output_op op2$ is not end-of-expr>>
    | IsAliasMatch -> <:html<$output_op op1$ and $output_op op2$ are not an alias pair>>
    | MatchAliasWrites  -> <:html<$output_op op1$ and $output_op op2$ are not matching alias writes>>
  in
  let output_obj_match_trace = function
    | NonMatching (path, val1, val2) ->
      <:html<At $str:output_path path$, $output_val val1$ does not match $output_val val2$>>
    | MissingOrig (fld, path) -> <:html< Missing field $str:fld$ at $str:output_path path$ in the set of objects for the original trace. >>
    | MissingXfrm (fld, path) -> <:html< Missing field $str:fld$ at $str:output_path path$ in the set of objects for the transformed trace. >>
    | Other reason -> <:html< $str:reason$ >>
  and output_function_mismatch = function
    | DifferentBodies (b1, b2) -> <:html<Function mismatch: <blockquote>$str:b1$</blockquote> vs. <blockquote>$str:b2$</blockquote> >>
    | DifferentInstrumentedBodies (b1, b2) -> <:html<Function mismatch (from_toString bodies): <blockquote>$str:b1$</blockquote> vs. <blockquote>$str:b2$</blockquote> >>
    | DifferentExternal (id1, id2) -> <:html<External function mismatch: $int:id1$ vs. $int:id2$>>
    | InconsistentlyInstrumented -> <:html<from_toString vs. from_jalangi function>>
    | InternalExternal -> <:html<Internal vs. external function>>
  in
  let rec output_reason = function
    | DifferentType -> <:html<Different types>>
    | DifferentObjects (where, failure) ->
      <:html<Objects in $str:where$ don't match: $output_obj_match_trace failure$>>
    | DifferentArguments -> <:html<Referencing different arguments>>
    | DifferentValues name -> <:html<Values for $str:name$ differ>>
    | DifferentOperations -> <:html<Comparing incomparable events>>
    | OtherOperation -> <:html<Event of a non-appropriate type>>
    | NotToString -> <:html<Not a call to toString>>
    | NotInitData -> <:html<Variable does not contain init data>>
    | NotSimpleMatchable -> <:html<Cannot be matched by simple matching>>
    | NotWrapCode -> <:html<Cannot occur in wrapper code>>
    | NotToStringCode -> <:html<Cannot occur in toString code>>
    | ExternalCall -> <:html<The call is external>> 
    | InternalCall -> <:html<The call is internal>>
    | NotLiterallyEqual -> <:html<The function bodies are not literally equal>> 
    | LiterallyEqual -> <:html<The function bodies are literally equal>>
    | NotToplevel -> <:html<Not top level code>>
    | NotFunction -> <:html<Not a function entry>>
    | NotExit -> <:html<Not a function exit>>
    | Observable -> <:html<Event is observable>>
    | NotAtToplevel -> <:html<Not at top level>>
    | NotFunctionUpdate -> <:html<Not a function update>>
    | NotInitCode -> <:html<Not init code>>
    | NotEnter -> <:html<Not a function entry>>
    | FunctionMismatch reason -> output_function_mismatch reason
    | NotUseStrict -> <:html<Not "use strict">>
    | NotCatch -> <:html<Not catch>>
    | And (r1, r2) -> <:html<$output_reason r1$ and $output_reason r2$>> in
  let output_cond (cond, reason) =
    <:html<$output_cond cond$: $output_reason reason$>> in
  let output_nonempty = function
    | [] -> <:html<Non-empty list???>>
    | [x] -> <:html<$x$>>
    | l -> let l' = List.map (fun x -> <:html< <li>$x$</li> >>) l in
      <:html< <ul> $list:l'$ </ul> >> in
  let output_trace_entry (conds, op) =
    <:html<
      <dt>$output_matchop op$</dt>
      <dd>$output_nonempty (List.map output_cond conds)$</dd>
    >> in
  <:html<
     <p>Operations being matched: $output_op op1$ versus $output_op op2$</p>
     <p>Current stack: $output_stack stack$</p>
     <p>Failed matching rules:</p>
     <dl>
     $list:List.map output_trace_entry trace_trace$
     </dl>
    >>

let trace_details_cases self tree idx =
  let open CertifierData in
  let output_node_link idx label = 
    <:html< <a href="$str:self [("event", "details"); ("index", string_of_int idx)]$">$label$</a> >>
  and output_label e = output_matchop (TraceTree.E.label e)
  in let output_predecessor tree idx = 
  match CertifierData.TraceTree.pred_e tree idx with
    | [ e ] -> output_node_link (TraceTree.E.src e) (output_label e)
    | [] -> <:html<No predecessor>>
    | _ -> <:html<Multiple predecessors?!>>
  and try_fast_forward = function
    | Some i -> output_node_link i <:html< Fast forward >>
    | None -> <:html< >>
  and output_follower e = 
    <:html< <li>$output_node_link (TraceTree.E.dst e) (output_label e)$</li> >>
  in function
    | FinalNodeData data ->
      <:html< <h2>Predecessor node</h2>$output_predecessor tree idx$
              <h2>Reasons why no more matching is possible:</h2>$trace_details_reasons data$ >>
    | NodeData data ->
      <:html< <h2>Follower nodes</h2>
              <ul>$list:TraceTree.fold_succ_e (fun e l -> output_follower e :: l) tree idx []$</ul> 
              $try_fast_forward (find_fast_forward tree idx)$
              <h2>Predecessor node</h2>$output_predecessor tree idx$
              <h2>Reasons why other matchings have been ruled out:</h2>$trace_details_reasons data$ >>
    | EndtraceData tr ->
      <:html< <h2>Leftover trace</h2>$output_trace tr$>>
    |  InitTailtraceData (tr, st) ->
      <:html<
      <h2>Predecessor node</h2>$output_predecessor tree idx$
      <h2>Leftover trace</h2>$output_trace tr$
      <h2>Stack</h2>$output_stack st$
    >>
    | SuccessNode ->
      <:html< Match successful!
      <h2>Predecessor node</h2>$output_predecessor tree idx$>>
    | BlockedData (len1, len2, stack) ->
      <:html< Blocked: A suffix pair of lengths $int:len1$, $int:len2$ is known not to match for stack $output_stack stack$
           <h2>Predecessor node</h2>$output_predecessor tree idx$ >> 

let trace_details self base { CertifierData.tree; CertifierData.nodes } idx =
  let node_class = CertifierData.TraceNodes.find idx nodes
  and link i = self [("event", "details"); ("index", string_of_int i)] in
  let output_matchop = let open MatchTypes in function
      | Pair(op1, op2) ->
        <:html< <td>$output_op op1$</td><td>$output_op op2$</td><td></td> >>
      | Init op ->
        <:html< <td></td><td>$output_op op$</td><td>Init</td> >>
      | Wrap op ->
        <:html< <td></td><td>$output_op op$</td><td>Wrap</td> >> in
  let build_partial_trace idx =
    CertifierData.collect_trace idx tree nodes
    |> List.map (fun (idx, stack, matchop) -> 
        <:html< <tr>
                <td><a href="$str:link idx$">$int:idx$</a></td>
                <td>$output_stack stack$</td>
                $output_matchop matchop$
                </tr> >>) in
  <:html<
  <html>
    <head>
      <title>Details for $str:base$, node $int:idx$</title>
      <link href="stylesheet.css" rel="stylesheet"/>
    </head>
    <body>
      <h1>Details for $str:base$, node $int:idx$</h1>
      <p>Executive summary: $trace_details_summary node_class$</p>
      <h2>Partial matching</h2>
      <table class="partial_trace">
        <tr>
          <th class="num"></th>
          <th class="stack">Stack</th>
          <th class="original">Original trace</th>
          <th class="modified">Modified trace</th>
          <th class="classification">Classification</th>
        </tr>
        $list:build_partial_trace idx$
      </table>
      $trace_details_cases self tree idx node_class$
    </body>
  </html> >>

let trace_multiplex self base data query =
  match query "event" with
  | Some "details" ->
    begin match query "index" with
      | Some idx -> Lwt.return (CertifierData.HTML, trace_details self base data (int_of_string idx) |> Cow.Html.to_string)
      | None -> raise (Invalid_argument "Needs exactly one index")
    end
  | Some "graph" ->
    CertifierGraph.format (base ^ ".png") (fun v -> self [("event", "details"); ("index", string_of_int v)]) data
    |> Lwt.map (fun out -> (CertifierData.PNG, out))
  | None -> Lwt.return (CertifierData.HTML, trace_main_page self base data |> Cow.Html.to_string)
  | _ -> raise (Invalid_argument "Unknown event given")

let list_certs self certs =
  let fmt_result = function
    | Some 0 -> "(match)"
    | Some 1 -> "(no match)"
    | Some 2 -> "(failed)"
    | Some i -> "(unknown result: " ^ string_of_int i ^ ")"
    | None -> "(can't read state)" in
  let link_cert (name, result) =
    <:html< <a href="$str:self name$">$str:name$</a> $str:fmt_result result$<br/> >>
  in
  (CertifierData.HTML, <:html<
    <html><head><title>List of certificates</title></head>
    <body>$list:List.map link_cert certs$</body></html> >> |> Cow.Html.to_string)

let bad_path path =
  (CertifierData.HTML, <:html<
  <html>
    <head><title>Invalid request</title></head>
    <body>
    Request for invalid reqsource $str:path$
    </body>
  </html>
  >> |> Cow.Html.to_string)
