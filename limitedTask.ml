open POSIXResource

let limit
      ?mem:mem
      ?time:time
      main failexit failsig =
  match Unix.fork () with
    | 0 ->
        setrlimit RLIMIT_CORE (Limit mem) (Limit mem);
        setrlimit RLIMIT_CPU (Limit time) (Limit time);
        main ();
        exit 0
    | pid ->
        match snd (Unix.waitpid [] pid) with
          | Unix.WEXITED 0 -> ()
          | Unix.WEXITED r -> failexit r
          | Unix.WSIGNALED s -> failsig s
          | Unix.WSTOPPED _ -> failwith "Not expecting stop status"

