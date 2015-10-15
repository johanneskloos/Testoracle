let read_result key =
    let data = MatchTracesObserver.read ("." ^ key ^ ".cert") in
    CertifierData.extract_data data

let get_certs () =
    let rec getdir handle list =
        try
            getdir handle (Unix.readdir handle :: list)
        with End_of_file ->
            Unix.closedir handle; list
    in
    let read_status name =
        try
            let chan = open_in (name ^ ".result") in
            let res = int_of_string (input_line chan) in
            close_in chan;
            (name, Some res)
        with _ -> (name, None) in
    getdir (Unix.opendir ".") []
    |> List.filter (fun name -> Filename.check_suffix name ".cert")
    |> List.map (fun name -> Filename.chop_suffix name ".cert")
    |> List.map read_status
    |> List.sort (fun (n1, _) (n2, _) -> String.compare n1 n2)

let good_path = Str.regexp "^/[^/]*$"

let server_callback cache conn req body =
    let uri = Cohttp.Request.uri req in
    Format.eprintf "Handling %s@." (Uri.to_string uri);
    let path = Uri.path uri
    and query key = Uri.get_query_param uri key
    and self query' = Uri.with_query' uri query' |> Uri.to_string
    and page base = Uri.with_path uri base |> Uri.to_string
    and small_response mimetype body =
      Cohttp_lwt_unix.Server.respond_string ~status:`OK
        ~headers: (Cohttp.Header.init_with "Content-type" mimetype) ~body ()
    and file_response mimetype fname =
      Cohttp_lwt_unix.Server.respond_file ~fname
        ~headers: (Cohttp.Header.init_with "Content-type" mimetype) () in
    try
        begin
            match path with
            | "/stylesheet.css" -> Lwt.return CertifierUI.shared_css
            | "" | "/" -> Lwt.return (CertifierUI.list_certs page (get_certs()))
            | _ when Str.string_match good_path path 0 ->
                let data = cache path in
                Lwt.return (CertifierUI.trace_multiplex self path data query)
            | _ -> Lwt.return (CertifierUI.bad_path path)
        end |> fun futute -> Lwt.bind future begin function
            | (CertifierData.HTML, body) -> small_response "text/html" body
            | (CertifierData.JSON, body) -> small_response "application/json" body
            | (CertifierData.CSS, body) -> small_response "text/css" body
            | (CertifierData.PNG, filename) -> file_response "image/png" fname
        end
    with e -> Format.eprintf "%s@." (Printexc.to_string e); raise e

let () =
    Format.eprintf "Starting...@.";
    let cache = BatCache.lru_cache ~gen: read_result ~cap:10 in
    Cohttp_lwt_unix.Server.create (Cohttp_lwt_unix.Server.make ~callback: (server_callback cache) ())
    |> Lwt_main.run
