open Lwt.Infix
open Redis_tools
open Cmdliner

let on_exn a = print_endline (Printexc.to_string a)

let host =
    let doc = "Host address" in
    let docv = "HOST" in
    Arg.(value & opt string "127.0.0.1" & info ["h"; "host"] ~doc ~docv)

let port =
    let doc = "Proxy port" in
    let docv = "PORT" in
    Arg.(value & opt int 7379 & info ["p"; "port"] ~doc ~docv)

let redis_port =
    let doc = "Redis port" in
    let docv = "REDIS_PORT" in
    Arg.(value & opt int 6379 & info ["r"; "redis"] ~doc ~docv)

let crt_file =
    let doc = "SSL certificate" in
    let docv = "CRT_FILE" in
    Arg.(value & opt string "" & info ["c"; "crt"] ~doc ~docv)

let key_file =
    let doc = "SSL private key" in
    let docv = "KEY_FILE" in
    Arg.(value & opt string "" & info ["k"; "key"] ~doc ~docv)

let main host port redis_port crt_file key_file =
    let pool = Pool.create ~port:redis_port host 16 in
    let rec handler client =
        Client.recv client >>= fun x ->
        Pool.execute pool (fun proxy ->
            Client.send proxy x >>= fun () ->
            Client.recv proxy >>= fun y ->
            Client.send client y >>= fun () ->
            Client.close proxy) >>= fun () ->
            handler client in

    let loop =
        Server.create ~port host >>= fun server ->
        Server.run ~on_exn server handler in
    Lwt_main.run loop

let cmd =
    Term.(const main $ host $ port $ redis_port $ crt_file $ key_file),
    Term.info "redis-proxy" ~version:"%%VERSION%%"

let () = Term.(exit @@ eval cmd)
