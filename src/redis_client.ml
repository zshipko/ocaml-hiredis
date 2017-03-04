exception Disconnected_client

let (>>=) = Lwt.bind

module Client = struct
    type t = {
        mutable authenticated : bool;
        tls_config : Conduit_lwt_unix.client_tls_config option;
        mutable sock :
            (Conduit_lwt_unix.flow *
             Conduit_lwt_unix.ic *
             Conduit_lwt_unix.oc) option;
    }

    let init ?authenticated:(authenticated=false) host port =
        let ip = Ipaddr.of_string_exn host in {
        authenticated = authenticated;
        tls_config =
            Some (`Hostname host, `IP ip, `Port port);
        sock = None;
    }

    let connect ?unix ?tls:(tls=false) ?ctx:(ctx=Conduit_lwt_unix.default_ctx) cli =
        (* If we're already connected then return true right away *)
        match cli.sock with
        | Some s -> Lwt.return true
        | None ->
            (* If no address is given it is most likely a client connected to a server *)
            let addr = match cli.tls_config with
                | Some x -> x | None -> failwith "No address provided" in

            (* Unix or tcp? *)
            begin match unix with
            | Some s ->
                Conduit_lwt_unix.connect ~ctx
                    (`Unix_domain_socket (`File s))
            | None ->
                let _, ip, port = addr in
                Conduit_lwt_unix.connect ~ctx
                    (if tls then `TLS addr else `TCP (ip, port)) end
            >>= fun conn ->
                cli.sock <- Some conn;
                Lwt.return true

    let rec read_all ic =
        Lwt_io.read ~count:(Lwt_io.buffered ic) ic >>= fun s ->
            if Lwt_io.buffered ic > 0 then
                read_all ic >>= fun x ->
                Lwt.return (s ^ x)
            else Lwt.return s

    let recv cli =
        match cli.sock with
        | Some (flow, ic, oc) ->
            read_all ic >>= fun s ->
            Lwt.return (Redis_protocol.Redis.Resp.decode_exn s)
        | _ -> raise Disconnected_client

    let send cli data =
        match cli.sock with
        | Some (flow, ic, oc) ->
            Lwt_io.write oc (Redis_protocol.Redis.Resp.encode_exn data) >>= fun _ ->
            Lwt_io.flush oc
        | _ -> raise Disconnected_client

    let run cli command args =
        connect cli >>= fun _ ->
        send cli (Redis_protocol.Redis.Redis_command.build ~command args) >>= fun _ ->
        recv cli

    let close cli =
        match cli.sock with
        | Some (flow, ic, oc) ->
            Lwt.catch (fun () -> Lwt_io.close ic) (fun _ -> Lwt.return_unit) >>= fun _ ->
            Lwt.catch (fun () -> Lwt_io.close oc) (fun _ -> Lwt.return_unit) >>= fun _ ->
                cli.sock <- None;
                Lwt.return_unit
        | None -> Lwt.return_unit

end
