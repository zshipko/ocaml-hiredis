exception Disconnected_client

let (>>=) = Lwt.bind

module Client = struct
    type t = {
        config: Conduit_lwt_unix.client option;
        ctx: Conduit_lwt_unix.ctx;
        mutable sock :
            (Conduit_lwt_unix.flow *
             Conduit_lwt_unix.ic *
             Conduit_lwt_unix.oc) option;
    }

    let init ?tls_config ?unix ?host:(host="127.0.0.1") ?port:(port=6379) ?ctx:(ctx=Conduit_lwt_unix.default_ctx) () =
        let config = match unix with
        | Some s -> (`Unix_domain_socket (`File s))
        | None -> begin
            match tls_config with
            | Some tls -> `TLS tls
            | None -> `TCP (`IP (Ipaddr.of_string_exn host), `Port port)
        end in {
            config = Some config;
            ctx = ctx;
            sock = None;
        }

    let connect cli =
        (* If the client is already connected then return true right away *)
        match cli.sock, cli.config with
        | Some s, _ -> Lwt.return true
        | None, Some cfg ->
            Conduit_lwt_unix.connect ~ctx:cli.ctx cfg >>= fun conn ->
                cli.sock <- Some conn;
                Lwt.return_true
        | _, _ -> raise Disconnected_client

    let close cli =
        match cli.sock with
        | Some (flow, ic, oc) ->
            Lwt.catch (fun () -> Lwt_io.close ic) (fun _ -> Lwt.return_unit) >>= fun _ ->
            Lwt.catch (fun () -> Lwt_io.close oc) (fun _ -> Lwt.return_unit) >>= fun _ ->
                cli.sock <- None;
                Lwt.return_unit
        | None -> Lwt.return_unit

    let rec read_all ic =
        Lwt_io.read ~count:(Lwt_io.buffered ic) ic >>= fun s ->
            if Lwt_io.buffered ic > 0 then
                read_all ic >>= fun x ->
                Lwt.return (s ^ x)
            else Lwt.return s

    let recv cli =
        match cli.sock with
        | Some (flow, ic, oc) ->
            Lwt.catch (fun () ->
                read_all ic >>= fun s ->
                Lwt.return (Redis_protocol.Redis.Resp.decode_exn s))
            (fun exc -> close cli >>= fun _ -> raise exc)
        | _ -> raise Disconnected_client

    let send cli data =
        match cli.sock with
        | Some (flow, ic, oc) ->
            Lwt.catch (fun () ->
                Lwt_io.write oc (Redis_protocol.Redis.Resp.encode_exn data) >>= fun _ ->
                Lwt_io.flush oc)
            (fun exc -> close cli >>= fun _ -> raise exc)
        | _ -> raise Disconnected_client

    let run_string cli command args =
        connect cli >>= fun _ ->
        send cli (Redis_protocol.Redis.Redis_command.build ~command args) >>= fun _ ->
        recv cli

    let run cli command args =
        connect cli >>= fun _ ->
        send cli (Redis_protocol.Redis.Redis_command.build ~command (List.map Redis_protocol.Redis.Conv.string args)) >>= fun _ ->
        recv cli

end
