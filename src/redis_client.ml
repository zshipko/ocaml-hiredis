open Lwt.Infix

exception Disconnected_client

module Client = struct
    type t = {
        config: Conduit_lwt_unix.client option;
        ctx: Conduit_lwt_unix.ctx;
        mutable c : (Conduit_lwt_unix.flow * Conduit_lwt_unix.ic * Conduit_lwt_unix.oc) option;
    }

    let default_addr = `TCP (`IP (Ipaddr.of_string_exn "127.0.0.1"), `Port 6379)

    let init ?ctx:(ctx=Conduit_lwt_unix.default_ctx) ?config:(config=default_addr) () =
        {
            config = Some config;
            ctx = ctx;
            c = None;
        }

    let connect cli =
        (* If the client is already connected then return true right away *)
        match cli.c, cli.config with
        | Some _,  _ -> Lwt.return true
        | None, Some cfg ->
            Conduit_lwt_unix.connect ~ctx:cli.ctx cfg >>= fun (_flow, ic, oc) ->
                cli.c <- Some (_flow, ic, oc);
                Lwt.return_true
        | None, None -> raise Disconnected_client

    let close cli =
        match cli.c with
        | Some (_, ic, oc) ->
            Lwt.catch
                (fun () -> Lwt_io.close ic)
                (fun _ -> Lwt.return_unit) >>= fun _ ->
            Lwt.catch
                (fun () -> Lwt_io.close oc)
                (fun _ -> Lwt.return_unit) >>= fun _ ->
            cli.c <- None;
            Lwt.return_unit
        | None -> Lwt.return_unit

    let rec read_all ic =
        Lwt_io.read ~count:(Lwt_io.buffered ic) ic >>= fun s ->
            if Lwt_io.buffered ic > 0 then
                read_all ic >|= fun s' ->
                s ^ s'
            else Lwt.return s

    let rec recv cli =
        match cli.c with
        | Some (_, ic, oc) ->
            read_all ic >>= fun s ->
            Lwt.return (Redis_protocol.Redis.Resp.decode_exn s)
        | _ -> raise Disconnected_client

    let send cli data =
        match cli.c with
        | Some (_, ic, oc) ->
            Lwt_io.write oc (Redis_protocol.Redis.Resp.encode_exn data)
            >>= fun _ -> Lwt_io.flush oc
        | _ -> raise Disconnected_client

    let run_string cli command args =
        connect cli >>= fun _ ->
        send cli (Redis_protocol.Redis.Redis_command.build ~command args)
        >>= fun _ -> recv cli

    let run cli command args =
        connect cli >>= fun _ ->
        send cli (Redis_protocol.Redis.Redis_command.build ~command (List.map Redis_protocol.Redis.Conv.string args))
        >>= fun _ -> recv cli

    let parallel clients command args =
        Lwt_list.map_p (fun cli ->
            run cli command args) clients
        >|= fun l ->
            let l = List.fold_left (fun acc -> function
                | Redis_protocol.Redis.Array (Some arr) ->
                    acc @ Array.to_list arr
                | Redis_protocol.Redis.Error _ -> acc
                | a -> acc @ [a]) [] l in
            Redis_protocol.Redis.Array (Some (Array.of_list l))

end
