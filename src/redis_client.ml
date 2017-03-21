open Lwt.Infix
open Conduit_lwt_unix

exception Disconnected_client

let get_mode port host =
    match port with
    | Some p -> `TCP (`IP (Ipaddr.of_string_exn host), `Port p)
    | None -> `Unix_domain_socket (`File host)

module Client = struct
    type t = {
        c_ctx : ctx;
        c_mode : client option;
        mutable c_conn : (flow * ic * oc) option;
    }

    let default_addr = `TCP (`IP (Ipaddr.of_string_exn "127.0.0.1"), `Port 6379)

    let create ?ctx:(ctx=default_ctx) ?port host =
        {
            c_ctx = ctx;
            c_mode = Some (get_mode port host);
            c_conn = None;
        }

    let connect cli =
        match cli.c_conn, cli.c_mode with
        | Some _, _ -> Lwt.return_unit
        | None, Some mode ->
            connect ~ctx:cli.c_ctx mode >|= fun c ->
            cli.c_conn <- Some c
        | None, None -> failwith "invalid client"

    let close cli =
        match cli.c_conn with
        | Some (_, ic, oc) ->
            Lwt.catch
                (fun () -> Lwt_io.close ic)
                (fun _ -> Lwt.return_unit) >>= fun _ ->
            Lwt.catch
                (fun () -> Lwt_io.close oc)
                (fun _ -> Lwt.return_unit) >>= fun _ ->
            cli.c_conn <- None;
            Lwt.return_unit
        | None -> Lwt.return_unit

    let rec read_all ic =
        Lwt_io.read ~count:1024 ic >>= fun s ->
        if String.length s = 1024 then
            read_all ic >|= fun s' -> s ^ s'
        else Lwt.return s

    let rec recv cli =
        match cli.c_conn with
        | Some (_, ic, oc) ->
            read_all ic >>= fun s ->
            if s = "" then failwith "no data"
            else
            begin match Redis_protocol.Redis.Resp.decode s with
            | Some r -> Lwt.return r
            | None -> read_all ic >|= fun s' -> Redis_protocol.Redis.Resp.decode_exn (s ^ s')
            end
        | _ -> raise Disconnected_client

    let send cli data =
        match cli.c_conn with
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
