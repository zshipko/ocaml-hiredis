open Hiredis_value
open Hiredis_client

open Lwt.Infix

module Server = struct
    open Unix

    type t = {
        s_ctx : Conduit_lwt_unix.ctx;
        s_mode : Conduit_lwt_unix.server;
        s_tls_config : Conduit_lwt_unix.tls_server_key option;
    }

    let create ?host:(host="127.0.0.1") ?tls_config mode =
        Conduit_lwt_unix.init ~src:host ?tls_server_key:tls_config () >|= fun ctx ->
        {
            s_ctx = ctx;
            s_mode = mode;
            s_tls_config = tls_config;
        }

    let buffer_size = 2048

    let rec read ic =
        Lwt_io.read ~count:buffer_size ic >>= fun s ->
        if String.length s = buffer_size then
            read ic >|= fun s' -> s ^ s'
        else Lwt.return s

    let rec aux callback ic oc r =
        read ic >>= fun s ->
        let () = if String.length s > 0
                 then ignore (Reader.feed r s) in
            match Reader.get_reply r with
            | None -> Lwt.return_unit
            | Some (Array a) ->
                (callback a >>= function
                | Some res ->
                    Lwt_io.write oc (Reader.encode_string res) >>= fun _ ->
                    aux callback ic oc r
                | None ->
                    Lwt.return_unit)
            | _ ->
                Lwt_io.write oc "-ERR INVALID COMMAND" >>= fun _ ->
                Lwt.return_unit

    let rec handle callback flow ic oc =
        let r = Reader.create () in
        Lwt.catch (fun () ->
            aux callback ic oc r)
        (fun _ -> Lwt_unix.yield ())

    let rec run ?backlog ?timeout ?stop ?on_exn srv callback =
        Conduit_lwt_unix.serve ?backlog ?timeout ?stop ?on_exn
            ~ctx:srv.s_ctx ~mode:srv.s_mode (handle callback)

end
