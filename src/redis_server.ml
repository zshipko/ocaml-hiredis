open Redis_protocol.Redis
open Lwt.Infix

exception Invalid_command
exception Disconnected_client

let split_command = function
    | Array (Some arr) when Array.length arr > 0 ->
        let name = Astring.String.Ascii.lowercase (Conv.string arr.(0)) in
        name, Array.sub arr 1 (Array.length arr - 1)
    | _ -> raise Invalid_command

module type EVAL = sig
    type db
    val auth : (db -> Redis_protocol.Redis.t array -> bool) option
    val execute : db -> Redis_client.Client.t -> string * Redis_protocol.Redis.t array -> Redis_protocol.Redis.t option Lwt.t
end

module type SERVER = sig
    type db

    type t = {
        port : int;
        db : db;
        tls_config : Conduit_lwt_unix.server_tls_config option;
        ctx : Conduit_lwt_unix.ctx Lwt.t;
    }

    val init :
        ?tls_config:[ `Crt_file_path of string ] *
                    [ `Key_file_path of string ] *
                    [ `No_password | `Password of bool -> string] -> db -> string -> int -> t

    val serve :
        ?timeout:int ->
        ?stop:(unit Conduit_lwt_unix.io) ->
        ?on_exn:(exn -> unit) ->
        ?unix:string ->
        t ->
        unit Lwt.t

    val execute : db ->
                  Redis_client.Client.t ->
                  string * Redis_protocol.Redis.t array ->
                  Redis_protocol.Redis.t option Lwt.t

end

module Server (E : EVAL) = struct
    type db = E.db

    type t = {
        port : int;
        db : E.db;
        tls_config : Conduit_lwt_unix.server_tls_config option;
        ctx : Conduit_lwt_unix.ctx Lwt.t;
    }

    let init ?tls_config db host port = {
        port = port;
        db = db;
        tls_config = (match tls_config with
        | Some (a, b, c) -> Some (a, b, c, `Port port)
        | None -> None);
        ctx = Conduit_lwt_unix.init ~src:host ();
    }

    let execute = E.execute
    let auth = E.auth

    let wrap_handler (srv : t) (handler : Redis_client.Client.t -> unit Lwt.t) : (Conduit_lwt_unix.flow -> Conduit_lwt_unix.ic -> Conduit_lwt_unix.oc -> unit Lwt.t) =
        let aux _flow _ic _oc =
            srv.ctx >>= fun ctx ->
            let client = Redis_client.Client.({
                config = None;
                ctx = ctx;
                sock = Some (_flow, _ic, _oc);
            }) in
            let rec check_auth () =
                match auth with
                | Some auth_fn ->
                    Lwt.catch (fun () ->
                        Redis_client.Client.recv client >>= function
                        | Array (Some arr) when
                            Array.length arr > 1 &&
                            Astring.String.Ascii.lowercase (Conv.string arr.(0)) =  "auth" ->
                            if auth_fn srv.db (Array.sub arr 1 (Array.length arr - 1)) then Redis_client.Client.send client (Simple_string "OK") >>= fun _ -> handler client
                            else Redis_client.Client.send client (Error "NOAUTH Authentication required")

                        | _ ->
                            Redis_client.Client.send client (Error "NOAUTH Authentication required") >>= fun _ -> check_auth())
                    (fun _ -> Redis_client.Client.close client)
                | _ -> handler client
            in check_auth ()
        in aux

    let _serve ?timeout ?stop ?on_exn ?unix srv handler =
        let mode = match srv.tls_config with
        | Some cfg -> `TLS cfg
        | None ->
            begin match unix with
            | Some u -> `Unix_domain_socket (`File u)
            | None -> `TCP (`Port srv.port) end in
        srv.ctx >>= fun ctx ->
            Conduit_lwt_unix.serve ~backlog: 128
                ?timeout ?stop ?on_exn ~ctx ~mode
                (wrap_handler srv handler)

    let serve ?timeout ?stop ?on_exn ?unix srv =
        let buffer = ref [] in
        let in_multi = ref false in
        _serve
        ?timeout ?stop ~on_exn:(fun _ -> ()) ?unix srv
        (fun cli ->
            let rec aux cli =
                Lwt.catch (fun () ->
                    Redis_client.Client.recv cli >>= function
                    (* Transactions allow for commands to be queued up and executed at once *)
                    | Array (Some arr) when Array.length arr >= 1 && Astring.String.Ascii.lowercase (Conv.string arr.(0)) = "multi" ->
                        in_multi := true;
                        Redis_client.Client.send cli (Simple_string "OK")

                    (* Execute all queued commands *)
                    | Array (Some arr) when Array.length arr >= 1 && Astring.String.Ascii.lowercase (Conv.string arr.(0)) = "exec" ->
                        if !in_multi then
                            let _ = in_multi := false in
                            Lwt_list.map_s (fun cmd ->
                                execute srv.db cli (split_command cmd) >|= function
                                    | Some x -> x
                                    | None -> Array None) (List.rev !buffer) >>= fun dst ->
                            let _ = buffer := [] in
                            Redis_client.Client.send cli (Array (Some (Array.of_list dst)))
                        else Redis_client.Client.send cli (Error "ERR EXEC without MULTI")

                    (* Throw away all queued commands *)
                    | Array (Some arr) when Array.length arr >= 1 && Astring.String.Ascii.lowercase (Conv.string arr.(0)) = "discard" ->
                        if !in_multi then
                            let _ = in_multi := false in
                            let _ = buffer := [] in
                            Redis_client.Client.send cli (Simple_string "OK")
                        else Redis_client.Client.send cli (Error "ERR DISCARD without MULTI")

                    (* If the command is unrelated to transaction management then just queue it or execute it depending on in_multi *)
                    | x ->
                        (Lwt.catch (fun () ->
                            if !in_multi then
                                let _ = buffer := x::!buffer in
                                Redis_client.Client.send cli (Simple_string "QUEUED")
                            else execute srv.db cli (split_command x)
                                >>= function
                                    | Some y -> Redis_client.Client.send cli y
                                    | None -> Lwt.return_unit)
                        (fun exc -> Redis_client.Client.send cli (Error (Printexc.to_string exc)))
                        >>= fun _ -> Gc.full_major (); aux cli))

                (fun exc -> Redis_client.Client.close cli)
            in aux cli)
end
