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
        config : Conduit_lwt_unix.server;
        db : db;
        ctx : Conduit_lwt_unix.ctx Lwt.t;
    }

    val init : ?host:string -> ?config:Conduit_lwt_unix.server -> db -> t

    val serve :
        ?timeout:int ->
        ?stop:(unit Conduit_lwt_unix.io) ->
        ?on_exn:(exn -> unit) ->
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
        config : Conduit_lwt_unix.server;
        db : E.db;
        ctx : Conduit_lwt_unix.ctx Lwt.t;
    }

    let init ?host:(host="127.0.0.1") ?config:(config=`TCP (`Port 6379)) db = {
        config = config;
        db = db;
        ctx = Conduit_lwt_unix.init ~src:host ();
    }

    let execute = E.execute
    let auth = E.auth

    let wrap_handler (srv : t) (handler : Redis_client.Client.t -> unit Lwt.t) : (Conduit_lwt_unix.flow -> Conduit_lwt_unix.ic -> Conduit_lwt_unix.oc -> unit Lwt.t) =
        let aux _flow _ic _oc =
            srv.ctx >>= fun ctx ->

            (* Wrap the connection in a new client *)
            let client = Redis_client.Client.({
                config = None;
                ctx = ctx;
                c = Some (_flow, _ic, _oc);
            }) in

            (* Make sure client is authenticated using the AUTH command *)
            let rec check_auth () =
                match auth with
                | Some auth_fn ->
                    Lwt.catch (fun () ->
                        Redis_client.Client.recv client >>= function
                        | Array (Some arr) when
                            Array.length arr > 1 &&
                            Astring.String.Ascii.lowercase (Conv.string arr.(0)) =  "auth" ->
                            if auth_fn srv.db (Array.sub arr 1 (Array.length arr - 1)) then Redis_client.Client.send client ok >>= fun _ -> handler client
                            else Redis_client.Client.send client (Error "NOAUTH Authentication required") >>= fun _ -> check_auth()
                        | _ ->
                            Redis_client.Client.send client (Error "NOAUTH Authentication required") >>= fun _ -> check_auth())
                    (fun _ -> Redis_client.Client.close client)
                | _ -> handler client
            in check_auth ()
        in aux

    let serve ?timeout ?stop ?on_exn srv =
        srv.ctx >>= fun ctx ->
        Conduit_lwt_unix.serve
            ?timeout ?stop ?on_exn ~ctx ~mode:srv.config
            (wrap_handler srv (fun cli ->
                let rec aux cli =
                    Redis_client.Client.recv cli >>= fun x ->
                    execute srv.db cli (split_command x) >>= function
                    | Some y -> Redis_client.Client.send cli y >>= fun _ -> Gc.full_major (); aux cli
                    | None -> Lwt.return_unit
                in aux cli))

end
