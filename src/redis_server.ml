open Redis_protocol.Redis
open Lwt.Infix
open Conduit_lwt_unix

exception Invalid_command
exception Disconnected_client

let split_command = function
    | Array (Some arr) when Array.length arr > 0 ->
        let name = Astring.String.Ascii.lowercase (Conv.string arr.(0)) in
        name, Array.sub arr 1 (Array.length arr - 1)
    | _ -> raise Invalid_command

let get_server_mode port host =
    match port with
    | Some p -> `TCP (`Port p)
    | None -> `Unix_domain_socket (`File host)


module Server = struct
    type t = {
        s_ctx : ctx;
        s_mode : server;
    }

    let create ?ssl ?port host =
        let mode = get_server_mode port host in
        init ?tls_server_key:ssl ~src:host () >|= fun ctx ->
        {
            s_ctx = ctx;
            s_mode = mode;
        }

    let run ?backlog ?timeout ?stop ?on_exn srv fn =
        serve ?backlog ?timeout ?stop ?on_exn ~ctx:srv.s_ctx ~mode:srv.s_mode (fun flow ic oc ->
            let client = Redis_client.Client.{
                c_ctx = srv.s_ctx;
                c_mode = None;
                c_conn = Some (flow, ic, oc);
            } in
            Lwt.catch
                (fun () -> fn client)
                (fun _ -> Lwt.return_unit) >>= fun () ->
            Redis_client.Client.close client)
end
