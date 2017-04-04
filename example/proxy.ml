open Hiredis
open Lwt.Infix

let pool = Pool.create ~port:1235 "127.0.0.1" 64

let handler args =
    Pool.use pool (fun cli ->
        Lwt.return_some (Client.run_v cli args))

let on_exn exc =
    print_endline (Printexc.to_string exc)

let main =
Server.set_auth (Some "abc123");
Server.create (`TCP (`Port 1234)) >>= fun srv ->
Server.run ~on_exn srv handler

let () = Lwt_main.run main



