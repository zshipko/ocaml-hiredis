open Lwt.Infix
open Redis_client

module Pool = struct
    type t = Client.t Lwt_pool.t

    let validate x = match x.Client.c_conn with
    | Some _ -> Lwt.return_true
    | None -> Lwt.return_false

    let create ?ctx ?port host n =
        Lwt_pool.create n ~validate (fun () ->
            let c = Client.create ?ctx ?port host in
            Client.connect c >|= fun () -> c)

    let execute pool fn =
        Lwt_pool.use pool fn
end
