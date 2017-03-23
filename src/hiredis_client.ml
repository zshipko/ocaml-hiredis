open Hiredis_value

type status =
    | OK
    | ERR of string option

let status_of_int ?msg:(msg=(fun x -> None)) = function
    | -1 -> ERR (msg ())
    | _ -> OK

let int_of_status = function
    | OK -> 0
    | ERR _ -> (-1)

module Reader = struct
    type t = {
        r_handle : C.reader;
    }

    let create () =
        let r = {r_handle = C.redis_reader_create ()} in
        Gc.finalise (fun x -> C.redis_reader_free x.r_handle) r; r

    let feed r s =
        status_of_int (C.redis_reader_feed r.r_handle s)

    let get_reply r =
        C.redis_reader_get_reply r.r_handle
end

module Client = struct
    type t = {
        c_handle : C.context;
        mutable c_freed : bool;
    }

    let error_string ctx =
        C.redis_context_errstr ctx.c_handle

    let close ?close_fd:(close_fd=true) ctx =
        let res = if ctx.c_freed then ()
        else if close_fd then C.redis_context_free ctx.c_handle
        else C.redis_context_free_keep_fd ctx.c_handle
        in ctx.c_freed <- true; res

    let to_fd ctx =
        C.redis_context_to_fd ctx.c_handle

    let of_fd ?close_fd:(close_fd=true) fd =
        let ctx = {
            c_handle = C.redis_context_of_fd fd;
            c_freed = false;
        } in
        Gc.finalise (fun x ->
            close ~close_fd x) ctx; ctx

    let connect ?nonblock:(nonblock=false) ?port host =
        let ctx = {
                c_handle = begin match port with
                    | Some port' -> C.redis_context_connect host port' nonblock
                    | None -> C.redis_context_connect_unix host nonblock
                    end;
                c_freed = false;
            }
        in Gc.finalise (fun x ->
            close x) ctx; ctx

    let reconnect ctx =
        C.redis_context_reconnect ctx.c_handle

    let set_timeout ctx s us =
        status_of_int ~msg:(fun () ->
            error_string ctx) (C.redis_context_set_timeout ctx.c_handle s us)

    let enable_keepalive ctx =
        status_of_int ~msg:(fun () ->
            error_string ctx) (C.redis_context_enable_keepalive ctx.c_handle)

    let append_command ctx arr =
        status_of_int ~msg:(fun () ->
            error_string ctx) (C.redis_context_append_command ctx.c_handle arr)

    let append_command_v ctx arr =
        status_of_int ~msg:(fun () ->
            error_string ctx) (C.redis_context_append_command ctx.c_handle (Array.map Value.to_string arr))

    let append_formatted ctx s =
        status_of_int ~msg:(fun () ->
            error_string ctx) (C.redis_context_append_formatted ctx.c_handle s)

    let get_reply ctx =
        C.redis_context_get_reply ctx.c_handle

    let run ctx arr =
        C.redis_context_command ctx.c_handle arr

    let run_v ctx arr =
        run ctx (Array.map Value.to_string arr)

end

module Pool = struct
    type t = Client.t Lwt_pool.t

    let create ?port host n =
        Lwt_pool.create n (fun () ->
            Lwt.return (Client.connect ?port host))

    let use pool fn =
        Lwt_pool.use pool fn
end
