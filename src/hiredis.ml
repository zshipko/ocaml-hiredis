type value =
    | Nil
    | Error of string
    | Integer of int64
    | String of string
    | Array of value array
    | Status of string

module Value = struct
    type t = value

    let nil = Nil
    let error s = Error s
    let int64 i = Integer i
    let int i = Integer (Int64.of_int i)
    let string s = String s
    let array a = Array (Array.map string a)
end

include Value

module C = struct
    type context
    external redis_context_of_fd : Unix.file_descr -> context = "redis_context_of_fd"
    external redis_context_of_fd_i : int -> context = "redis_context_of_fd"
    external redis_context_free_keep_fd : context -> unit = "redis_context_free_keep_fd"
    external redis_context_connect : string -> int -> bool -> context = "redis_context_connect"
    external redis_context_connect_unix : string -> bool -> context = "redis_context_connect_unix"
    external redis_context_reconnect : context -> int = "redis_context_reconnect"
    external redis_context_set_timeout : context -> int -> int -> int = "redis_context_set_timeout"
    external redis_context_enable_keepalive : context -> int = "redis_context_enable_keepalive"
    external redis_context_command : context -> string array -> t = "redis_context_command"
    external redis_context_append_command : context -> string array -> int = "redis_context_append_command"
    external redis_context_append_formatted : context -> string -> int = "redis_context_append_formatted"
    external redis_context_free : context -> unit = "redis_context_free"
    external redis_context_get_reply : context -> t = "redis_context_get_reply"

    external redis_format_command : string array -> string = "redis_format_command"

    type reader
    external redis_reader_create : unit -> reader = "redis_reader_create"
    external redis_reader_free : reader -> unit = "redis_reader_free"
    external redis_reader_feed : reader -> string -> int = "redis_reader_feed"
    external redis_reader_get_reply : reader -> t = "redis_reader_get_reply"
end

type status =
    | OK
    | ERR

let status_of_int = function
    | -1 -> ERR
    | _ -> OK

let int_of_status = function
    | OK -> 0
    | ERR -> (-1)

let command arr =
    C.redis_format_command arr

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
        c_borrowed : bool;
    }

    let set_timeout ctx s us =
        status_of_int (C.redis_context_set_timeout ctx.c_handle s us)

    let connect ?nonblock:(nonblock=false) ?fd ?port host =
        let ctx = match fd with
        | Some fd' -> {
            c_handle = C.redis_context_of_fd fd';
            c_borrowed = true;
        }
        | None ->
            {
                c_handle = begin match port with
                    | Some port' -> C.redis_context_connect host port' nonblock
                    | None -> C.redis_context_connect_unix host nonblock
                    end;
                c_borrowed = false;
            }
        in Gc.finalise (fun x ->
            if x.c_borrowed then C.redis_context_free_keep_fd x.c_handle
            else C.redis_context_free x.c_handle) ctx; ctx

    let append_command ctx arr =
        status_of_int (C.redis_context_append_command ctx.c_handle arr)

    let append_formatted ctx s =
        status_of_int (C.redis_context_append_formatted ctx.c_handle s)

    let get_reply ctx =
        C.redis_context_get_reply ctx.c_handle

    let run ctx arr =
        C.redis_context_command ctx.c_handle arr
end

module Pool = struct
    type t = Client.t Lwt_pool.t

    let create ?port host n =
        Lwt_pool.create n (fun () ->
            Lwt.return (Client.connect ?port host))

    let use pool fn =
        Lwt_pool.use pool fn
end
