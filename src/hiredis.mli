type value =
    | Nil
    | Error of string
    | Integer of int64
    | String of string
    | Array of value array
    | Status of string

module Value : sig
    type t = value
    val nil : t
    val string : string -> t
    val int64 : int64 -> t
    val int : int -> t
    val array : t array -> t
    val error : string -> t
    val status : string -> t

    exception Invalid_value

    val is_nil : t -> bool
    val is_error : t -> bool
    val to_string : t -> string
    val to_int64 : t -> int64
    val to_int : t -> int
    val to_float : t -> float
    val to_array : t -> t array
    val to_list : t -> t list
    val to_hashtbl : t -> (string, t) Hashtbl.t
end

type status =
    | OK
    | ERR of string option

val command : string array -> string option
val command_v : value array -> string option

module Reader : sig
    type t
    val create : unit -> t
    val feed : t -> string -> status
    val get_reply : t -> Value.t option

    val encode_string : value -> string
    val decode_string : string -> value option
end

module Client : sig
    type t

    (** Returns the error string associated with a hiredis context *)
    val error_string : t -> string option

    (** Create a new context *)
    val connect : ?auth:string -> ?nonblock:bool -> ?port:int -> string -> t

    (* Convert between clients and file_descrs *)
    val of_fd : ?close_fd:bool -> Unix.file_descr -> t
    val to_fd : t -> Unix.file_descr
    val set_timeout : t -> int -> int -> status
    val enable_keepalive : t -> status

    (* Queue commands to be executed *)
    val append_command : t -> string array -> status
    val append_command_v : t -> value array -> status
    val append_formatted : t -> string -> status
    val append_value : t -> value -> status

    (* Write queued commands *)
    val flush_buffer : t -> status
    val read_buffer : t -> status

    (* Execute queued commands *)
    val get_reply : t -> value option
    val run : t -> string array -> value
    val run_v : t -> value array -> value
end

module Pool : sig
    type t = Client.t Lwt_pool.t
    val create : ?port:int -> string -> int -> t
    val use : t -> (Client.t -> 'a Lwt.t) -> 'a Lwt.t
end

module Shell : sig
    module Server : sig
        type t
        val start : ?temp_dir:string -> ?config:(string * string list) list -> int -> t
        val stop : t -> unit
    end

    module Client : sig
        val interactive : ?host:string -> int -> unit
    end
end

module Server : sig
    type t
    val create :
        ?host:string ->
        ?tls_config:Conduit_lwt_unix.tls_server_key ->
        Conduit_lwt_unix.server ->
        t Lwt.t

    val run :
        ?backlog:int ->
        ?timeout:int ->
        ?stop:unit Lwt.t ->
        ?on_exn:(exn -> unit) ->
        t ->
        (Value.t array -> Value.t option Lwt.t) ->
        unit Lwt.t
end
