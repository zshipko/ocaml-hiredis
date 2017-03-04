exception Invalid_command
exception Disconnected_client

module Redis = Redis_protocol.Redis

(** Split a command into a tuple containing (name, arguments) *)
val split_command : Redis.t -> string * Redis.t array

module Client : sig

    (** Clients are used for both incoming and outgoing connections *)
    type t = {
        config: Conduit_lwt_unix.client;
        ctx: Conduit_lwt_unix.ctx;
        mutable sock :
            (Conduit_lwt_unix.flow *
             Conduit_lwt_unix.ic *
             Conduit_lwt_unix.oc) option;
    }

    (** Create a new t, host and port *)
    val init :
        ?tls_config:([`Hostname of string] * [`IP of Ipaddr.t] * [`Port of int]) ->
        ?unix:string ->
        ?host:string ->
        ?port:int ->
        ?ctx:Conduit_lwt_unix.ctx ->
        unit -> t


    (** Connect a t *)
    val connect : t -> bool Lwt.t

    (** Receive data from a t *)
    val recv : t -> Redis.t Lwt.t

    (** Send data to a t *)
    val send : t -> Redis.t -> unit Lwt.t

    (** Run a Redis command and return result *)
    val run : t -> string -> Redis.t list -> Redis.t Lwt.t
    val run_string : t -> string -> string list -> Redis.t Lwt.t

    (** Close a t's connection *)
    val close : t -> unit Lwt.t

end
