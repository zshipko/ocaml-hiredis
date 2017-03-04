exception Invalid_command
exception Disconnected_client

module Redis = Redis_protocol.Redis

(** Split a command into a tuple containing (name, arguments) *)
val split_command : Redis.t -> string * Redis.t array

module Client : sig

    (** Clients are used for both incoming and outgoing connections *)
    type t = {
        mutable authenticated : bool;
        tls_config : Conduit_lwt_unix.client_tls_config option;
        mutable sock :
            (Conduit_lwt_unix.flow *
             Conduit_lwt_unix.ic *
             Conduit_lwt_unix.oc) option;
    }

    (** Create a new t, host and port *)
    val init : ?authenticated:bool -> string -> int -> t

    (** Connect a t *)
    val connect :
        ?unix:string ->
        ?tls:bool ->
        ?ctx:Conduit_lwt_unix.ctx ->
        t ->
        bool Lwt.t

    (** Receive data from a t *)
    val recv : t -> Redis.t Lwt.t

    (** Send data to a t *)
    val send : t -> Redis.t -> unit Lwt.t

    (** Run a Redis command and return result *)
    val run : t -> string -> string list -> Redis.t Lwt.t

    (** Close a t's connection *)
    val close : t -> unit Lwt.t

end
