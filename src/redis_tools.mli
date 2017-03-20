exception Invalid_command
exception Disconnected_client

module Redis = Redis_protocol.Redis

module Client : sig

    (** Clients are used for both incoming and outgoing connections *)
    type t = {
        c_ctx: Conduit_lwt_unix.ctx;
        c_mode: Conduit_lwt_unix.client option;
        mutable c_conn :
             (Conduit_lwt_unix.flow *
              Conduit_lwt_unix.ic *
              Conduit_lwt_unix.oc) option;
    }

    val default_addr : Conduit_lwt_unix.client

    (** Create a new t, given host and port *)
    val create :
        ?ctx:Conduit_lwt_unix.ctx ->
        ?port:int ->
        string -> t

    (** Connect a t *)
    val connect : t -> unit Lwt.t

    (** Receive data from a t *)
    val recv : t -> Redis.t Lwt.t

    (** Send data to a t *)
    val send : t -> Redis.t -> unit Lwt.t

    (** Run a Redis command and return result *)
    val run : t -> string -> Redis.t list -> Redis.t Lwt.t
    val run_string : t -> string -> string list -> Redis.t Lwt.t
    val parallel : t list -> string -> Redis.t list -> Redis.t Lwt.t

    (** Close a t's connection *)
    val close : t -> unit Lwt.t

end

module Server : sig
    type t = {
        s_ctx : Conduit_lwt_unix.ctx;
        s_mode : Conduit_lwt_unix.server;
    }

    val create : ?ssl:Conduit_lwt_unix.tls_server_key -> ?port:int -> string -> t Lwt.t

    val run :
        ?backlog:int ->
        ?timeout:int ->
        ?stop:(unit Lwt.t) ->
        ?on_exn:(exn -> unit) ->
        t ->
        (Client.t -> unit Lwt.t) ->
        unit Lwt.t
end

