exception Invalid_command
exception Disconnected_client

module Redis = Redis_protocol.Redis

module Client : sig

    (** Clients are used for both incoming and outgoing connections *)
    type t = {
        config: Conduit_lwt_unix.client option;
        ctx: Conduit_lwt_unix.ctx;
        mutable c :
             (Conduit_lwt_unix.flow *
              Conduit_lwt_unix.ic *
              Conduit_lwt_unix.oc) option;
    }

    (** Create a new t, host and port *)
    val init :
        ?ctx:Conduit_lwt_unix.ctx ->
        ?config:Conduit_lwt_unix.client ->
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
    val parallel : t list -> string -> Redis.t list -> Redis.t Lwt.t

    (** Close a t's connection *)
    val close : t -> unit Lwt.t

end

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

module Server (X : EVAL) : SERVER with type db = X.db
