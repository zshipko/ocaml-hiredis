(** Hiredis is an OCaml wrapper around the [hiredis] C library *)

(** The [value] type is used to encode values to communicate with Redis *)
type value =
    | Nil
    | Error of string
    | Integer of int64
    | String of string
    | Array of value array
    | Status of string

module Value : sig
    type t = value

    (** Get a nil value *)
    val nil : t

    (** Convert an OCaml string to a Hiredis string *)
    val string : string -> t

    (** Convert an int64 to Hiredis number *)
    val int64 : int64 -> t

    (** Convert an int to Hiredis number *)
    val int : int -> t

    (** Convert an OCaml array to Hiredis array *)
    val array : t array -> t

    (** Create an error value *)
    val error : string -> t

    (** Create a status or "simple string" value *)
    val status : string -> t

    exception Invalid_value

    val is_nil : t -> bool
    val is_error : t -> bool

    (** [to_string v] converts [v] to a string, otherwise [Invalid_value] is raised *)
    val to_string : t -> string

    (** [to_int64 v] converts [v] to an int64, otherwise [Invalid_value] is raised *)
    val to_int64 : t -> int64

    (** [to_int v] converts [v] to an int, otherwise [Invalid_value] is raised *)
    val to_int : t -> int

    (** [to_float v] converts [v] to a float, otherwise [Invalid_value] is raised *)
    val to_float : t -> float

    (** [to_array v] converts [v] to an array of values if [v] is an array value,
     *  otherwise [Invalid_value] is raised *)
    val to_array : t -> t array

    (** [to_list v] converts [v] to a list of values if [v] is an array value,
     *  otherwise [Invalid_value] is raised *)
    val to_list : t -> t list

    (** [to_hashtbl v] converts [v] to a Hashtbl.t if [v] is an array value and
     *  the array can be interpreted as a hash table, otherwise [Invalid_value] is
     *  raised*)
    val to_hashtbl : t -> (string, t) Hashtbl.t
end

type status =
    | OK
    | ERR of string option

(** Create a command from an array of strings *)
val command : string array -> string option

(** Create a command from an array of values *)
val command_v : value array -> string option

(** Encode value to string *)
val encode_string : value -> string

(** Decode value from string *)
val decode_string : string -> value option

(** Readers are used to decode Redis values from buffered input *)
module Reader : sig
    type t

    (** Create a new, empty reader *)
    val create : unit -> t

    (** Feed the reader more input data *)
    val feed : t -> string -> status

    (** Attempt to read a reply from the accumulated input data *)
    val get_reply : t -> Value.t option
end

module Client : sig
    type t

    (** Returns the error string associated with a hiredis context *)
    val error_string : t -> string option

    (** Create a new client connection *)
    val connect :
        ?scripts:(string, string) Hashtbl.t ->
        ?auth:string ->
        ?nonblock:bool ->
        ?port:int ->
        string -> t

    (** Create a new client from an existing file descr *)
    val of_fd :
        ?scripts:(string, string) Hashtbl.t ->
        ?close_fd:bool ->
        Unix.file_descr -> t

    (** Get the underlying file descr *)
    val to_fd : t -> Unix.file_descr

    (** Set a client's timeout *)
    val set_timeout : t -> int -> int -> status

    (** Enable keepalive on the client *)
    val enable_keepalive : t -> status

    (** Queue command to be executed *)
    val append_command : t -> string array -> status

    (** Similar to [append_command] but using a command made of Hiredis values *)
    val append_command_v : t -> value array -> status

    (** Append a pre-formatted command string to be executed *)
    val append_formatted : t -> string -> status
    val append_value : t -> value -> status

    (* Write queued commands *)
    val flush_buffer : t -> status
    val read_buffer : t -> status

    (** [get_reply client] executes the queued commands and returns the result *)
    val get_reply : t -> value option

    (** Execute a command formatted as an array of strings and return the reply immediately *)
    val run : t -> string array -> value

    (** Execute a command formatted as an array of values and return the reply immediately *)
    val run_v : t -> value array -> value

    (** [load_script name script] will load a lua script onto the server and make it available from the existing
     *  client as [name]*)
    val load_script : t -> string -> string -> unit

    (** [call_script client name nkeys args] calls a script by name with the given number of keys and arguments *)
    val call_script : t -> string -> int -> string list -> value

    (** Similar to [call_script] but with a list of values for arguments *)
    val call_script_v : t -> string -> int -> value list -> value
end

(** A pool is used to access one server using many clients *)
module Pool : sig
    type t = Client.t Lwt_pool.t

    (** [create ~port host n] creates a new pool of [n] clients listening on [host:port] *)
    val create : ?port:int -> string -> int -> t

    (** [use pool] returns a new client from the pool *)
    val use : t -> (Client.t -> 'a Lwt.t) -> 'a Lwt.t
end

module Shell : sig
    module Server : sig
        type t

        (** [start ~temp_dir ~config port] starts a new [redis-server] instance on [port] with the given [config].
         *  The config file will be saved to [temp_dir] *)
        val start : ?temp_dir:string -> ?config:(string * string list) list -> int -> t

        (** Stop an active server *)
        val stop : t -> unit
    end

    module Client : sig
        (** Open the interactive client on the given host and port *)
        val interactive : ?host:string -> int -> unit
    end
end
