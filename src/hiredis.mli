module Value : sig
    type t =
        | Nil
        | Error of string
        | Integer of int64
        | String of string
        | Array of t array
        | Status of string

    val nil : t
    val string : string -> t
    val int64 : int64 -> t
    val int : int -> t
    val array : string array -> t
    val error : string -> t
end

type status =
    | OK
    | ERR

val status_of_int : int -> status
val int_of_status : status -> int

val command : string array -> string

module Reader : sig
    type t

    val create : unit -> t
    val feed : t -> string -> status
    val get_reply : t -> Value.t
end

module Client : sig
    type t

    val connect : ?nonblock:bool -> ?fd:Unix.file_descr -> ?port:int -> string -> t
    val set_timeout : t -> int -> int -> status
    val append_command : t -> string array -> status
    val append_formatted : t -> string -> status
    val get_reply : t -> Value.t
    val run : t -> string array -> Value.t
end

