type t =
    | Nil
    | Error of string
    | Integer of int64
    | String of string
    | Array of t array
    | Status of string

type context
external redis_context_errstr : context -> string option = "redis_context_errstr"
external redis_context_to_fd : context -> Unix.file_descr = "redis_context_to_fd"
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
external redis_context_flush_buffer : context -> int = "redis_context_flush_buffer"
external redis_context_read_buffer : context -> int = "redis_context_read_buffer"
external redis_context_get_reply : context -> t = "redis_context_get_reply"

external redis_format_command : string array -> string = "redis_format_command"

type reader
external redis_reader_create : unit -> reader = "redis_reader_create"
external redis_reader_free : reader -> unit = "redis_reader_free"
external redis_reader_feed : reader -> string -> int = "redis_reader_feed"
external redis_reader_get_reply : reader -> t = "redis_reader_get_reply"

