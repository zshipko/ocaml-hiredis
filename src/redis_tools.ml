module Redis = Redis_protocol.Redis
module Client = Redis_client.Client


exception Invalid_command
exception Disconnected_client

open Redis

let split_command = function
    | Array (Some arr) when Array.length arr > 0 ->
        let name = Astring.String.Ascii.lowercase (Conv.string arr.(0)) in
        name, Array.sub arr 1 (Array.length arr - 1)
    | _ -> raise Invalid_command
