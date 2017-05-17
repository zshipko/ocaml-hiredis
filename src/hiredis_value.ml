type value = C.t =
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
    let array a = Array a
    let status s = Status s

    exception Invalid_value

    let is_nil = function
        | Nil -> true
        | _ -> false

    let is_error = function
        | Error _ -> true
        | _ -> false

    let to_string = function
        | Nil -> ""
        | String s -> s
        | Error s -> s
        | Integer i -> Int64.to_string i
        | Status s -> s
        | _ -> raise Invalid_value

    let to_int64 = function
        | Integer i -> i
        | x ->
            begin try
                to_string x |> Int64.of_string
            with _ -> raise Invalid_value
            end

    let to_int x = to_int64 x |> Int64.to_int
    let to_float x = to_int64 x |> Int64.to_float

    let to_array = function
        | Array a -> a
        | x -> [| x |]

    let to_list = function
        | Array a -> Array.to_list a
        | x -> [x]

    let to_hashtbl a =
        let a = to_array a in
        let len = Array.length a in
        let key = ref None in
        let _ = if len mod 2 <> 0 then raise Invalid_value in
        let dst = Hashtbl.create (len/2) in
        let _ = Array.iteri (fun n item ->
            if n mod 2 = 0 then
                key := Some (to_string item)
            else match !key with
            | Some k ->
                Hashtbl.replace dst k item;
                key := None
            | None -> ()) a
        in dst
end

let command arr =
    C.redis_format_command arr

let command_v arr =
    command (Array.map Value.to_string arr)
