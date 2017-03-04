open Redis_tools

let sql_of_redis = function
    | Redis.Bulk_string None | Redis.Array None -> Ezsqlite.Null
    | Redis.Simple_string s -> Ezsqlite.Text s
    | Redis.Error s -> Ezsqlite.Text ("ERROR: " ^ s)
    | Redis.Integer s -> Ezsqlite.Int (Int64.of_string s)
    | Redis.Bulk_string (Some s) -> Ezsqlite.Blob s
    | Redis.Array _ -> failwith "Invalid conversion"

module Archive = struct
    type t = {
        filename: string;
        client: Client.t;
        db: Ezsqlite.t;
    }

    let init dbfile (host, port) = {
        filename = dbfile;
        client = Client.init host port;
        db = Ezsqlite.load dbfile;
    }

end
