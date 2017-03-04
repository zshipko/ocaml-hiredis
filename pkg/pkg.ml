#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"

open Topkg

let () =
    Pkg.describe "redis_tools" @@ fun c ->
        Ok [
            Pkg.mllib ~api:["Redis_tools"] "src/redis_tools.mllib";
            (*Pkg.bin "archive/redis-archive" ~dst:"redis-archive";*)
        ]
