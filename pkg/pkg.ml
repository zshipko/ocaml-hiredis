#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"

open Topkg

let () =
    Pkg.describe "redis-tools" @@ fun c ->
        Ok [
            Pkg.mllib ~api:["Redis_tools"; "Redis_protocol"] "src/redis-tools.mllib";
        ]
