#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"

open Topkg

let () =
    Pkg.describe "hiredis" @@ fun c ->
        Ok [
            Pkg.lib "src/hiredis/fmacros.h";
            Pkg.lib "src/hiredis/sdsalloc.h";
            Pkg.lib "src/hiredis/sds.h";
            Pkg.lib "src/hiredis/sds.c";
            Pkg.lib "src/hiredis/dict.h";
            Pkg.lib "src/hiredis/dict.c";
            Pkg.lib "src/hiredis/net.h";
            Pkg.lib "src/hiredis/net.c";
            Pkg.lib "src/hiredis/async.h";
            Pkg.lib "src/hiredis/async.c";
            Pkg.lib "src/hiredis/read.h";
            Pkg.lib "src/hiredis/read.c";
            Pkg.lib "src/hiredis/hiredis.h";
            Pkg.lib "src/hiredis/win32.h";
            Pkg.clib "src/libhiredis_stubs.clib";
            Pkg.mllib ~api:["Hiredis"] "src/hiredis.mllib";
            Pkg.test ~dir:"test" "test/hiredis_test";
        ]
