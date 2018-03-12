open Ocamlbuild_plugin

let () = dispatch begin function
    | After_rules ->
    flag ["use_hiredis_stubs"] &
        S[A"-I"; A"src"];

    flag ["ocaml"; "link"; "byte"; "library"; "use_hiredis_stubs"] &
        S[A"-dllib"; A"-lhiredis_stubs"; A"-cclib"; A"-lpthread"];

    flag ["ocaml"; "link"; "native"; "library"; "use_hiredis_stubs"] &
        S[ A"-cclib"; A"-lhiredis_stubs"; A"-cclib"; A"-lpthread"];

    flag ["link"; "ocaml"; "link_hiredis_stubs"] &
        S[A"-cclib"; A"-lhiredis_stubs"; A"-cclib"; A"-lpthread"];

    | _ -> ()
end
