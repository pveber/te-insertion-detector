open Ocamlbuild_plugin

let ocamlify = A"ocamlify";;

let () = dispatch (function
    | After_options ->
      rule "ocamlify: %.mlify -> %.mlify.depends"
        ~prod:"%.mlify.depends"
        ~dep:"%.mlify"
        begin
          fun env _ ->
            Cmd(S[ocamlify;
                  T(tags_of_pathname (env "%.mlify")++"ocamlify"++"depends");
                  A"--depends";
                  A"--var-string";  A"contents"; P(env "%.mlify");
                  A"--output"; P(env "%.mlify.depends")])
        end ;

      rule "ocamlify: %.mlify & %.mlify.depends -> %.ml"
        ~prod:"%.ml"
        ~deps:["%.mlify"; "%.mlify.depends"]
        begin
          fun env build ->
            Cmd(S[ocamlify; A"--var-string"; A"contents"; P(env "%.mlify") ; A"--output"; P(env "%.ml"); ])
        end
    | _ -> ()
  )
