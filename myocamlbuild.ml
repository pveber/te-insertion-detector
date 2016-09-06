open Printf
open Solvuu_build.Std
open Solvuu_build.Util

let project_name = "te-insertion-detector"
let version = "dev"

let annot = ()
let bin_annot = ()
let g = ()
let short_paths = ()
let thread = ()

let undash = String.map (function '-' -> '_' | c -> c)

let lib =
  let name = "te_insertion_detector" in
  Project.lib name
    ~annot ~bin_annot ~g ~short_paths ~thread
    ~pkg:name
    ~dir:"lib"
    ~style:(`Pack name)
    ~findlib_deps:[
      "biocaml.ez" ;
      "bistro.utils";
      "bistro.bioinfo";
      "ppx_deriving.show" ;
    ]

let app =
  let name = "te-insertion-detector" in
  Project.app name
    ~annot ~bin_annot ~g ~short_paths ~thread
    ~file:(sprintf "app/%s_app.ml" (undash name))
    ~internal_deps:[lib]

let items = [ app ; lib ]


let () =
  let open Solvuu_build.Std.Project in

  (* Compute graph to check for cycles and other errors. *)
  ignore (Graph.of_list items);

  let libs = filter_libs items in
  let apps = filter_apps items in

  Ocamlbuild_plugin.dispatch @@ function
  | Ocamlbuild_plugin.After_rules -> (
      Ocamlbuild_plugin.clear_rules();

      List.iter libs ~f:build_lib;
      List.iter apps ~f:build_app;

      build_static_file ".merlin" (merlin_file items);
      build_static_file ".ocamlinit" (ocamlinit_file items);
      build_static_file "project.mk" (makefile items ~project_name);
      Findlib.build_meta_file (meta_file ~version libs);
      build_static_file (sprintf "%s.install" project_name)
        (install_file items);
    )
  | _ -> ()

