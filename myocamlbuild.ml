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

let lib ?findlib_deps ?internal_deps ?build_if ?ml_files lib_name
  : Project.item
  =
  Project.lib (sprintf "%s_%s" project_name lib_name)
    ~annot ~bin_annot ~g ~short_paths ~thread
    ~pkg:(sprintf "%s.%s" project_name lib_name)
    ~dir:(sprintf "lib/%s" lib_name)
    ~style:(`Pack (sprintf "%s_%s" project_name lib_name))
    ?findlib_deps
    ?internal_deps
    ?ml_files

let undash = String.map (function '-' -> '_' | c -> c)

let app ?internal_deps name : Project.item =
  Project.app name
    ~annot ~bin_annot ~g ~short_paths ~thread
    ~file:(sprintf "app/%s.ml" (undash name))
    ?internal_deps

let te_insertion_detector_lib = lib "te_insertion_detector" ~findlib_deps:[
    "bistro.utils";
    "bistro.bioinfo";
  ]

let te_insertion_detector_app =
  app "te-insertion-detector" ~internal_deps:[te_insertion_detector_lib]

let items = [ te_insertion_detector_app ; te_insertion_detector_lib ]


let () =
  let open Solvuu_build.Std.Project in

  (* Compute graph to check for cycles and other errors. *)
  ignore (Graph.of_list items);

  let libs = filter_libs items in
  let apps = filter_apps items in

  Ocamlbuild_plugin.dispatch @@ function
  | Ocamlbuild_plugin.After_rules -> (
      Ocamlbuild_plugin.clear_rules();

      Tools.m4_rule ()
        ~_D:[
          "GIT_COMMIT", Some (match Tools.git_last_commit() with
            | None -> "None"
            | Some x -> sprintf "Some \"%s\"" x
          );
          "VERSION", Some version;
        ];

      List.iter libs ~f:build_lib;
      List.iter apps ~f:build_app;

      build_static_file ".merlin" (merlin_file items);
      build_static_file ".ocamlinit"
        (ocamlinit_file items ~postfix:["open Biocaml_unix.Std"]);
      build_static_file "project.mk"
        (makefile items ~project_name);
      Findlib.build_meta_file (meta_file ~version libs);
      build_static_file (sprintf "%s.install" project_name)
        (install_file items);
    )
  | _ -> ()

