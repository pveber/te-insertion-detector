(* A small script to format the fasta file provided by Marie
   containing all known drosophila TE. The file was fetched from

   http://www.girinst.org/protected/repbase_extract.php?division=Drosophila&customdivision=&rank=&type=&autonomous=1&nonautonomous=1&simple=&format=FASTA&sa=Download

   Sequence names should be extracted from the fasta comment line *)

open Core.Std
open CFStream
open Biocaml_ez.Std


let main in_fa out_fa () =
  Fasta.with_file in_fa ~f:(fun _ items ->
      Out_channel.with_file out_fa ~f:(fun oc ->
          Stream.iter items ~f:(fun it ->
              let description =
                String.split ~on:' ' it.Fasta.description
                |> List.hd_exn
              in
              fprintf oc ">%s\n%s\n" description it.Fasta.sequence
            )
        )
    )

let command =
  let spec =
    let open Command.Spec in
    empty
    +> flag "--in" (required file)      ~doc:"PATH input fasta"
    +> flag "--out" (required file)    ~doc:"PATH output fasta"
  in
  Command.basic
    ~summary:"A small script to format a fasta file containing all known drosophila TE"
    spec
    main
