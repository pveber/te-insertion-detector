(* A small script to format the fasta file provided by Marie
   containing all known drosophila TE. The file was fetched from

   http://www.girinst.org/protected/repbase_extract.php?division=Drosophila&customdivision=&rank=&type=&autonomous=1&nonautonomous=1&simple=&format=FASTA&sa=Download

   Sequence names should be extracted from the fasta comment line *)

open Core
open CFStream
open Biocaml_ez


let main in_fa out_fa () =
  Fasta.with_file in_fa ~f:(fun _ items ->
      Out_channel.with_file out_fa ~f:(fun oc ->
          Stream.iter items ~f:(fun { Fasta.description ; sequence } ->
              if String.is_substring description ~substring:"melanogaster" then
                let description =
                  String.split ~on:'\t' description
                  |> List.hd_exn
                in
                fprintf oc ">%s\n%s\n" description sequence
            )
        )
    )

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"A small script to format a fasta file containing all known drosophila TE"
    [%map_open
      let file_in  = flag "--in"  (required Filename.arg_type) ~doc:"PATH input fasta"
      and file_out = flag "--out" (required Filename.arg_type) ~doc:"PATH output fasta" in
      main file_in file_out]
