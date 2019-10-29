open Core
open Te_insertion_detector

include Bistro_utils.Toplevel_eval.Make(struct
  let np = 7
  let mem = 10
end)()

let annotated_insertions spe =
  let open Te_insertion_detector_pipeline in
  let te_library =
    Te_insertion_detector.Misc.load_transposable_elements (Species.te_library spe)
  in
  Dna_sample.annotated_insertions spe te_library
  |> less

let wip_main () =
  let open Te_insertion_detector_pipeline in
  try
    List.concat Igv.Script.[
        [
          _new_ ;
          custom_genome (Species.indexed_genome `Dmel) ;
        ] ;
        List.map [`DmGoth10_k4_1 ; `DmGoth10_k4_2 ; `DmSJRP23_k4_1 ; `DmSJRP23_k4_2 ] ~f:(fun s ->
            load_bigwig (Genomic_sample.signal s)
          ) ;
      ]
    |> Igv.Script.make
    |> Igv.script
    |> path
    |> print_endline
  with Failure msg -> print_endline msg

let wip_command = Command.basic ~summary:"WIP" (Command.Param.return wip_main)


let () = Command.(
    group ~summary:"Transposable Element Insertion Detector" [
      "detection", Pipeline_alt.detection_command ;
      "simulation", Pipeline.simulation_command ;
      "simu2", Pipeline_alt.simulation_command ;
      "filter-fastq-with-sam", Filter_fastq_with_sam.command ;
      "insertions-in-fasta", Insertions_in_fasta.command ;
      "match-insertions", Match_insertions.command ;
      "droso-te-fasta-format", Droso_te_fasta_format.command ;
      "wip", wip_command ;
    ]
    |> run
  )
