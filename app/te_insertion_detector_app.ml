open Core
open Te_insertion_detector

include Bistro_utils.Toplevel_eval.Make(struct
  let np = 7
  let mem = 10
end)()

let annotated_insertions spe =
  let open Te_insertion_detector_pipeline in
  let te_library =
    Species.load_te_library "data/consensus_tousET_tousDroso.txt"
  in
  Dna_sample.annotated_insertions spe te_library
  |> less

let browse_chIP_datasets () =
  let open Bistro in
  let open Te_insertion_detector_pipeline in
  let samples = EM_ChIP_sample.subset ~species:`Dmel () in
  let signals = List.map samples ~f:(fun s ->
      Genomic_sample.signal s
    )
  in
  [%workflow
    let igv = new Gzt.Igv.proxy () in
    ignore igv#_new_ ;
    ignore @@ igv#genome [%path Species.indexed_genome `Dmel] ;
    List.iter2_exn samples  [%eval Workflow.eval_paths signals] ~f:(fun s signal ->
        ignore @@ igv#load ~format:"bigwig" ~name:(Sample.to_string s) signal
      )
  ]

let repo () =
  let loggers = [ Bistro_utils.Console_logger.create () ] in
  Te_insertion_detector_pipeline.Repo.make ()
  |> Bistro_utils.Repo.build_main ~loggers ~np:6 ~mem:(`GB 8) ~outdir:"results"

let wip_main () =
  try
    (* eval @@ browse_chIP_datasets () *)
    (* annotated_insertions `Dsim *)
    repo ()
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
