open Core
open Te_insertion_detector

let () = Command.(
    group ~summary:"Transposable Element Insertion Detector" [
      "oldpipeline", Pipeline.command ;
      "pipeline", Analysis_pipeline.command ;
      "filter-fastq-with-sam", Filter_fastq_with_sam.command ;
      "insertions-in-fasta", Insertions_in_fasta.command ;
      "match-insertions", Match_insertions.command ;
      "droso-te-fasta-format", Droso_te_fasta_format.command ;
    ]
    |> run
  )
