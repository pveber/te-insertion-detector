(*
/pandata/fablet/Severine/trimmed_reads_UrQt-QC20
*)
(* #require "bistro.bioinfo bistro.utils core" *)

open Core.Std
open Bistro.Std
open Bistro_bioinfo.Std
open Bistro.EDSL


(* === CUSTOM WRAPPERS === *)

let ltr_412_fa_text = ">DM412_LTR
tgtagtatgtgcctatgcaatattaagaacaattaaataaaatagcatattaacttatggcagcactttg
ttgctatgtttatgtttatgtttatgcacgcagttaggccagggcggatgtaacatgatcacccactcga
aggcaaaaagtataagtgcatggtcagcattcacacgccgaccaaatacatattacatacgtacatacat
atctcgctctcccgataagcctagatatataagatatacataagaacgccgctccgctgctggcgtaccc
ggcagcgcagctacgcggattagcctaagtccaaatatattaaaaactgtaaaatcggagagactctgta
gacgttgagcggacagaaccatttctgcctactctaaaatcaaaagaagaaattgaataaatatatgtca
gcccgacggctgccttcaacttaaaacggacttgtgttctgaattggagttcatcattaca
"

let ltr_fa = workflow ~descr:"echo.LTR412" [
    cmd "echo" ~stdout:dest [ quote ~using:'"' (string ltr_412_fa_text) ] ;
  ]

let ltr_index = Bowtie2.bowtie2_build ltr_fa

let fastq_gz_head (fq_gz : _ fastq gz workflow as 'a) i : 'a =
  workflow ~descr:"fastq_gz_head" [
    pipe [
      cmd "zcat" [ dep fq_gz ] ;
      cmd "head" [ opt "-n" int (i * 4) ] ;
      cmd "gzip" ~stdout:dest [ string "-c" ]
    ]
  ]

let gzdep (gz : _ gz workflow) =
  seq ~sep:"" [
    string "<(gunzip -c " ;
    dep gz ;
    string ")"
  ]

let gzdest =
  seq ~sep:"" [
    string ">(gzip -c > " ;
    dest ;
    string ")" ;
  ]

let filter_fastq_with_sam (sam : sam workflow) (fq : 'a fastq gz workflow) : 'a fastq gz workflow =
  workflow ~descr:"filter_fastq_with_sam" [
    cmd "te-insertion-detector" [
      string "filter-fastq-with-sam" ;
      opt "--sam" dep sam ;
      opt "--fastq" gzdep fq ;
      opt "--output" ident gzdest ;
    ]
  ]

let match_insertions (peaks1 : Macs2.peaks_xls workflow) (peaks2 : Macs2.peaks_xls workflow) =
  workflow ~descr:"match_insertions" [
    cmd "te-insertion-detector" [
      string "match-insertions" ;
      dep peaks1 ;
      dep peaks2 ;
      opt "--output" ident dest ;
    ]
  ]

let cat xs =
  workflow ~descr:"cat" [
    cmd "cat" [
      list dep xs
    ]
  ]

let gzip x =
  workflow ~descr:"gzip" [
    cmd "gzip" ~stdout:dest [
      string "-c" ;
      dep x
    ]
  ]

let bowtie2_env = docker_image ~account:"pveber" ~name:"bowtie2" ~tag:"2.2.9" ()

let bowtie2 index fqs =
  let args = match fqs with
    | `single_end fqs ->
      opt "-U" (list gzdep ~sep:",") fqs
    | `paired_end (fqs1, fqs2) ->
      seq [
        opt "-1" (list gzdep ~sep:",") fqs1 ;
        string " " ;
        opt "-2" (list gzdep ~sep:",") fqs2
      ]
  in
  workflow ~descr:"te-insertion-locator-bowtie2" ~mem:(3 * 1024) ~np:8 [
    cmd "bowtie2" ~env:bowtie2_env [
      string "--local" ;
      opt "--threads" ident np ;
      opt "-x" (fun index -> seq [dep index ; string "/index"]) index ;
      args ;
      opt "-S" ident dest ;
    ]
  ]




type sample = G0 | G1
[@@deriving show]

let samples = [ G0 ; G1 ]


module Pipeline = struct

  let mel_genome =
    Unix_tools.wget "ftp://hgdownload.cse.ucsc.edu/goldenPath/dm6/bigZips/dm6.fa.masked.gz"
    |> Unix_tools.gunzip

  let mel_genome_index =
    Bowtie2.bowtie2_build mel_genome

  let mel_gff : gff workflow =
    Unix_tools.wget "ftp://ftp.flybase.net/genomes/Drosophila_melanogaster/dmel_r6.11_FB2016_03/gff/dmel-all-r6.11.gff.gz"
    |> Unix_tools.gunzip

  let witness_reads_one_way fq1 fq2 =
    let sam1 = bowtie2 ltr_index (`single_end [fq1]) in
    let filtered2 = filter_fastq_with_sam sam1 fq2 in
    let witness_reads = bowtie2 mel_genome_index (`single_end [filtered2]) in
    object
      method witness_reads = witness_reads
      method filtered_reads = filtered2
      method anchor_reads = sam1
    end

  let te_positions fq1 fq2 =
    let wr1 = witness_reads_one_way fq1 fq2 in
    let wr2 = witness_reads_one_way fq2 fq1 in
    let insertions =
      Macs2.callpeak
        ~nomodel:true ~extsize:150 ~qvalue:0.1 Macs2.sam
        [ wr1#witness_reads ; wr2#witness_reads ] in
    object
      method way1 = wr1
      method way2 = wr2
      method insertions = insertions
      method insertion_xls = insertions / Macs2.peaks_xls
    end

  (* === SIMULATIONS === *)

  (* Sequencing simulation using Art *)
  let sequencer cov fa =
    let ao =
      Art.(
        art_illumina
          ~aln_output:False
          ~sam_output:False
          ~errfree_sam_output:False
          (Paired_end { len = 150 ;
                        mflen = 400. ;
                        sdev = 20. ;
                        matepair = false })
          (`Coverage_fold cov) fa
      )
    in
    (ao / Art.pe_fastq `One,
     ao / Art.pe_fastq `Two)

  let insertions_in_fasta ~te ~genome : [`genome_with_insertions] directory workflow =
    workflow ~descr:"insertions_in_fasta" [
      cmd "te-insertion-detector" [
        string "insertions-in-fasta" ;
        opt "--te" dep te ;
        opt "--genome" dep genome ;
        opt "--output" ident dest ;
      ]
    ]

  let genome_of_insertions_in_fasta
    : ([`genome_with_insertions] directory, fasta) selector
    = selector ["genome.fa"]

  let simulation root =
    let open Bistro_app in
    let simulated_genome = insertions_in_fasta ~te:ltr_fa ~genome:mel_genome in
    let f n =
      let fq1, fq2 = sequencer n (simulated_genome / genome_of_insertions_in_fasta) in
      (te_positions (gzip fq1) (gzip fq2))#insertions
    in
    [
      ["simulation" ; "genome"] %> simulated_genome ;
      ["simulation" ; "positions_1X"]   %> f 1. ;
      ["simulation" ; "positions_10X"]  %> f 10. ;
      ["simulation" ; "positions_30X"]  %> f 30. ;
      ["simulation" ; "positions_50X"]  %> f 50. ;
      ["simulation" ; "positions_100X"] %> f 100. ;
    ]

  let sample_path_prefix = function
    | G0 -> "data/Severine/DNA-seq_donneesIGH/G0_parent/150102_I595_FCC5W8BACXX_L1_wHAIPI014963-113"
    | G1 -> "data/Severine/DNA-seq_donneesIGH/G1_shpiwi/150102_I595_FCC5W8BACXX_L1_wHAIPI014964-112"

  let sample_path side x =
    sprintf "%s_%d.fq.gz"
      (sample_path_prefix x)
      (match side with `Left -> 1 | `Right -> 2)

  let fastq_gz mode side x : [`sanger] fastq gz workflow =
    let fq = input (sample_path side x) in
    match mode with
    | `preview i ->
      fastq_gz_head fq (i * 1_000_000)
    | `full -> fq

  let te_positions mode x =
    let fq1 = fastq_gz mode `Left x in
    let fq2 = fastq_gz mode `Right x in
    te_positions fq1 fq2

  let comparison mode =
    match_insertions (te_positions mode G0)#insertion_xls (te_positions mode G1)#insertion_xls
end

module Repo = struct
  open Bistro_app

  let root mode path = match mode with
    | `full -> [ "full" ] @ path
    | `preview i -> [ "preview" ; sprintf "%03d" i ] @ path

  let detection_pipeline mode x =
    let tep = Pipeline.te_positions mode x in
    let p u = root mode (show_sample x :: u) in
    [
      p[ "te_positions" ] %> tep#insertions ;
      p[ "witness_reads1" ] %> tep#way1#witness_reads ;
      p[ "witness_reads2" ] %> tep#way2#witness_reads ;
    ]

  let make ~do_simulations ~preview_mode =
    let add_simulations accu =
      if do_simulations then Pipeline.simulation () :: accu else accu
    in
    let mode = match preview_mode with
      | None -> `full
      | Some i -> `preview i
    in
    List.concat (
      (
        List.map samples ~f:(fun x -> detection_pipeline mode x)
        |> add_simulations
      )
      @
      [
        [ root mode [ "comparison" ] %> Pipeline.comparison mode ]
      ]
    )
end

let main do_simulations preview_mode np mem outdir () =
  let outdir = Option.value outdir ~default:"res" in
  let np = Option.value ~default:4 np in
  let mem = Option.value ~default:4 mem in
  Repo.make ~do_simulations ~preview_mode
  |> Bistro_app.local ~np ~mem:(mem * 1024) ~use_docker:true ~outdir

let command =
  let spec =
    let open Command.Spec in
    empty
    +> flag "--simulations" no_arg ~doc:" Perform validation study by simulations"
    +> flag "--preview-mode" (optional int) ~doc:"INT If present, only consider K million reads"
    +> flag "--np" (optional int) ~doc:"INT Number of available processors"
    +> flag "--mem" (optional int) ~doc:"INT Available memory (in GB)"
    +> flag "--outdir" (optional string) ~doc:"PATH Output directory"
  in
  Command.basic ~summary:"Main program" spec main









(* let bed_of_aligned_reads (sam : sam workflow) : bed3 workflow = *)
(*   workflow ~descr:"bed_of_aligned_reads" [ *)
(*     cmd "bed_of_aligned_reads.native" [ *)
(*       opt "--sam" dep sam ; *)
(*       opt "--output" ident dest ; *)
(*     ] *)
(*   ] *)

(* let te_positions_one_way fq1 fq2 = *)
(*   let sam1 = Bowtie2.bowtie2 ~mode:`local ltr_index (`single_end [fq1]) in *)
(*   let filtered2 = filter_fastq_with_sam sam1 fq2 in *)
(*   let sam2 = Bowtie2.bowtie2 ~mode:`local genome_index (`single_end [filtered2]) in *)
(*   bed_of_aligned_reads sam2 *)

(* let te_positions fq1 fq2 = *)
(*   cat [ *)
(*     te_positions_one_way fq1 fq2 ; *)
(*     te_positions_one_way fq2 fq1 ; *)
(*   ] *)


(* let te_counts_one_way fq1 fq2 = *)
(*   let sam1 = Bowtie2.bowtie2 ~mode:`local ltr_index (`single_end [fq1]) in *)
(*   let filtered2 = filter_fastq_with_sam sam1 fq2 in *)
(*   let sam2 = Bowtie2.bowtie2 ~mode:`local genome_index (`single_end [filtered2]) in *)
(*   Htseq.count ~stranded:`no ~idattribute:"gene" (`sam sam2) mel_gff *)
