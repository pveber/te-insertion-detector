(*
/pandata/fablet/Severine/trimmed_reads_UrQt-QC20
*)
(* #require "bistro.bioinfo bistro.utils core" *)

open Core.Std
open Bistro.Std
open Bistro_bioinfo.Std
open Bistro.EDSL

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

let filter_fastq_with_sam (sam : sam workflow) (fq : 'a fastq workflow) : 'a fastq workflow =
  workflow ~descr:"filter_fastq_with_sam" [
    cmd "te-insertion-detector" [
      string "filter-fastq-with-sam" ;
      dep sam ;
      dep fq ;
      dest
    ]
  ]

let cat xs =
  workflow ~descr:"cat" [
    cmd "cat" [
      list dep xs
    ]
  ]

let mel_genome =
  Unix_tools.wget "ftp://hgdownload.cse.ucsc.edu/goldenPath/dm6/bigZips/dm6.fa.masked.gz"
  |> Unix_tools.gunzip

let genome_index =
  Bowtie2.bowtie2_build mel_genome

let mel_gff : gff workflow =
  Unix_tools.wget "ftp://ftp.flybase.net/genomes/Drosophila_melanogaster/dmel_r6.11_FB2016_03/gff/dmel-all-r6.11.gff.gz"
  |> Unix_tools.gunzip


let te_positions_one_way fq1 fq2 =
  let sam1 = Bowtie2.bowtie2 ~mode:`local ltr_index (`single_end [fq1]) in
  let filtered2 = filter_fastq_with_sam sam1 fq2 in
  Bowtie2.bowtie2 ~mode:`local genome_index (`single_end [filtered2])

let te_positions fq1 fq2 =
  let sam1 = te_positions_one_way fq1 fq2 in
  let sam2 = te_positions_one_way fq2 fq1 in
  Macs2.callpeak ~nomodel:true ~extsize:150 ~qvalue:0.5 Macs2.sam [ sam1 ; sam2 ]


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
    te_positions fq1 fq2
  in
  [
    ["simulation" ; "genome"] %> simulated_genome ;
    ["simulation" ; "positions_1X"]   %> f 1. ;
    ["simulation" ; "positions_10X"]  %> f 10. ;
    ["simulation" ; "positions_30X"]  %> f 30. ;
    ["simulation" ; "positions_50X"]  %> f 50. ;
    ["simulation" ; "positions_100X"] %> f 100. ;
  ]


type sample = G0 | G1
[@@deriving show]

let samples = [ G0 ; G1 ]

module Pipeline = struct
  let sample_path_prefix = function
    | G0 -> "data/Severine/DNA-seq_donneesIGH/G0_parent/150102_I595_FCC5W8BACXX_L1_wHAIPI014963-113"
    | G1 -> "data/Severine/DNA-seq_donneesIGH/G1_shpiwi/150102_I595_FCC5W8BACXX_L1_wHAIPI014964-112"

  let sample_path side x =
    sprintf "%s_%d.fq.gz"
      (sample_path_prefix x)
      (match side with `Left -> 1 | `Right -> 2)

  let fastq side x : [`sanger] fastq workflow =
    input (sample_path side x)

  let te_positions x =
    let fq1 = fastq `Left x in
    let fq2 = fastq `Right x in
    let sam1 = Bowtie2.bowtie2 ~mode:`local ltr_index (`single_end [fq1]) in
    let filtered2 = filter_fastq_with_sam sam1 fq2 in
    let te_positions = te_positions fq1 fq2 in
    object
      method ltr_aligned = sam1
      method filtered_reads = filtered2
      method te_positions = te_positions
    end
end

module Repo = struct
  open Bistro_app

  let detection_pipeline root x =
    let tep = Pipeline.te_positions x in
    [
      (root @ [ "ltr_aligned" ])    %> tep#ltr_aligned ;
      (root @ [ "filtered_reads" ]) %> tep#filtered_reads ;
      (root @ [ "te_positions" ])   %> tep#te_positions ;
    ]

end

let pipeline ~do_simulations ~root =
  List.concat (
    simulation () ::
    List.map samples ~f:(fun x -> Repo.detection_pipeline (root @ [ show_sample x ]) x)
  )

let main do_simulations () =
  pipeline ~do_simulations ~root:[]
  |> Bistro_app.local ~np:4 ~mem:(4 * 1024) ~use_docker:true ~outdir:"res"

let command =
  let spec =
    let open Command.Spec in
    empty
    +> flag "--simulations" no_arg ~doc:" Perform validation study by simulations"
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
