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
    Bistro.OCamlscript.(
      make
        Filter_fastq_with_sam.contents
        (app "main" [dep sam ; dep fq ; dest ()])
    )
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
    script "ocamlscript" (string Insertions_in_fasta.contents) ~args:[
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

let detection_pipeline fq1_path fq2_path =
  let open Bistro_app in
  let fq1 = input fq1_path in
  let fq2 = input fq2_path in
  let sam1 = Bowtie2.bowtie2 ~mode:`local ltr_index (`single_end [fq1]) in
  let filtered2 = filter_fastq_with_sam sam1 fq2 in
  let te_positions = te_positions fq1 fq2 in
  [
    [ "ltr_aligned" ] %> sam1 ;
    [ "filtered2" ] %> filtered2 ;
    [ "te_positions" ] %> te_positions ;
  ]

let pipeline fq1_path fq2_path =
  detection_pipeline fq1_path fq2_path
  @ simulation ()

let main fq1_path fq2_path () =
  pipeline fq1_path fq2_path
  |> Bistro_app.local ~np:4 ~mem:(4 * 1024) ~use_docker:true ~outdir:"res"

let command =
  let spec =
    let open Command.Spec in
    empty
    +> anon ("FQ1" %: file)
    +> anon ("FQ2" %: file)
  in
  Command.basic ~summary:"insertion_ET pipeline" spec main

let () = Command.run command









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
