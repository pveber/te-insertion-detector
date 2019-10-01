open Core_kernel
open Bistro
open Bistro_bioinfo

type t = [
  | `DmGoth6_dna
  | `DmGoth6_k4_1
  | `DmGoth6_k4_2
  | `DmGoth6_k9_1
  | `DmGoth6_k9_2
  | `DmGoth6_wce_1
  | `DmGoth6_wce_2
  | `DmGoth10_dna
  | `DmGoth10_k4_1
  | `DmGoth10_k4_2
  | `DmGoth10_k9_1
  | `DmGoth10_k9_2
  | `DmGoth10_wce_1
  | `DmGoth10_wce_2
  | `DmSJRP23_dna
  | `DmSJRP23_k4_1
  | `DmSJRP23_k4_2
  | `DmSJRP23_k9_1
  | `DmSJRP23_k9_2
  | `DmSJRP23_wce_1
  | `DmSJRP23_wce_2
  | `DmSJRP7_dna
  | `DmSJRP7_k4_1
  | `DmSJRP7_k4_2
  | `DmSJRP7_k9_1
  | `DmSJRP7_k9_2
  | `DmSJRP7_wce_1
  | `DmSJRP7_wce_2
]

let to_string = function
  | `DmGoth6_dna -> "DmGoth6_dna"
  | `DmGoth6_k4_1 -> "DmGoth6_k4_1"
  | `DmGoth6_k4_2 -> "DmGoth6_k4_2"
  | `DmGoth6_k9_1 -> "DmGoth6_k9_1"
  | `DmGoth6_k9_2 -> "DmGoth6_k9_2"
  | `DmGoth6_wce_1 -> "DmGoth6_wce_1"
  | `DmGoth6_wce_2 -> "DmGoth6_wce_2"
  | `DmGoth10_dna -> "DmGoth10_dna"
  | `DmGoth10_k4_1 -> "DmGoth10_k4_1"
  | `DmGoth10_k4_2 -> "DmGoth10_k4_2"
  | `DmGoth10_k9_1 -> "DmGoth10_k9_1"
  | `DmGoth10_k9_2 -> "DmGoth10_k9_2"
  | `DmGoth10_wce_1 -> "DmGoth10_wce_1"
  | `DmGoth10_wce_2 -> "DmGoth10_wce_2"
  | `DmSJRP23_dna -> "DmSJRP23_dna"
  | `DmSJRP23_k4_1 -> "DmSJRP23_k4_1"
  | `DmSJRP23_k4_2 -> "DmSJRP23_k4_2"
  | `DmSJRP23_k9_1 -> "DmSJRP23_k9_1"
  | `DmSJRP23_k9_2 -> "DmSJRP23_k9_2"
  | `DmSJRP23_wce_1 -> "DmSJRP23_wce_1"
  | `DmSJRP23_wce_2 -> "DmSJRP23_wce_2"
  | `DmSJRP7_dna -> "DmSJRP7_dna"
  | `DmSJRP7_k4_1 -> "DmSJRP7_k4_1"
  | `DmSJRP7_k4_2 -> "DmSJRP7_k4_2"
  | `DmSJRP7_k9_1 -> "DmSJRP7_k9_1"
  | `DmSJRP7_k9_2 -> "DmSJRP7_k9_2"
  | `DmSJRP7_wce_1 -> "DmSJRP7_wce_1"
  | `DmSJRP7_wce_2 -> "DmSJRP7_wce_2"


let id = function
  | `DmGoth6_k4_1 -> "CNVR32"
  | `DmGoth6_k4_2 -> "CNVR35"
  | `DmGoth6_k9_1 -> "CNVR33"
  | `DmGoth6_k9_2 -> "CNVR36"
  | `DmGoth6_wce_1 -> "CNVR31"
  | `DmGoth6_wce_2 -> "CNVR34"
  | `DmGoth10_k4_1 -> "CNVR15"
  | `DmGoth10_k4_2 -> "CNVR17"
  | `DmGoth10_k9_1 -> "CNVR16"
  | `DmGoth10_k9_2 -> "CNVR18"
  | `DmGoth10_wce_1 -> "CNVR11"
  | `DmGoth10_wce_2 -> "CNVR12"
  | `DmSJRP23_k4_1 -> "CNVR44"
  | `DmSJRP23_k4_2 -> "CNVR47"
  | `DmSJRP23_k9_1 -> "CNVR45"
  | `DmSJRP23_k9_2 -> "CNVR48"
  | `DmSJRP23_wce_1 -> "CNVR43"
  | `DmSJRP23_wce_2 -> "CNVR46"
  | `DmSJRP7_k4_1 -> "CNVR23"
  | `DmSJRP7_k4_2 -> "CNVR25"
  | `DmSJRP7_k9_1 -> "CNVR24"
  | `DmSJRP7_k9_2 -> "CNVR26"
  | `DmSJRP7_wce_1 -> "CNVR7"
  | `DmSJRP7_wce_2 -> "CNVR8"

let species = function
  | `DmGoth6_k4_1
  | `DmGoth6_k4_2
  | `DmGoth6_k9_1
  | `DmGoth6_k9_2
  | `DmGoth6_wce_1
  | `DmGoth6_wce_2
  | `DmGoth10_k4_1
  | `DmGoth10_k4_2
  | `DmGoth10_k9_1
  | `DmGoth10_k9_2
  | `DmGoth10_wce_1
  | `DmGoth10_wce_2
  | `DmSJRP23_k4_1
  | `DmSJRP23_k4_2
  | `DmSJRP23_k9_1
  | `DmSJRP23_k9_2
  | `DmSJRP23_wce_1
  | `DmSJRP23_wce_2
  | `DmSJRP7_k4_1
  | `DmSJRP7_k4_2
  | `DmSJRP7_k9_1
  | `DmSJRP7_k9_2
  | `DmSJRP7_wce_1
  | `DmSJRP7_wce_2
  | `DmGoth6_dna
  | `DmGoth10_dna
  | `DmSJRP23_dna
  | `DmSJRP7_dna -> `Dmel

let r1_path = function
  | `DmGoth6_k4_1
  | `DmGoth6_k4_2
  | `DmGoth6_k9_1
  | `DmGoth6_k9_2
  | `DmGoth6_wce_1
  | `DmGoth6_wce_2
  | `DmGoth10_k4_1
  | `DmGoth10_k4_2
  | `DmGoth10_k9_1
  | `DmGoth10_k9_2
  | `DmGoth10_wce_1
  | `DmGoth10_wce_2
  | `DmSJRP23_k4_1
  | `DmSJRP23_k4_2
  | `DmSJRP23_k9_1
  | `DmSJRP23_k9_2
  | `DmSJRP23_wce_1
  | `DmSJRP23_wce_2
  | `DmSJRP7_k4_1
  | `DmSJRP7_k4_2
  | `DmSJRP7_k9_1
  | `DmSJRP7_k9_2
  | `DmSJRP7_wce_1
  | `DmSJRP7_wce_2 as s ->
    sprintf "data/ChIPseq_trimmed_reads/%s/%s_R1_trimmed_%s.tar.gz"
      (match species s with `Dmel -> "dmel")
      (id s)
      (assert false) (* replicate ? *)
  | `DmGoth6_dna -> "data/FRM/FRM_genomes/Pool1-PE/LibPE-DmGoth6-3_CGATGT_L001_R1.fastq.gz"
  | `DmGoth10_dna -> "data/FRM/FRM_genomes/Pool1-PE/LibPE-DmGoth10-1_ATCACG_L001_R1.fastq.gz"
  | `DmSJRP23_dna -> "data/FRM/FRM_genomes/23_ATTCCT_L006_R1.fastq.gz"
  | `DmSJRP7_dna -> "data/FRM/FRM_genomes/Pool1-PE/LibPE-DmSJRP7_TTAGGC_L001_R1.fastq.gz"

let r2_path = function
  | `DmGoth6_dna -> "data/FRM/FRM_genomes/Pool1-PE/LibPE-DmGoth6-3_CGATGT_L001_R2.fastq.gz"
  | `DmGoth10_dna -> "data/FRM/FRM_genomes/Pool1-PE/LibPE-DmGoth10-1_ATCACG_L001_R2.fastq.gz"
  | `DmSJRP23_dna -> "data/FRM/FRM_genomes/23_ATTCCT_L006_R2.fastq.gz"
  | `DmSJRP7_dna -> "data/FRM/FRM_genomes/Pool1-PE/LibPE-DmSJRP7_TTAGGC_L001_R2.fastq.gz"

let fastq_gz s : sanger_fastq gz pworkflow SE_or_PE.t =
  SE_or_PE.Paired_end (r1_path s, r2_path s)
  |> SE_or_PE.map ~f:Workflow.input
