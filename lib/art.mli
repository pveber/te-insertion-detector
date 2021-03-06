open Bistro

type _ tbool =
  | True  : [`True] tbool
  | False : [`False] tbool

type _ read_model =
  | Single_end : int -> [`single_end] read_model
  | Paired_end : paired_end -> [`paired_end] read_model

and paired_end = {
  len : int ;
  mflen : float ;
  sdev : float ;
  matepair : bool ;
}

type 'a art_illumina_output =
  [ `art_illumina_output of 'a ]
  constraint 'a = < aln : _ ;
                    errfree_sam : _ ;
                    sam : _ ;
                    read_model : _ >

val art_illumina :
  ?qprof1:string ->
  ?qprof2:string ->
  ?amplicon:bool ->
  ?id:string ->
  ?insRate:float ->
  ?insRate2:float ->
  ?delRate:float ->
  ?delRate2:float ->
  ?maskN:int ->
  ?qShift:float ->
  ?qShift2:float ->
  ?rndSeed:float ->
  ?sepProf:bool ->
  ?seqSys:[< `GA1 | `GA2 | `HS10 | `HS20 | `HS25 | `MS ] ->
  ?cigarM:bool ->
  aln_output:'a tbool ->
  errfree_sam_output:'b tbool ->
  sam_output:'c tbool ->
  'rm read_model ->
  [< `Coverage_fold of float | `Read_count of int ] ->
  fasta file ->
  < aln : 'a;
    errfree_sam : 'b;
    read_model : 'rm;
    sam : 'c > art_illumina_output directory

val se_fastq :
  < read_model : [`single_end] ; .. > art_illumina_output directory ->
  fastq file

val pe_fastq :
  < read_model : [`paired_end] ; .. > art_illumina_output directory ->
  [`One | `Two] ->
  fastq file
