open Bistro
open Biotope

module Script : sig
  type statement
  type t
  val _new_ : statement
  val genome : string -> statement
  val custom_genome : [ `indexed_fasta ] directory -> statement
  val load_bam : [ `indexed_bam ] directory -> statement
  val load_bigwig : Ucsc_gb.bigWig file -> statement
  val make : statement list -> t
end

val script :
  Script.t ->
  text file
