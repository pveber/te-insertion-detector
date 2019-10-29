open Bistro
open Bistro_bioinfo

module Script : sig
  type statement
  type t
  val _new_ : statement
  val genome : string -> statement
  val custom_genome : indexed_fasta pworkflow -> statement
  val load_bam : indexed_bam pworkflow -> statement
  val load_bigwig : Ucsc_gb.bigWig pworkflow -> statement
  val make : statement list -> t
end

val script :
  Script.t ->
  text_file pworkflow
