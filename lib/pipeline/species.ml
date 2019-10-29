open Bistro
open Bistro_bioinfo

type t = [`Dmel | `Dsim]

let annotation s : gff3 pworkflow =
  match s with
  | `Dmel ->
    Bistro_unix.wget "ftp://ftp.flybase.net/genomes/Drosophila_melanogaster/dmel_r6.29_FB2019_04/gtf/dmel-all-r6.29.gtf.gz"
    |> Bistro_unix.gunzip

let reduced_te_library = function
  | `Dmel ->
    "data/reduced_melanogaster_te_list.fa"

let te_library = function
  | `Dmel ->
    "data/melanogaster_te_list.fa"

let genome s : fasta pworkflow =
  match s with
  | `Dmel ->
    Bistro_unix.wget "ftp://ftp.flybase.net/genomes/Drosophila_melanogaster/dmel_r6.29_FB2019_04/fasta/dmel-all-chromosome-r6.29.fasta.gz"
    |> Bistro_unix.gunzip

let indexed_genome s = Samtools.faidx (genome s)

let bowtie2_index s =
  Bowtie2.bowtie2_build (genome s)
