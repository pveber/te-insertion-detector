open Bistro
open Bistro_bioinfo

type t = [`Dmel | `Dsim]

let annotation s : gff3 pworkflow =
  Bistro_unix.wget (
    match s with
    | `Dmel -> "ftp://ftp.flybase.net/genomes/Drosophila_melanogaster/dmel_r6.16_FB2017_03/gtf/dmel-all-r6.16.gtf.gz"
    | `Dsim -> "ftp://ftp.flybase.net/genomes/Drosophila_simulans/dsim_r2.02_FB2017_04/gtf/dsim-all-r2.02.gtf.gz"
  )
  |> Bistro_unix.gunzip

let reduced_te_library = function
  | `Dmel ->
    "data/reduced_melanogaster_te_list.fa"
  | `Dsim ->
    assert false

let te_library = function
  | `Dmel ->
    "data/melanogaster_te_list.fa"
  | `Dsim ->
    assert false

let genome s : fasta pworkflow =
  Bistro_unix.wget (
    match s with
    | `Dmel -> "ftp://ftp.flybase.net/genomes/Drosophila_melanogaster/dmel_r6.16_FB2017_03/fasta/dmel-all-chromosome-r6.16.fasta.gz"
    | `Dsim -> "ftp://ftp.flybase.net/genomes/Drosophila_simulans/dsim_r2.02_FB2017_04/fasta/dsim-all-chromosome-r2.02.fasta.gz"
  )
  |> Bistro_unix.gunzip

let indexed_genome s = Samtools.faidx (genome s)

let bowtie2_index s =
  Bowtie2.bowtie2_build (genome s)
