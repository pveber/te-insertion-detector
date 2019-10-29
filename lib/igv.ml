open Core_kernel
open Bistro
open Bistro_bioinfo

module Script = struct
  type statement = Template_dsl.template
  type t = Template_dsl.template

  open Template_dsl
  let _new_ = string "new"
  let genome g = string (sprintf "genome %s" g)
  let custom_genome faidx = seq ~sep:" " [
      string "genome" ;
      dep (Samtools.fasta_of_indexed_fasta faidx)
    ]
  let load_bam b = seq ~sep:" " [ string "load" ; dep b ; string "format=bam" ]
  let load_bigwig b = seq ~sep:" " [ string "load" ; dep b ; string "format=bigwig" ]
  let make statements =
    seq ~sep:"\n" statements
end

let script s =
  Workflow.shell ~descr:"igv.script" Shell_dsl.[
    cmd "cp" [ file_dump s ; dest]
  ]
