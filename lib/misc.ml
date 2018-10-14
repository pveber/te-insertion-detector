open Core
open Bistro.Std
open Bistro_bioinfo.Std
open Bistro.EDSL
open CFStream
open Biocaml_ez

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

let filter_fastq_with_sam ?invert (sam : sam workflow) (fq : 'a fastq gz workflow) : 'a fastq gz workflow =
  workflow ~descr:"filter_fastq_with_sam" [
    cmd "bash" [
      file_dump (seq ~sep:" " [
          string "te-insertion-detector" ;
          string "filter-fastq-with-sam" ;
          option (flag string "-v") invert ;
          opt "--sam" dep sam ;
          opt "--fastq" gzdep fq ;
          opt "--output" ident gzdest ;
        ] ;
        )
    ]
  ]

let match_insertions (peaks1 : Macs2.peaks_xls workflow) (peaks2 : Macs2.peaks_xls workflow) =
  workflow ~descr:"match_insertions" ~version:8 [
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
let samtools_env = docker_image ~account:"pveber" ~name:"samtools" ~tag:"1.3.1" ()

(* FIXME!!!  This wrapper doesn't work as one could expect: docker
   logs everything that passes on stdout, which takes LOTS of space in
   that particular case. Either use named pipes or intermediate files...
*)
let bowtie2 (index : Bowtie2.index workflow) fqs =
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
    pipe [
      cmd "bowtie2" ~env:bowtie2_env [
        string "--local" ;
        opt "--threads" ident np ;
        opt "-x" (fun index -> seq [dep index ; string "/index"]) index ;
        args ;
      ] ;
      cmd "samtools" ~env:samtools_env ~stdout:dest [
        string "view" ;
        string "-" ;
        opt "-q" int 5 ;
      ] ;
    ]
  ]


type transposable_element = { id : string ; sequence : string }

let load_transposable_elements fn =
  Fasta.(
    with_file
      ~fmt:{ default_fmt with allow_empty_lines = true }
      fn
      ~f:(fun _ items ->
          Stream.map items ~f:(fun it ->
              { id = it.description ; sequence = it.sequence }
            )
          |> Stream.to_list
        )
  )

let fasta_of_te { id ; sequence } =
  workflow ~descr:("echo." ^ id) [
    cmd "echo" ~stdout:dest [ quote ~using:'"' (string (">" ^ id ^ "\\n"  ^ sequence)) ] ;
  ]

(* this is because of a bug in macs2 #101 *)
let macs2 treatment =
  let env = docker_image ~account:"pveber" ~name:"macs2" ~tag:"2.1.1" () in
  let script =
    seq ~sep:"\n" [
      string "set -e" ;
      seq ~sep:"" [ string "export F='" ; list ~sep:" " dep treatment ; string "'" ] ;
      seq ~sep:"" [ string "export DEST='" ; dest ; string "'" ] ;
      string {|
if [ `head -q -n 1000 $F | wc -l` -gt 20 ]; then
  macs2 callpeak --outdir $DEST --name macs2 --extsize 150 --nomodel --qvalue 0.1 --treatment $F;
else
  mkdir -p $DEST;
  touch $DEST/macs2_peaks.xls;
fi
|}
    ]
  in
  workflow ~descr:"custom.macs2" [
    cmd "bash" ~env [ file_dump script ]
  ]
