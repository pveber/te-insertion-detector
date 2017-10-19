open Core
open CFStream
open Bistro.Std
open Bistro_bioinfo.Std
open Bistro.EDSL
open Bistro_utils
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

let filter_fastq_with_sam (sam : sam workflow) (fq : 'a fastq gz workflow) : 'a fastq gz workflow =
  workflow ~descr:"filter_fastq_with_sam" [
    cmd "bash" [
      file_dump (seq ~sep:" " [
          string "te-insertion-detector" ;
          string "filter-fastq-with-sam" ;
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

module Pipeline = struct

  let fasta_of_te { id ; sequence } =
      workflow ~descr:("echo." ^ id) [
        cmd "echo" ~stdout:dest [ quote ~using:'"' (string (">" ^ id ^ "\\n"  ^ sequence)) ] ;
      ]

  let index_of_te te = Bowtie2.bowtie2_build (fasta_of_te te)


  let witness_reads_one_way ~te_index ~genome_index fq1 fq2 =
    let sam1 = bowtie2 te_index (`single_end [fq1]) in
    let filtered2 = filter_fastq_with_sam sam1 fq2 in
    let witness_reads = bowtie2 genome_index (`single_end [filtered2]) in
    object
      method witness_reads = witness_reads
      method filtered_reads = filtered2
      method anchor_reads = sam1
    end

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

  let te_positions ~te ~genome_index fq1 fq2 =
    let te_index = index_of_te te in
    let wr1 = witness_reads_one_way ~te_index ~genome_index fq1 fq2 in
    let wr2 = witness_reads_one_way ~te_index ~genome_index fq2 fq1 in
    let insertions =
      (* Macs2.callpeak *) macs2
        (* ~nomodel:true ~extsize:150 ~qvalue:0.1*) (* Macs2.sam *)
        [ wr1#witness_reads ; wr2#witness_reads ] in
    object
      method way1 = wr1
      method way2 = wr2
      method insertions = insertions
      method insertion_xls = insertions / Macs2.peaks_xls
    end

  let fetch_genome x =
    match Ucsc_gb.genome_of_string x with
    | Some org -> Ucsc_gb.genome_sequence org
    | None ->
      if String.is_prefix ~prefix:"http://" x then
        Unix_tools.wget x
      else
        input x

  let fastq_gz mode fn : [`sanger] fastq gz workflow =
    let fq = input fn in
    match mode with
    | `preview i ->
      fastq_gz_head fq (i * 1_000_000)
    | `full -> fq


  let detection mode te_list fq1 fq2 genome =
    let fq1 = fastq_gz mode fq1 in
    let fq2 = fastq_gz mode fq2 in
    let genome_index = Bowtie2.bowtie2_build (fetch_genome genome) in
    List.map te_list ~f:(fun ({ id } as te) ->
        te_positions ~te ~genome_index fq1 fq2
      )
end


module Repo = struct
  let root mode path = match mode with
    | `full -> path
    | `preview i -> [ "preview" ; sprintf "%03d" i ] @ path

  let analysis_pipeline mode transposable_elements ~fq1 ~fq2 ~genome =
    let open Bistro_repo in
    let results = Pipeline.detection mode transposable_elements fq1 fq2 genome in
    let repos =
      List.map2_exn transposable_elements results ~f:(fun { id } tep ->
          let p u = root mode (id :: u) in
          Bistro_repo.[
            p[ "te_positions" ] %> tep#insertions ;
            p[ "witness_reads1" ] %> tep#way1#witness_reads ;
            p[ "witness_reads2" ] %> tep#way2#witness_reads ;
          ]
        )
    in
    List.concat repos

end

let main preview_mode te_list fq1 fq2 genome np mem outdir verbose () =
  let logger =
    Bistro_logger.tee
      (Bistro_console_logger.create ())
      (Bistro_html_logger.create "report.html")
  in
  let outdir = Option.value outdir ~default:"res" in
  let np = Option.value ~default:4 np in
  let mem = Option.value ~default:4 mem in
  let repo =
    let transposable_elements = load_transposable_elements te_list in
    let mode = match preview_mode with
      | None -> `full
      | Some i -> `preview i
    in
    Repo.analysis_pipeline mode transposable_elements ~fq1 ~fq2 ~genome
  in
  Bistro_repo.(build ~logger ~np ~mem:(`GB mem) ~outdir repo)

let cli_spec =
  let open Command.Spec in
  empty
  +> flag "--preview-mode" (optional int) ~doc:"INT If present, only consider K million reads"
  +> flag "--te-list" (required string) ~doc:"PATH FASTA containing elements to be tested"
  +> flag "--genome" (required string) ~doc:"PATH_OR_ID Either a path to a FASTA file or a UCSC Genome Browser ID"
  +> flag "--fq1" (required string) ~doc:"PATH FASTQ1 file"
  +> flag "--fq2" (required string) ~doc:"PATH FASTQ2 file"
  +> flag "--np" (optional int) ~doc:"INT Number of available processors"
  +> flag "--mem" (optional int) ~doc:"INT Available memory (in GB)"
  +> flag "--outdir" (optional string) ~doc:"PATH Output directory"
  +> flag "--verbose" no_arg ~doc:" Log actions"

let command =
  let open Command in
  Command.basic ~summary:"Run detection pipeline" cli_spec main
