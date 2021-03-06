(*
/pandata/fablet/Severine/trimmed_reads_UrQt-QC20
*)
(* #require "bistro.bioinfo bistro.utils core" *)

open Core
open CFStream
open Bistro
open Bistro_bioinfo
open Bistro.Shell_dsl
open Bistro_utils
open Biocaml_ez

(* === CUSTOM WRAPPERS === *)

let fastq_gz_head (fq_gz : #fastq gz file as 'a) i : 'a =
  Workflow.shell ~descr:"fastq_gz_head" [
    pipe [
      cmd "zcat" [ dep fq_gz ] ;
      cmd "head" [ opt "-n" int (i * 4) ] ;
      cmd "gzip" ~stdout:dest [ string "-c" ]
    ]
  ]

let gzdep (gz : _ gz file) =
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

let filter_fastq_with_sam (sam : sam file) (fq : (#fastq as 'a) gz file) : 'a gz file =
  Workflow.shell ~descr:"filter_fastq_with_sam" [
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

let match_insertions (peaks1 : Macs2.peaks_xls file) (peaks2 : Macs2.peaks_xls file) =
  Workflow.shell ~descr:"match_insertions" ~version:8 [
    cmd "te-insertion-detector" [
      string "match-insertions" ;
      dep peaks1 ;
      dep peaks2 ;
      opt "--output" ident dest ;
    ]
  ]

let cat xs =
  Workflow.shell ~descr:"cat" [
    cmd "cat" [
      list dep xs
    ]
  ]

let gzip x =
  Workflow.shell ~descr:"gzip" [
    cmd "gzip" ~stdout:dest [
      string "-c" ;
      dep x
    ]
  ]

let bowtie2_env = [ docker_image ~account:"pveber" ~name:"bowtie2" ~tag:"2.2.9" () ]
let samtools_env = [ docker_image ~account:"pveber" ~name:"samtools" ~tag:"1.3.1" () ]

(* FIXME!!!  This wrapper doesn't work as one could expect: docker
   logs everything that passes on stdout, which takes LOTS of space in
   that particular case. Either use named pipes or intermediate files...
*)
let bowtie2 (index : [`bowtie2_index] directory) fqs =
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
  Workflow.shell ~descr:"te-insertion-locator-bowtie2" ~mem:(Workflow.int (3 * 1024)) ~np:8 [
    pipe [
      cmd "bowtie2" ~img:bowtie2_env [
        string "--local" ;
        opt "--threads" ident np ;
        opt "-x" (fun index -> seq [dep index ; string "/index"]) index ;
        args ;
      ] ;
      cmd "samtools" ~img:samtools_env ~stdout:dest [
        string "view" ;
        string "-" ;
        opt "-q" int 5 ;
      ] ;
    ]
  ]


type sample = G0 | G1
[@@deriving show]

let samples = [ G0 ; G1 ]

type transposable_element =
  | Known_TE of Te_library.te
  | User_TE of { id : string ; sequence : string }

let show_transposable_element = function
  | Known_TE te -> Te_library.show_te te
  | User_TE { id ; _ } -> id

let load_transposable_elements fn =
  Fasta.(
    with_file fn ~f:(fun _ items ->
        Stream.map items ~f:(fun it ->
            User_TE { id = it.description ; sequence = it.sequence }
          )
        |> Stream.to_list
      )
  )

module Pipeline = struct

  let mel_genome =
    Bistro_unix.wget "ftp://hgdownload.cse.ucsc.edu/goldenPath/dm6/bigZips/dm6.fa.masked.gz"
    |> Bistro_unix.gunzip

  let mel_genome_index =
    Bowtie2.bowtie2_build mel_genome

  let mel_gff : gff file =
    Bistro_unix.wget "ftp://ftp.flybase.net/genomes/Drosophila_melanogaster/dmel_r6.11_FB2016_03/gff/dmel-all-r6.11.gff.gz"
    |> Bistro_unix.gunzip

  let fasta_of_te = function
    | Known_TE te -> Misc.fasta_of_te (Te_library.fasta_of_te te)
    | User_TE { id ; sequence } ->
      Workflow.shell ~descr:("echo." ^ id) [
        cmd "echo" ~stdout:dest [ quote ~using:'"' (string (">" ^ id ^ "\\n"  ^ sequence)) ] ;
      ]

  let index_of_te te = Bowtie2.bowtie2_build (fasta_of_te te)


  let witness_reads_one_way te_index fq1 fq2 =
    let sam1 = bowtie2 te_index (`single_end [fq1]) in
    let filtered2 = filter_fastq_with_sam sam1 fq2 in
    let witness_reads = bowtie2 mel_genome_index (`single_end [filtered2]) in
    object
      method witness_reads = witness_reads
      method filtered_reads = filtered2
      method anchor_reads = sam1
    end

  (* this is because of a bug in macs2 #101 *)
  let macs2 treatment =
    let img = [ docker_image ~account:"pveber" ~name:"macs2" ~tag:"2.1.1" () ] in
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
    Workflow.shell ~descr:"custom.macs2" [
      cmd "bash" ~img [ file_dump script ]
    ]

  let te_positions te_index fq1 fq2 =
    let wr1 = witness_reads_one_way te_index fq1 fq2 in
    let wr2 = witness_reads_one_way te_index fq2 fq1 in
    let insertions =
      (* Macs2.callpeak *) macs2
        (* ~nomodel:true ~extsize:150 ~qvalue:0.1*) (* Macs2.sam *)
        [ wr1#witness_reads ; wr2#witness_reads ] in
    object
      method way1 = wr1
      method way2 = wr2
      method insertions = insertions
      method insertion_xls = insertions
    end

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
    (Art.pe_fastq ao `One, Art.pe_fastq ao `Two)

  let insertions_in_fasta ~te ~genome : [`genome_with_insertions] directory =
    Workflow.shell ~descr:"insertions_in_fasta" [
      cmd "te-insertion-detector" [
        string "insertions-in-fasta" ;
        opt "--te" dep te ;
        opt "--genome" dep genome ;
        opt "--output" ident dest ;
      ]
    ]

  let genome_of_insertions_in_fasta (x : [`genome_with_insertions] directory) : fasta file =
    Workflow.select x ["genome.fa"]

  let simulation te =
    let simulated_genome = insertions_in_fasta ~te:(fasta_of_te te) ~genome:mel_genome in
    object
      method genome = simulated_genome
      method tep n =
        let fq1, fq2 = sequencer n (genome_of_insertions_in_fasta simulated_genome) in
        (te_positions (index_of_te te) (gzip fq1) (gzip fq2))#insertions
    end

  let sample_path_prefix = function
    | G0 -> "data/Severine/DNA-seq_donneesIGH/G0_parent/150102_I595_FCC5W8BACXX_L1_wHAIPI014963-113"
    | G1 -> "data/Severine/DNA-seq_donneesIGH/G1_shpiwi/150102_I595_FCC5W8BACXX_L1_wHAIPI014964-112"

  let sample_path side x =
    sprintf "%s_%d.fq.gz"
      (sample_path_prefix x)
      (match side with `Left -> 1 | `Right -> 2)

  let fastq_gz mode side x : sanger_fastq gz file =
    let fq = Workflow.input (sample_path side x) in
    match mode with
    | `preview i ->
      fastq_gz_head fq (i * 1_000_000)
    | `full -> fq

  let te_positions mode te x =
    let fq1 = fastq_gz mode `Left x in
    let fq2 = fastq_gz mode `Right x in
    te_positions (index_of_te te) fq1 fq2

  let comparison mode te =
    match_insertions (te_positions mode te G0)#insertion_xls (te_positions mode te G1)#insertion_xls

  let stats_of_comparison comp = Workflow.select comp ["stats"]

  let%pworkflow assemble_stats elements stats =
    let match_stats_line c =
      let open Match_insertions in
      sprintf "%d\t%d\t%d\t%d"
        c.left_only
        c.left_with_match
        c.right_only
        c.right_with_match
    in
    let header = "left_only\tleft_with_match\tright_only\tright_with_match" in
    let lines =
      List.map2_exn elements [%eval Workflow.path_list stats] ~f:(fun te p ->
          let counts =
            In_channel.read_all p
            |> Sexp.of_string
            |> Match_insertions.match_stats_of_sexp
          in
          show_transposable_element te ^ "\t" ^ (match_stats_line counts)
        )
    in
    Out_channel.write_lines [%dest] (header :: lines)
end


module Repo = struct
  let root mode path = match mode with
    | `full -> [ "full" ] @ path
    | `preview i -> [ "preview" ; sprintf "%03d" i ] @ path

  let simulation transposable_elements =
    let f te =
      let sim = Pipeline.simulation te in
      let p x = ["simulation" ; show_transposable_element te ; x ] in
      Repo.[
        p "genome" %> sim#genome ;
        p "positions_1X"   %> sim#tep 1. ;
        p "positions_10X"  %> sim#tep 10. ;
        p "positions_30X"  %> sim#tep 30. ;
        p "positions_50X"  %> sim#tep 50. ;
        p "positions_100X" %> sim#tep 100. ;
      ]
    in
    List.map transposable_elements ~f
    |> List.concat

  let detection_pipeline_for_te mode te x =
    let tep = Pipeline.te_positions mode te x in
    let p u = root mode (show_transposable_element te :: show_sample x :: u) in
    Repo.[
      p[ "te_positions" ] %> tep#insertions ;
      p[ "witness_reads1" ] %> tep#way1#witness_reads ;
      p[ "witness_reads2" ] %> tep#way2#witness_reads ;
    ]

  let analysis_pipeline_for_te mode te =
    let comparison = Pipeline.comparison mode te in
    let repo =
      detection_pipeline_for_te mode te G0
      @ detection_pipeline_for_te mode te G1
      @
      Repo.[
        root mode (show_transposable_element te :: [ "comparison" ]) %> comparison
      ]
    in
    repo, Pipeline.stats_of_comparison comparison

  let analysis_pipeline mode transposable_elements =
    let open Repo in
    let repos, stats =
      List.map transposable_elements ~f:(analysis_pipeline_for_te mode)
      |> List.unzip
    in
    root mode ["summary"] %> Pipeline.assemble_stats transposable_elements stats
    :: List.concat repos

end

let main np mem outdir _ f =
  let loggers = [
      Console_logger.create () ;
      Html_logger.create "report.html" ;
    ]
  in
  let outdir = Option.value outdir ~default:"res" in
  let np = Option.value ~default:4 np in
  let mem = Option.value ~default:4 mem in
  let repo = f () in
  Bistro_utils.Repo.(build ~loggers ~np ~mem:(`GB mem) ~outdir repo)

let analysis_mode preview_mode te_list _ np mem outdir verbose () =
  main np mem outdir verbose @@ fun () ->
  let transposable_elements = load_transposable_elements te_list in
  let mode = match preview_mode with
    | None -> `full
    | Some i -> `preview i
  in
  Repo.analysis_pipeline mode transposable_elements

let simulation_mode np mem outdir verbose () =
  main np mem outdir verbose @@ fun () ->
  let te_list = List.map ~f:(fun te -> Known_TE te) Te_library.all_of_te in
  Repo.simulation te_list

let general_args spec =
  let open Command.Spec in
  spec
  +> flag "--np" (optional int) ~doc:"INT Number of available processors"
  +> flag "--mem" (optional int) ~doc:"INT Available memory (in GB)"
  +> flag "--outdir" (optional string) ~doc:"PATH Output directory"
  +> flag "--verbose" no_arg ~doc:" Log actions"

(* let analysis_args = *)
(*   let open Command.Spec in *)
(*   empty *)
(*   +> flag "--preview-mode" (optional int) ~doc:"INT If present, only consider K million reads" *)
(*   +> flag "--te-list" (required string) ~doc:"PATH FASTA containing elements to be tested" *)
(*   +> flag "--genome" (required string) ~doc:"PATH_OR_ID Either a path to a FASTA file or a UCSC Genome Browser ID" *)
(*   |> general_args *)

(* let command = *)
(*   let open Command in *)
(*   group ~summary:"Transposable Element Insertion Detector" [ *)
(*     "run", Command.basic ~summary:"Run detection pipeline" analysis_args analysis_mode ; *)
(*     "simulation", Command.basic ~summary:"Validation pipeline" (general_args Command.Spec.empty) simulation_mode ; *)
(*   ] *)


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
