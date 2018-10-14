open Core
open Bistro.Std
open Bistro_bioinfo.Std
open Bistro_utils
open Misc

module Pipeline = struct
  open Bistro.EDSL

  let index_of_te te = Bowtie2.bowtie2_build (fasta_of_te te)


  let witness_reads_one_way ~te_index ~genome_index fq1 fq2 =
    let anchor_reads = bowtie2 te_index (`single_end [fq1]) in
    let filtered_reads = filter_fastq_with_sam anchor_reads fq2 in
    let filtered_reads_not_in_te =
      filter_fastq_with_sam
        ~invert:true
        (bowtie2 te_index (`single_end [filtered_reads]))
        filtered_reads
    in
    let witness_reads = bowtie2 genome_index (`single_end [filtered_reads_not_in_te]) in
    object
      method witness_reads = witness_reads
      method filtered_reads = filtered_reads
      method anchor_reads = anchor_reads
    end

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
    List.map te_list ~f:(fun te ->
        te_positions ~te ~genome_index fq1 fq2
      )
end


module Repo = struct
  let root mode path = match mode with
    | `full -> path
    | `preview i -> [ "preview" ; sprintf "%03d" i ] @ path

  let analysis_pipeline mode transposable_elements ~fq1 ~fq2 ~genome =
    let results = Pipeline.detection mode transposable_elements fq1 fq2 genome in
    let repos =
      List.map2_exn transposable_elements results ~f:(fun { id ; _ } tep ->
          let p u = root mode (id :: u) in
          Repo.[
            p[ "te_positions" ] %> tep#insertions ;
            p[ "witness_reads1" ] %> tep#way1#witness_reads ;
            p[ "witness_reads2" ] %> tep#way2#witness_reads ;
          ]
        )
    in
    List.concat repos

end

let main ~preview_mode ~te_list ~fq1 ~fq2 ~genome ~np ~mem ~outdir ~verbose:_ () =
  let logger =
    Logger.tee [
      Console_logger.create () ;
      Html_logger.create "report.html" ;
    ]
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
  Bistro_utils.Repo.(build ~logger ~np ~mem:(`GB mem) ~outdir repo)

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Run detection pipeline"
    [%map_open
      let preview_mode = flag "--preview-mode" (optional int) ~doc:"INT If present, only consider K million reads"
      and te_list = flag "--te-list" (required string) ~doc:"PATH FASTA containing elements to be tested"
      and genome = flag "--genome" (required string) ~doc:"PATH_OR_ID Either a path to a FASTA file or a UCSC Genome Browser ID"
      and fq1 = flag "--fq1" (required string) ~doc:"PATH FASTQ1 file"
      and fq2 = flag "--fq2" (required string) ~doc:"PATH FASTQ2 file"
      and np = flag "--np" (optional int) ~doc:"INT Number of available processors"
      and mem = flag "--mem" (optional int) ~doc:"INT Available memory (in GB)"
      and outdir = flag "--outdir" (optional string) ~doc:"PATH Output directory"
      and verbose = flag "--verbose" no_arg ~doc:" Log actions" in
      main ~preview_mode ~te_list ~fq1 ~fq2 ~genome ~np ~mem ~outdir ~verbose
    ]
