open Core
open Bistro
open Bistro_bioinfo
open Bistro_utils
open Misc

module Detection = struct
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
       macs2
      (* Macs2.callpeak *)
      (*   ~nomodel:true ~extsize:150 ~qvalue:0.1 Macs2.sam *)
        [ wr1#witness_reads ; wr2#witness_reads ] in
    object
      method way1 = wr1
      method way2 = wr2
      method insertions = insertions
      method insertion_xls = Macs2.peaks_xls insertions
    end

  let fetch_genome x =
    match Ucsc_gb.genome_of_string x with
    | Some org -> Ucsc_gb.genome_sequence org
    | None ->
      if String.is_prefix ~prefix:"http://" x then
        Bistro_unix.wget x
      else
        Workflow.input x

  let fastq_gz mode fn : sanger_fastq gz pworkflow =
    let fq = Workflow.input fn in
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


module Simulation = struct
  open Bistro.Shell_dsl

  (* Sequencing simulation using Art *)
  let sequencer ~coverage fa =
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
          (`Coverage_fold coverage) fa
      )
    in
    (Art.pe_fastq ao `One, Art.pe_fastq ao `Two)

  let insertions_in_fasta ~te ~genome : [`genome_with_insertions] dworkflow =
    Workflow.shell ~descr:"insertions_in_fasta" [
      cmd "te-insertion-detector" [
        string "insertions-in-fasta" ;
        opt "--te" dep te ;
        opt "--genome" dep genome ;
        opt "--output" ident dest ;
      ]
    ]

  let genome_of_insertions_in_fasta (x : [`genome_with_insertions] dworkflow) : fasta pworkflow =
    Workflow.select x ["genome.fa"]

  let insertions_of_insertions_in_fasta (x : [`genome_with_insertions] dworkflow) : bed3 pworkflow =
    Workflow.select x ["inserts.bed"]

  let simulation te original_genome =
    let original_genome = Detection.fetch_genome original_genome in
    let simulated_genome = insertions_in_fasta ~te:(fasta_of_te te) ~genome:original_genome in
    let te_positions ~coverage genome =
      let fq1, fq2 = sequencer ~coverage genome in
      Detection.te_positions ~te ~genome_index:(Bowtie2.bowtie2_build genome) (gzip fq1) (gzip fq2)
    in
    object
      method original_genome = original_genome
      method genome = simulated_genome
      method te_positions ~coverage =
        te_positions ~coverage (genome_of_insertions_in_fasta simulated_genome)
      method te_positions_in_original_genome ~coverage =
        te_positions ~coverage original_genome
    end

  module Pred = struct
    type t = Assignment_bed.Position.t * float
    let position = fst
  end

  module Position = struct
    include Assignment_bed.Position
    let position x = x
  end

  let%pworkflow evaluation ~detected_insertions ~simulated_genome =
    let module M = Assignment_dynamic.Dynamic.Make(Pred)(Position) in
    let detected_insertions =
      let file = [%path detected_insertions] in
      Assignment_bed.Parser.(parse_bed ~file ~line_parser:parse_position_score)
    in
    let reference =
      [%path insertions_of_insertions_in_fasta simulated_genome]
      |> In_channel.read_lines
      |> List.map ~f:(String.lsplit2_exn ~on:'\t')
      |> List.map ~f:(fun (x, y) -> x, Int.of_string y)
      |> List.map ~f:Assignment_bed.Position.of_tuple
    in
    let matching =
      M.align_list
        detected_insertions
        reference
        ~max_dist:10_000
    in
    let tp = List.count matching ~f:(function
        | M.Match ((_, _), _) -> true
        | Insertion (_, _) -> false
        | Deletion _ -> false
      )
    and fp = List.count matching ~f:(function
        | M.Match ((_, _), _) -> false
        | Insertion (_, _) -> true
        | Deletion _ -> false
      )
    and fn = List.count matching ~f:(function
        | M.Match ((_, _), _) -> false
        | Insertion (_, _) -> false
        | Deletion _ -> true
      )
    in
    Out_channel.with_file [%dest] ~f:Pervasives.(fun oc ->
        fprintf oc "prec=%f\trecall=%f\n"
          (float tp /. (float tp +. float fp))
          (float tp /. (float tp +. float fn))
      )
end

module Repo = struct
  let root mode path = match mode with
    | `full -> path
    | `preview i -> [ "preview" ; sprintf "%03d" i ] @ path

  let analysis_pipeline mode transposable_elements ~fq1 ~fq2 ~genome =
    let results = Detection.detection mode transposable_elements fq1 fq2 genome in
    let repos =
      List.map2_exn transposable_elements results ~f:(fun { id ; _ } tep ->
          let p u = root mode (id :: u) in
          Repo.[
            p[ "te_positions" ] %> tep#insertions ;
            p[ "witness_reads1" ] %> tep#way1#witness_reads ;
            p[ "witness_reads2" ] %> tep#way2#witness_reads ;
            p[ "anchor_reads1" ] %> tep#way1#anchor_reads ;
            p[ "anchor_reads2" ] %> tep#way2#anchor_reads ;
          ]
        )
    in
    List.concat repos

  let simulation transposable_elements genome =
    let f te =
      let sim =
        Simulation.simulation (Te_library.fasta_of_te te) genome in
      let p x = [Te_library.show_te te ; x ] in
      let f coverage =
        let detected_insertions = (sim#te_positions ~coverage:(Float.of_int coverage))#insertion_xls in
        Repo.[
          item (p (sprintf "positions_%dX.bed" coverage)) detected_insertions ;
          item (p (sprintf "comparaison_%dX" coverage)) (Simulation.evaluation ~detected_insertions ~simulated_genome:sim#genome) ;
        ]
      in
      Repo.item ["genome"] sim#genome
      :: (List.concat @@ List.map [ 1 ; 10 ; 30 ; 50 ; 100 ] ~f)
    in
    List.map transposable_elements ~f
    |> List.concat
end

let detection ~preview_mode ~te_list ~fq1 ~fq2 ~genome ~np ~mem ~outdir ~verbose:_ () =
  let loggers = [
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
  Bistro_utils.Repo.(build_main ~loggers ~np ~mem:(`GB mem) ~outdir repo)

let detection_command =
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
      detection ~preview_mode ~te_list ~fq1 ~fq2 ~genome ~np ~mem ~outdir ~verbose
    ]


let simulation_main ~genome ~np ~mem ~outdir ~verbose:_ () =
  let loggers = [
      Console_logger.create () ;
      Html_logger.create "report.html" ;
    ]
  in
  let outdir = Option.value outdir ~default:"res" in
  let np = Option.value ~default:4 np in
  let mem = Option.value ~default:4 mem in
  let repo = Repo.simulation Te_library.all_of_te genome in
  Bistro_utils.Repo.(build_main ~loggers ~np ~mem:(`GB mem) ~outdir repo)

let simulation_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Run simulation pipeline"
    [%map_open
      let genome = flag "--genome" (required string) ~doc:"PATH_OR_ID Either a path to a FASTA file or a UCSC Genome Browser ID"
      and np = flag "--np" (optional int) ~doc:"INT Number of available processors"
      and mem = flag "--mem" (optional int) ~doc:"INT Available memory (in GB)"
      and outdir = flag "--outdir" (optional string) ~doc:"PATH Output directory"
      and verbose = flag "--verbose" no_arg ~doc:" Log actions" in
      simulation_main ~genome ~np ~mem ~outdir ~verbose
    ]
