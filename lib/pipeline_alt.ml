open Core
open Bistro_bioinfo
open Bistro_utils

module Detection = struct
  type read = {
    chr : string ;
    pos : int ;
    seq : string ;
    mapq : int ;
  }
  [@@deriving sexp]

  type frontier_pair = {
      genome_read : read ;
      et_read : read ;
    }
  [@@deriving sexp]

  let loc_of_read { chr ; pos ; seq ; _ } =
    Gzt.GLoc.{ chr ; lo = pos ; hi = pos + String.length seq }

  let%workflow detect_frontier_pairs ~genome_reads1 ~genome_reads2 ~et_reads1 ~et_reads2 =
    let open Biocaml_ez in
    let open CFStream in
    let read_of_al h al =
      let open Bam.Alignment0 in
      match rname al h, pos al, seq al, mapq al with
      | Ok (Some chr), Some pos, Some seq, Some mapq ->
        Some { chr ; pos ; seq ; mapq }
      | _ -> None
    in
    let frontier_pairs ~genome_reads ~et_reads =
      Bam.with_file0 genome_reads ~f:(fun genome_header genome_als ->
          Bam.with_file0 et_reads ~f:(fun et_header et_als ->
              let rec loop acc =
                match Stream.peek genome_als, Stream.peek et_als with
                | None, _
                | _, None -> acc
                | Some g_al, Some et_al -> (
                    match Bam.Alignment0.(qname g_al, qname et_al) with
                    | None, _ | _, None -> acc
                    | Some g_qname, Some et_qname ->
                      let g_id, et_id = Filter_fastq_with_sam.(normalize_id g_qname, normalize_id et_qname) in
                      if g_id = et_id then (
                        match read_of_al genome_header g_al, read_of_al et_header et_al with
                        | Some genome_read, Some et_read ->
                          Stream.junk genome_als ;
                          Stream.junk et_als ;
                          loop ({ genome_read ; et_read } :: acc)
                        | _ -> loop acc
                      )
                      else if g_id < et_id then (Stream.junk genome_als ; loop acc)
                      else (Stream.junk et_als ; loop acc)
                  )
              in
              loop []
            )
        )
    in
    (frontier_pairs ~genome_reads:[%path genome_reads1] ~et_reads:[%path et_reads2])
    @ (frontier_pairs ~genome_reads:[%path genome_reads2] ~et_reads:[%path et_reads1])

  let%workflow frontier_pair_peak_detection pairs =
    let sorted_pairs =
      List.filter_map [%eval pairs] ~f:(fun { genome_read ; _ } ->
          if genome_read.mapq > 30 then
            Some (loc_of_read genome_read, genome_read)
          else None
        )
      |> List.sort ~compare:(fun (x, _) (y, _) -> Gzt.GLoc.compare x y)
    in
    let groups =
      List.group sorted_pairs ~break:(fun (x, _) (y, _) ->
          Option.value_map (Gzt.GLoc.dist x y) ~default:true ~f:(( < ) 100)
        )
    in
    List.map groups ~f:(fun xs ->
        let locs, reads = List.unzip xs in
        let loc =
          List.reduce_exn locs ~f:Gzt.(fun l1 l2 ->
              let r = Range.convex_hull (GLoc.range l1) (GLoc.range l2) in
              { chr = l1.chr ; lo = r.lo ; hi = r.hi }
            )
        in
        loc, reads
      )

  let%pworkflow dump_frontier_pair_peak_detection groups =
    Out_channel.with_file [%dest] ~f:(fun oc ->
        List.iter [%eval groups] ~f:(fun ((loc : Gzt.GLoc.t), reads) ->
            fprintf oc "%s\t%d\t%d\t%d\n" loc.chr loc.lo loc.hi (List.length reads)
          )
      )

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
      List.map [%eval detected_insertions] ~f:(fun (loc, reads) ->
          Position.{ chrom = loc.chr ; position = Gzt.GLoc.(loc.lo + loc.hi) / 2 },
          Float.of_int (List.length reads)
        )
    in
    let reference =
      [%path Pipeline.Simulation.insertions_of_insertions_in_fasta simulated_genome]
      |> In_channel.read_lines
      |> List.map ~f:(String.split ~on:'\t')
      |> List.map ~f:(function
          | [ x ; y ; _ ] -> x, Int.of_string y
          | _ -> assert false
        )
      |> List.map ~f:Assignment_bed.Position.of_tuple
    in
    let scores =
      List.map detected_insertions ~f:snd
      |> List.dedup_and_sort ~compare:Float.compare
    in
    let eval theta =
      let detected_insertions = List.filter detected_insertions ~f:(fun (_, x) -> x > theta) in
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
      let open Pervasives in
      let prec = float tp /. (float tp +. float fp) in
      let recall = float tp /. (float tp +. float fn) in
      recall, prec
    in
    let res = List.map scores ~f:(fun s ->
        let r, p = eval s in
        s, r, p
      )
    in
    Out_channel.with_file [%dest] ~f:(fun oc ->
        fprintf oc "score\trecall\tprecision\n" ;
        List.iter res ~f:(fun (s, r, p) ->
            fprintf oc "%f\t%f\t%f\n" s r p
          )
      )

  let%pworkflow dump_frontier_pairs xs =
    Out_channel.with_file [%dest] ~f:(fun oc ->
        [%eval xs]
        |> [%sexp_of: frontier_pair list]
        |> Sexp.output_hum oc
      )

  let%pworkflow mapq_hypothesis frontier_pairs insertions =
    let insertions = Gzt.Bed.Bed3.load_as_lmap [%path insertions] in
    let pairs = [%eval frontier_pairs] in
    let mapq =
      List.map pairs ~f:(fun { genome_read ; _ } ->
          genome_read.mapq
        )
      |> Array.of_list
      |> OCamlR_base.Integer.of_array
    in
    let close_to_insertion =
      List.map pairs ~f:(fun { genome_read ; _ } ->
          match Gzt.GAnnot.LMap.closest insertions (loc_of_read genome_read) with
          | None -> false
          | Some (_, _, d) -> Int.abs d < 1_000
        )
      |> Array.of_list
    in
    let df =
      let open OCamlR_base in
      Dataframe.(create [
        integer "mapq" mapq ;
        logical "close" (Logical.of_array close_to_insertion) ;
      ])
    in
    let ntrue = Array.count close_to_insertion ~f:Fn.id in
    let nfalse = Array.count close_to_insertion ~f:Fn.(non id) in
    OCamlR_grDevices.pdf [%dest] ;
    OCamlR_graphics.dataframe_boxplot
      ~main:(sprintf "F = %d / T = %d" nfalse ntrue)
      (OCamlR_stats.Formula.of_string "mapq ~ close")
      df ;
    OCamlR_grDevices.dev_off ()

  let%pworkflow count_table ~gtf detected_inserts =
    let detected_inserts =
      [%eval Bistro.Workflow.list detected_inserts]
      |> List.map ~f:(List.map ~f:fst)
      |> List.concat
    in
    let gtf = Gzt.Gtf.load [%path gtf] in
    let annotation = Gzt.Gtf.Annotation.of_items gtf in
    let genes, _ = Gzt.Gtf.Annotation.genes annotation in
    let exons = String.Table.map genes ~f:Gzt.Gene.exons in
    let introns = String.Table.map genes ~f:Gzt.Gene.introns in
    let utr3' = Gzt.Gtf.Annotation.utr3' annotation in
    let utr5' = Gzt.Gtf.Annotation.utr5' annotation in
    let counts = String.Table.mapi genes ~f:(fun ~key:id ~data:_ ->
        let exons = String.Table.find_exn exons id in
        let introns = String.Table.find_exn introns id in
        let exon_count =
          List.count detected_inserts ~f:(fun i -> List.exists exons ~f:(Gzt.GLoc.intersects i))
        in
        let intron_count =
          List.count detected_inserts ~f:(fun i -> List.exists introns ~f:(Gzt.GLoc.intersects i))
        in
        let utr3'_count =
          Option.value_map (String.Table.find utr3' id) ~default:0 ~f:(fun utr3' ->
              let utr3'_loc = Gzt.Gff.Record.loc utr3' in
              List.count detected_inserts ~f:(fun i -> Gzt.GLoc.intersects i utr3'_loc)
            )
        in
        let utr5'_count =
          Option.value_map (String.Table.find utr5' id) ~default:0 ~f:(fun utr5' ->
              let utr5'_loc = Gzt.Gff.Record.loc utr5' in
              List.count detected_inserts ~f:(fun i -> Gzt.GLoc.intersects i utr5'_loc)
            )
        in
        [ exon_count ; intron_count ; utr5'_count ; utr3'_count ]
      )
    in
    Out_channel.with_file [%dest] ~f:(fun oc ->
        fprintf oc "id\texon\tintron\t5UTR\t3UTR\n" ;
        String.Table.iteri counts ~f:(fun ~key:id ~data:counts ->
            Out_channel.output_string oc id ;
            List.iter counts ~f:(fprintf oc "\t%d") ;
            Out_channel.newline oc
          ) ;
      )

  let pipeline_for_te ~genome_index ~fq1 ~fq2 te =
    let te_index = Bowtie2.bowtie2_build (Misc.fasta_of_te te) in
    let mapped_reads index fq =
      Bowtie2.bowtie2 ~mode:`local ~no_unal:true index (`single_end [fq])
      |> Samtools.bam_of_sam
      |> Picardtools.sort_bam_by_name
    in
    let genome_reads1 = mapped_reads genome_index fq1 in
    let genome_reads2 = mapped_reads genome_index fq2 in
    let et_reads1 = mapped_reads te_index fq1 in
    let et_reads2 = mapped_reads te_index fq2 in
    let frontier_pairs =
      detect_frontier_pairs ~genome_reads1 ~et_reads1 ~genome_reads2 ~et_reads2
    in
    let detected_inserts = frontier_pair_peak_detection frontier_pairs in
    let detected_inserts_bed = dump_frontier_pair_peak_detection detected_inserts in
    object
      method te_index = te_index
      method frontier_pairs = frontier_pairs
      method detected_inserts = detected_inserts
      method detected_inserts_bed = detected_inserts_bed
      method repo = Repo.[
        item ["detected_inserts.bed"] detected_inserts_bed ;
      ]
    end

  let pipeline mode transposable_elements ~fq1 ~fq2 ~gtf ~genome =
    let fq1, fq2 =
      Pipeline.Detection.fastq_gz mode fq1,
      Pipeline.Detection.fastq_gz mode fq2 in
    let genome_index = Bowtie2.bowtie2_build genome in
    let genome_mapped_pairs =
      Bowtie2.bowtie2
        ~no_unal:true ~no_discordant:true ~no_mixed:true
        genome_index
        (`paired_end ([fq1], [fq2])) in
    let filtered_fq1, filtered_fq2 =
      let filter x =
        Misc.filter_fastq_with_sam_gz ~min_mapq:1 ~invert:true genome_mapped_pairs x
      in
      filter fq1, filter fq2
    in
    let res_by_te =
      let f te =
        te, pipeline_for_te ~genome_index ~fq1:filtered_fq1 ~fq2:filtered_fq2 te
      in
      List.map transposable_elements ~f in
    let count_table = Option.map gtf ~f:(fun gtf ->
        List.map res_by_te ~f:(fun (_, x) -> x#detected_inserts)
        |> count_table ~gtf
      )
    in
    object
      method res_by_te = res_by_te
      method repo =
        let count_table =
          Option.value_map count_table ~default:[] ~f:(fun x -> [
                Repo.item ["count_table.tsv"] x
              ]
            )
        in
        count_table ::
        List.map res_by_te ~f:(fun (te, x) -> Repo.shift te.Te_library.id x#repo)
        |> List.concat
    end
end


let simulation_main ~genome ~np ~mem ~outdir ~verbose:_ () =
  let loggers = [
      Console_logger.create () ;
      Html_logger.create "report.html" ;
    ]
  in
  let outdir = Option.value outdir ~default:"res" in
  let te =
    Te_library.jockey
    |> Misc.fasta_of_te
  in
  let original_genome = Pipeline.Detection.fetch_genome genome in
  let simulated_genome = Pipeline.Simulation.insertions_in_fasta ~te ~genome:original_genome in
  let simulated_genome_fa = Pipeline.Simulation.genome_of_insertions_in_fasta simulated_genome in
  let insertion_bed = Pipeline.Simulation.insertions_of_insertions_in_fasta simulated_genome in
  let fq1, fq2 = Pipeline.Simulation.sequencer ~coverage:10. simulated_genome_fa in
  let genome_index = Bowtie2.bowtie2_build original_genome in
  let te_index = Bowtie2.bowtie2_build te in
  let genome_mapped_pairs =
    Bowtie2.bowtie2 ~no_unal:true ~no_discordant:true ~no_mixed:true genome_index (`paired_end ([fq1], [fq2])) in
  let filtered_fq1, filtered_fq2 =
    let filter x =
      Misc.filter_fastq_with_sam ~min_mapq:1 ~invert:true genome_mapped_pairs x
    in
    filter fq1, filter fq2
  in
  let mapped_reads index fq =
    Bowtie2.bowtie2 ~mode:`local ~no_unal:true index (`single_end [fq])
    |> Samtools.bam_of_sam
    |> Picardtools.sort_bam_by_name
  in
  let genome_reads1 = mapped_reads genome_index filtered_fq1 in
  let genome_reads2 = mapped_reads genome_index filtered_fq2 in
  let et_reads1 = mapped_reads te_index filtered_fq1 in
  let et_reads2 = mapped_reads te_index filtered_fq2 in
  let frontier_pairs = Detection.detect_frontier_pairs ~genome_reads1 ~et_reads1 ~genome_reads2 ~et_reads2 in
  let repo = Repo.[
      item ["frontier_reads"] (Detection.dump_frontier_pairs frontier_pairs) ;
      item ["mapq_hyp.pdf"] (Detection.mapq_hypothesis frontier_pairs insertion_bed) ;
      item ["insertions.bed"] insertion_bed ;
      item ["mapped_reads"] (Samtools.indexed_bam_of_sam (Bowtie2.(bowtie2 (bowtie2_build simulated_genome_fa) (`paired_end ([fq1], [fq2]))))) ;
      item ["mapped_on_original.sam"] genome_mapped_pairs ;
      item ["original.fa"] original_genome ;
      item ["simulated_genome.fa"] simulated_genome_fa ;
      item ["simulated_reads_1.fq"] fq1 ;
      item ["simulated_reads_2.fq"] fq2 ;
      item ["filtered_reads_1.fq"] filtered_fq1 ;
      item ["filtered_reads_2.fq"] filtered_fq2 ;
      item ["detected_insertions.bed"] Detection.(dump_frontier_pair_peak_detection (frontier_pair_peak_detection frontier_pairs)) ;
      item ["evaluation.tsv"] Detection.(evaluation ~detected_insertions:(frontier_pair_peak_detection frontier_pairs) ~simulated_genome) ;
    ]
  in
  Bistro_utils.Repo.(build_main ~loggers ~np ~mem:(`GB mem) ~outdir repo)


let simulation_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Run simulation pipeline"
    [%map_open
      let genome = flag "--genome" (required string) ~doc:"PATH_OR_ID Either a path to a FASTA file or a UCSC Genome Browser ID"
      and np = flag "--np" (optional_with_default 8 int) ~doc:"INT Number of available processors"
      and mem = flag "--mem" (optional_with_default 8 int) ~doc:"INT Available memory (in GB)"
      and outdir = flag "--outdir" (optional string) ~doc:"PATH Output directory"
      and verbose = flag "--verbose" no_arg ~doc:" Log actions" in
      simulation_main ~genome ~np ~mem ~outdir ~verbose
    ]

let detection ~preview_mode ~te_list ~fq1 ~fq2 ~gtf ~genome ~np ~mem ~outdir ~verbose:_ () =
  let loggers = [
    Console_logger.create () ;
    Html_logger.create "report.html" ;
  ]
  in
  let outdir = Option.value outdir ~default:"res" in
  let res =
    let transposable_elements = Misc.load_transposable_elements te_list in
    let mode = match preview_mode with
      | None -> `full
      | Some i -> `preview i
    in
    let gtf = Option.map ~f:Bistro.Workflow.input gtf in
    let genome = Bistro.Workflow.input genome in
    Detection.pipeline mode transposable_elements ~fq1 ~fq2 ~gtf ~genome
  in
  Bistro_utils.Repo.(build_main ~loggers ~np ~mem:(`GB mem) ~outdir res#repo)

let detection_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Run detection pipeline"
    [%map_open
      let preview_mode = flag "--preview-mode" (optional int) ~doc:"INT If present, only consider K million reads"
      and te_list = flag "--te-list" (required string) ~doc:"PATH FASTA containing elements to be tested"
      and genome = flag "--genome" (required string) ~doc:"PATH_OR_ID Either a path to a FASTA file or a UCSC Genome Browser ID"
      and fq1 = flag "--fq1" (required Filename.arg_type) ~doc:"PATH FASTQ1 file"
      and fq2 = flag "--fq2" (required Filename.arg_type) ~doc:"PATH FASTQ2 file"
      and gtf = flag "--gtf" (optional Filename.arg_type) ~doc:"PATH GTF annotation"
      and np = flag "--np" (optional_with_default 8 int) ~doc:"INT Number of available processors"
      and mem = flag "--mem" (optional_with_default 8 int) ~doc:"INT Available memory (in GB)"
      and outdir = flag "--outdir" (optional string) ~doc:"PATH Output directory"
      and verbose = flag "--verbose" no_arg ~doc:" Log actions" in
      detection ~preview_mode ~te_list ~fq1 ~fq2 ~genome ~gtf ~np ~mem ~outdir ~verbose
    ]
