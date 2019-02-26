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
end


let simulation_main ~genome ~np ~mem ~outdir ~verbose:_ () =
  let loggers = [
      Console_logger.create () ;
      Html_logger.create "report.html" ;
    ]
  in
  let outdir = Option.value outdir ~default:"res" in
  let te =
    Te_library.idefix
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
    Bowtie2.bowtie2 ~no_unal:true index (`single_end [fq])
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
