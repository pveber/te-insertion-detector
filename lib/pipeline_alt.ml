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
                      match String.compare g_id et_id with
                      | -1 -> Stream.junk genome_als ; loop acc
                      |  1 -> Stream.junk et_als ; loop acc
                      | 0 -> (
                          match read_of_al genome_header g_al, read_of_al et_header et_al with
                          | Some genome_read, Some et_read ->
                            loop ({ genome_read ; et_read } :: acc)
                          | _ -> loop acc
                        )
                      | _ -> assert false
                  )
              in
              loop []
            )
        )
    in
    (frontier_pairs ~genome_reads:[%path genome_reads1] ~et_reads:[%path et_reads2])
    @ (frontier_pairs ~genome_reads:[%path genome_reads2] ~et_reads:[%path et_reads1])

  let%pworkflow dump_frontier_pairs xs =
    Out_channel.with_file [%dest] ~f:(fun oc ->
        [%eval xs]
        |> [%sexp_of: frontier_pair list]
        |> Sexp.output_hum oc
      )
end


let simulation_main ~genome ~np ~mem ~outdir ~verbose:_ () =
  let loggers = [
      Console_logger.create () ;
      Html_logger.create "report.html" ;
    ]
  in
  let outdir = Option.value outdir ~default:"res" in
  let np = Option.value ~default:4 np in
  let mem = Option.value ~default:4 mem in
  let te =
    Te_library.idefix
    |> Misc.fasta_of_te
  in
  let original_genome = Pipeline.Detection.fetch_genome genome in
  let simulated_genome = Pipeline.Simulation.insertions_in_fasta ~te ~genome:original_genome in
  let simulated_genome_fa = Pipeline.Simulation.genome_of_insertions_in_fasta simulated_genome in
  let fq1, fq2 = Pipeline.Simulation.sequencer ~coverage:10. simulated_genome_fa in
  let genome_index = Bowtie2.bowtie2_build original_genome in
  let te_index = Bowtie2.bowtie2_build te in
  let mapped_reads index fq =
    Bowtie2.bowtie2 ~no_unal:true index (`single_end [fq])
    |> Samtools.bam_of_sam
    |> Samtools.sort ~on:`name
  in
  let genome_reads1 = mapped_reads genome_index fq1 in
  let genome_reads2 = mapped_reads genome_index fq2 in
  let et_reads1 = mapped_reads te_index fq1 in
  let et_reads2 = mapped_reads te_index fq2 in
  let frontier_pairs = Detection.detect_frontier_pairs ~genome_reads1 ~et_reads1 ~genome_reads2 ~et_reads2 in
  let repo = Repo.[
      item ["frontier_reads"] (Detection.dump_frontier_pairs frontier_pairs) ;
    ]
  in
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
