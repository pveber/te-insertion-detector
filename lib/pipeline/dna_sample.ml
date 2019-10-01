open Core_kernel
open Bistro
open Bistro_bioinfo
open Te_insertion_detector

type t = [
  | `DmGoth6_dna
  | `DmGoth10_dna
  | `DmSJRP23_dna
  | `DmSJRP7_dna
]
[@@deriving enumerate]

let samples_for_species spe =
  List.filter all ~f:(fun s -> Sample.species s = spe)

let genome s : fasta pworkflow =
  match Sample.species s with
  | `Dmel ->
    Bistro_unix.wget "ftp://ftp.flybase.net/genomes/Drosophila_melanogaster/dmel_r6.29_FB2019_04/fasta/dmel-all-chromosome-r6.29.fasta.gz"
    |> Bistro_unix.gunzip

let annotation s =
  Species.annotation (Sample.species s)

let insertions (s : t) te =
  let res =
    Pipeline_alt.Detection.pipeline_for_te
      ~mode:`full te
      ~fq1:(Sample.r1_path s)
      ~fq2:(Sample.r2_path s)
      ~genome:(genome s)
  in
  [%workflow List.map [%eval res#detected_inserts] ~f:fst]


type annotated_insertion = {
  chrom : string ;
  pos : string ;
  te : string ;
  sample : string ;
}

let%pworkflow annotated_insertions spe te_library =
  let inserts = [%eval
     let samples = samples_for_species spe in
     List.map te_library ~f:(fun te ->
         List.map samples ~f:(fun s ->
             [%workflow s, [%eval insertions s te]]
           )
         |> Workflow.list
       )
     |> Workflow.list
  ]
  in
  let grouped_inserts =
    List.map inserts ~f:(fun ins_by_sample ->
        List.concat_map ins_by_sample ~f:(fun (s, ins) ->
            List.map ins ~f:(fun i -> i, s)
          )
        |> List.sort ~compare:(fun (i1, _) (i2, _) -> Gzt.GLoc.compare i1 i2)
        |> List.group ~break:(fun (i1, _) (i2, _) ->
            match Gzt.GLoc.dist i1 i2 with
            | None -> true
            | Some d -> Int.abs d > 200
          )
        |> List.map ~f:(fun grp ->
            let barycenter =
              List.map grp ~f:(fun ((l : Gzt.GLoc.t), _) -> (float l.lo +. float l.hi) /. 2.)
              |> List.fold ~init:0. ~f:( +. )
              |> (fun s -> Float.to_int (s /. float (List.length grp)))
            in
            let chrom = match grp with
              | (i, _) :: _ -> i.chr
              | _ -> assert false
            in
            chrom, barycenter, List.map grp ~f:snd
          )
      )
  in
  let gtf = Gzt.Gtf.load [%path Species.annotation spe ] in
  let annotation = Gzt.Gtf.Annotation.of_items gtf in
  let genes, _ = Gzt.Gtf.Annotation.genes annotation in
  let gene_map =
    String.Table.to_alist genes
    |> CFStream.Stream.of_list
    |> CFStream.Stream.map ~f:(fun (id, g) ->
        Gzt.Gene.loc g, (id, g)
      )
    |> Gzt.GAnnot.LMap.of_stream
  in
  let samples = samples_for_species spe in
  Out_channel.with_file [%dest] ~f:(fun oc ->
      fprintf oc "chrom\tpos\tTE\t%s\tgene_id\texon\tintron\t5UTR\t3UTR\tupstream\tdownstream\n"
        (String.concat ~sep:"\t" (List.map samples ~f:Sample.to_string)) ;
      List.iter2_exn te_library grouped_inserts ~f:(fun te inserts ->
          List.iter inserts ~f:(fun (chrom, pos, _samples) ->
              let loc = Gzt.GLoc.{ chr = chrom ; lo = pos ; hi = pos + 1 } in
              let genes =
                Gzt.GAnnot.LMap.intersecting_elems gene_map loc
                |> CFStream.Stream.map ~f:snd
                |> CFStream.Stream.to_list
              in
              let gene_id = match genes with
                | [] -> "none"
                | [ id, _ ] -> id
                | _ -> "ambiguous"
              in
              Out_channel.fprintf oc "%s\t%d\t%s\t%s" chrom pos te.id gene_id ;
              Out_channel.newline oc
            )
        ) ;
    )

    (* let exons = String.Table.map genes ~f:Gzt.Gene.exons in
     * let introns = String.Table.map genes ~f:Gzt.Gene.introns in
     * let utr3' = Gzt.Gtf.Annotation.utr3' annotation in
     * let utr5' = Gzt.Gtf.Annotation.utr5' annotation in
     * let counts = String.Table.mapi genes ~f:(fun ~key:id ~data:gene ->
     *     let exons = String.Table.find_exn exons id in
     *     let introns = String.Table.find_exn introns id in
     *     let upstream_region = Gzt.Gene.upstream gene 5_000 in
     *     let downstream_region = Gzt.Gene.downstream gene 5_000 in
     *     List.map2_exn tes detected_inserts ~f:(fun te detected_inserts ->
     *         let count f = List.count detected_inserts ~f in
     *         let count_inter loc = count (fun i -> Gzt.GLoc.intersects i loc) in
     *         let exon_count =
     *           count (fun i -> List.exists exons ~f:(Gzt.GLoc.intersects i))
     *         in
     *         let intron_count =
     *           count (fun i -> List.exists introns ~f:(Gzt.GLoc.intersects i))
     *         in
     *         let utr3'_count =
     *           Option.value_map (String.Table.find utr3' id) ~default:0 ~f:(fun utr3' ->
     *               let utr3'_loc = Gzt.Gff.Record.loc utr3' in
     *               count_inter utr3'_loc
     *             )
     *         in
     *         let utr5'_count =
     *           Option.value_map (String.Table.find utr5' id) ~default:0 ~f:(fun utr5' ->
     *               let utr5'_loc = Gzt.Gff.Record.loc utr5' in
     *               count_inter utr5'_loc
     *             )
     *         in
     *         let upstream_count = count_inter upstream_region in
     *         let downstream_count = count_inter downstream_region in
     *         te.Te_library.id, [ exon_count ; intron_count ; utr5'_count ; utr3'_count ; upstream_count ; downstream_count ]
     *       )
     *   )
     * in
     * () *)
