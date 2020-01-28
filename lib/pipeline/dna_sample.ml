open Core_kernel
open Bistro
open Te_insertion_detector

type t = [
  | `DmGoth6_dna
  | `DmGoth10_dna
  | `DmSJRP23_dna
  | `DmSJRP7_dna
  | `DsGoth3_dna
  | `DsGoth6_dna
  | `DsSJRP27_dna
  | `DsSJRP9_dna
]
[@@deriving enumerate]

let samples_for_species spe =
  List.filter all ~f:(fun s -> Poly.(Sample.species s = spe))

let annotation s =
  Species.annotation (Sample.species s)

let insertions (s : t) te =
  let res =
    Pipeline_alt.Detection.pipeline_for_te
      ~mode:`full te
      ~fq1:(Sample.r1_path s)
      ~fq2:(Sample.r2_path s)
      ~genome:(Species.genome (Sample.species s))
  in
  res#detected_inserts

type annotated_insertion = {
  chrom : string ;
  pos : string ;
  te : string ;
  sample : string ;
}

let group_insertions_by_te spe te_library =
  let samples = samples_for_species spe in
  List.map te_library ~f:(fun te ->
      List.map samples ~f:(fun s ->
          [%workflow s, [%eval insertions s te]]
        )
      |> Workflow.list
    )
  |> Workflow.list

let merge_insertions_from_all_samples inserts =
  List.map inserts ~f:(fun ins_by_sample ->
      List.concat_map ins_by_sample ~f:(fun (s, ins) ->
          List.map ins ~f:(fun (loc, reads) -> loc, (s, reads))
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

let%pworkflow insertions_from_all_samples_bed spe te_library =
  let inserts = [%eval group_insertions_by_te spe te_library ] in
  let grouped_inserts = merge_insertions_from_all_samples inserts in
  let open Gzt.Bed.Bed4 in
  Out_channel.with_file [%dest] ~f:(fun oc ->
      List.iter2_exn te_library grouped_inserts ~f:(fun te inserts ->
          List.iter inserts ~f:(fun (chrom, chromStart, samples_with_reads) ->
              let name =
                sprintf "%s|%s" te.id
                  (List.map samples_with_reads ~f:(fun (s, _) ->
                       Sample.to_string s
                     )
                   |> String.concat ~sep:":")
              in
              { chrom ; chromStart ; chromEnd = chromStart + 1 ; name }
              |> Record.to_line
              |> (fun x -> Out_channel.output_string oc x ; Out_channel.output_char oc '\n')
            )
        )
    )

let%pworkflow annotated_insertions spe te_library =
  let inserts = [%eval group_insertions_by_te spe te_library ] in
  let grouped_inserts = merge_insertions_from_all_samples inserts in
  let gtf = Gzt.Gtf.load [%path Species.annotation spe ] in
  let annotation = Gzt.Gtf.Annotation.of_items gtf in
  let genes, _ = Gzt.Gtf.Annotation.genes annotation in
  let feature_map annotation f =
    String.Table.to_alist annotation
    |> CFStream.Stream.of_list
    |> CFStream.Stream.concat_map ~f:(fun (_, g) ->
        CFStream.Stream.of_list (f g)
      )
    |> Gzt.GAnnot.LSet.of_stream
  in
  let exon_map = feature_map genes Gzt.Gene.exons in
  let intron_map = feature_map genes Gzt.Gene.introns in
  let utr3_map = feature_map (Gzt.Gtf.Annotation.utr3' annotation) (fun r -> [ Gzt.Gtf.Record.loc r ]) in
  let utr5_map = feature_map (Gzt.Gtf.Annotation.utr5' annotation) (fun r -> [ Gzt.Gtf.Record.loc r ]) in
  let upstream_map = feature_map genes Gzt.(fun g ->
      List.map g.Gene.transcripts ~f:(fun t ->
          Gene.Transcript.upstream t 5_000)
    ) in
  let downstream_map = feature_map genes Gzt.(fun g ->
      List.map g.Gene.transcripts ~f:(fun t ->
          Gene.Transcript.upstream t 5_000)
    ) in
  let gene_map =
    String.Table.to_alist genes
    |> CFStream.Stream.of_list
    |> CFStream.Stream.map ~f:(fun (id, g) ->
        Gzt.Gene.loc g, (id, g)
      )
    |> Gzt.GAnnot.LMap.of_stream
  in
  let samples = samples_for_species spe in
  let feature_maps = [ exon_map ; intron_map ; utr3_map ; utr5_map ; upstream_map ; downstream_map ] in
  Out_channel.with_file [%dest] ~f:(fun oc ->
      fprintf oc "chrom\tpos\tTE\tgene_id\tnb_reads\t%s\texon\tintron\t5UTR\t3UTR\tupstream\tdownstream\n"
        (String.concat ~sep:"\t" (List.map samples ~f:Sample.to_string)) ;
      List.iter2_exn te_library grouped_inserts ~f:(fun te inserts ->
          List.iter inserts ~f:(fun (chrom, pos, samples_with_reads) ->
              let loc = Gzt.GLoc.{ chr = chrom ; lo = pos ; hi = pos + 1 } in
              let found = List.map samples_with_reads ~f:fst in
              let nb_reads =
                List.map samples_with_reads ~f:(fun (_,reads) -> List.length reads)
                |> List.reduce_exn ~f:Int.max
              in
              let genes =
                Gzt.GAnnot.LMap.intersecting_elems gene_map loc
                |> CFStream.Stream.map ~f:snd
                |> CFStream.Stream.to_list
              in
              let gene_id = match genes with
                | [] -> "NA"
                | xs -> List.map xs ~f:fst |> String.concat ~sep:":"
              in
              let sample_cols =
                List.map samples ~f:(fun s ->
                    if List.mem found s ~equal:Poly.equal then "1" else "0"
                  )
                |> String.concat ~sep:"\t"
              in
              let region_type =
                List.map feature_maps ~f:(fun map ->
                    if Gzt.GAnnot.LSet.intersects map loc then "1" else "0"
                  )
                |> String.concat ~sep:"\t"
              in
              Out_channel.fprintf oc "%s\t%d\t%s\t%s\t%d\t%s\t%s" chrom pos te.id gene_id nb_reads sample_cols region_type ;
              Out_channel.newline oc
            )
        ) ;
    )
