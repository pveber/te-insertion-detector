open Core_kernel
open Bistro

let bed x : bed4 file =
  Workflow.input (
    Filename.concat
      "data/insertions_ion_torrent"
      (
        match x with
        | `DmGoth10_dna -> "Dm_Goth_10-1_insertion.bed"
        | `DmGoth6_dna  -> "Dm_goth_6-3_insertion.bed"
        | `DmSJRP23_dna -> "Dm_SJ_23_insertion.bed"
        | `DmSJRP7_dna   -> "Dm_SJ7_insertion.bed"
        | `DsGoth3_dna  -> "Ds_Goth_3-1_insertion.bed"
        | `DsGoth6_dna  -> "Ds_Goth_6-13_insertion.bed"
        | `DsSJRP27_dna -> "Ds_SJ_27_insertion.bed"
        | `DsSJRP9_dna  -> "Ds_sj9_insertion.bed"
      )
  )

let%workflow compare_with_illumina_insertions spe te_library =
  let bed_paths = [%eval
     Workflow.path_list (
       Dna_sample.samples_for_species spe
       |> List.map ~f:bed
     )
   ]
  in
  let ion_torrent_insertions =
    List.concat_map bed_paths ~f:Biotk.Bed.Bed4.load_records
    |> Biotk.GAnnot.LAssoc.of_list ~f:(fun item ->
        Biotk.Bed.Bed4.Record.loc item
      )
  in
  let illumina_insertions =
    [%path Dna_sample.annotated_insertions spe te_library]
    |> In_channel.read_lines
    |> Fn.flip List.drop 1
    |> Biotk.GAnnot.LAssoc.of_list ~f:(fun l ->
        match String.split ~on:'\t' l with
        | chr :: pos :: _ ->
          let pos = Int.of_string pos in
          { Biotk.GLoc.chr ; lo = pos ; hi = pos + 1 }
        | _ -> assert false
      )
  in
  let matching =
    Biotk.GAnnot.LAssoc.matching
      ~mode:`Point
      ~max_dist:1_000
      illumina_insertions
      ion_torrent_insertions
  in
  let intersection = List.count matching ~f:(function
      | `Match _ -> true
      | `Left _ -> false
      | `Right _ -> false
    )
  and illumina = List.count matching ~f:(function
      | `Match _ -> true
      | `Left _ -> true
      | `Right _ -> false
    )
  and ion_torrent = List.count matching ~f:(function
      | `Match _ -> true
      | `Left _ -> false
      | `Right _ -> true
    )
  in
  (intersection, illumina, ion_torrent)
