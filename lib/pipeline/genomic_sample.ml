(* open Core_kernel *)
open Bistro_bioinfo

type t = [
  | `DmGoth6_wce_1
  | `DmGoth6_wce_2
  | `DmGoth10_k4_1
  | `DmGoth10_k4_2
  | `DmGoth10_k9_1
  | `DmGoth10_k9_2
  | `DmGoth10_wce_1
  | `DmGoth10_wce_2
  | `DmSJRP23_k4_1
  | `DmSJRP23_k4_2
  | `DmSJRP23_k9_1
  | `DmSJRP23_k9_2
  | `DmSJRP23_wce_1
  | `DmSJRP23_wce_2
  | `DmSJRP7_k4_1
  | `DmSJRP7_k4_2
  | `DmSJRP7_k9_1
  | `DmSJRP7_k9_2
  | `DmSJRP7_wce_1
  | `DmSJRP7_wce_2
]

let mapped_reads x =
  Bowtie2.bowtie2
    (Species.bowtie2_index (Sample.species x))
    (SE_or_PE.map (Sample.fastq_gz x) ~f:(fun x -> [ x ]))

let mapped_reads_bam x =
  Samtools.indexed_bam_of_sam (mapped_reads x)

let signal x =
  Deeptools.(bamcoverage ~binsize:10 ~normalizeUsing:`RPKM bigwig) (mapped_reads_bam x)

(* let%pworkflow genome_slices ~fa ~gtf =
 *   let genes =
 *     Gzt.Gtf.load [%path gtf]
 *     |> Gzt.Gtf.Annotation.of_items
 *     |> Gzt.Gtf.Annotation.genes
 *     |> fst
 *   in
 *   let genes_by_chr =
 *     let module Acc = Biocaml_unix.Accu.Relation in
 *     let r = Acc.create () in
 *     String.Table.iteri genes ~f:(fun ~key:id ~data:g ->
 *         Acc.add r g.chr (id, g)
 *       ) ;
 *     Acc.to_alist r
 *     |> List.Assoc.map ~f:(fun genes ->
 *         List.map genes ~f:(fun (id, g) ->
 *             Gzt.Gene.loc g, id, g
 *           )
 *         |> List.sort ~compare:(fun (l1, _, _) (l2,_,_) ->
 *             Gzt.GLoc.compare l1 l2
 *           )
 *       )
 *   in
 *   match Gzt.Fasta.from_file [%path fa] with
 *   | Ok (_, items) ->
 *     List.concat_map items ~f:(fun it ->
 *         match List.Assoc.find genes_by_chr it.description with
 *         | None -> failwithf "genome_slices: unknown chr %s" it.description ()
 *         | Some genes ->
 *           let rec slices acc pos last_id = function
 *             | [] ->
 *               let last_slice = Fasta.{
 *                   description = "downstream_" ^ last_id ;
 *                   sequence = String.sub it.sequence ~pos ~len:(String.length it.sequence - pos) ;
 *                 }
 *               in
 *               last_slice :: acc
 *             | g :: rest ->
 *               let intergenic_slice 
 *       )
 *   | Error _ -> assert false *)
