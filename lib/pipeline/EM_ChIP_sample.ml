open Core_kernel
open Bistro_bioinfo

type t = [
  | `DmGoth6_k4_1
  | `DmGoth6_k4_2
  | `DmGoth6_k9_1
  | `DmGoth6_k9_2
  | `DmGoth10_k4_1
  | `DmGoth10_k4_2
  | `DmGoth10_k9_1
  | `DmGoth10_k9_2
  | `DmSJRP23_k4_1
  | `DmSJRP23_k4_2
  | `DmSJRP23_k9_1
  | `DmSJRP23_k9_2
  | `DmSJRP7_k4_1
  | `DmSJRP7_k4_2
  | `DmSJRP7_k9_1
  | `DmSJRP7_k9_2
]
[@@deriving enumerate]

let subset ?species () =
  List.filter all ~f:(fun s ->
      Option.value_map species ~default:true ~f:(fun spe -> Sample.species s = spe)
    )

let macs_peaks s =
  Macs2.(callpeak_broad sam ~qvalue:0.01 ~call_summits:true [ Genomic_sample.mapped_reads s ])
