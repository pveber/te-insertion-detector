open Bistro_utils

let make () =
  let te_library =
    Species.load_te_library "data/consensus_tousET_tousDroso.txt"
  in
  Repo.[
    item ["annotated_insertions" ; "dmel.tsv" ] (Dna_sample.annotated_insertions `Dmel te_library) ;
    item ["annotated_insertions" ; "dsim.tsv" ] (Dna_sample.annotated_insertions `Dsim te_library) ;
  ]
