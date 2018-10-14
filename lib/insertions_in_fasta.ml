open Core
open Biocaml_ez
open CFStream

let chr_size fasta_path =
  Fasta.with_file fasta_path ~f:(fun _ xs ->
      Stream.map xs ~f:(fun it ->
          it.Fasta.description,
          String.length it.Fasta.sequence
        )
      |> Stream.to_list
    )

let random_positions chr_size =
  let foreach (chr, length) =
    let n =
      float length /. 1e6
      |> Float.round_nearest
      |> Float.to_int
    in
    chr, List.init n ~f:(fun _ -> Random.int length)
  in
  List.map chr_size ~f:foreach

let perform_insertions_aux te_seq inserts it =
  let positions = List.Assoc.find_exn ~equal:String.( = ) inserts it.Fasta.description in
  let sequence =
    List.fold_right positions ~init:it.Fasta.sequence ~f:(fun p seq ->
        String.sub seq ~pos:0 ~len:p
        ^
        te_seq
        ^
        String.(sub seq ~pos:p ~len:(length seq - p))
        )
  in
  it.Fasta.description, sequence

let perform_insertions te_seq inserts fa_path out_path =
  Out_channel.with_file out_path ~f:(fun oc ->
      Fasta.with_file fa_path ~f:(fun _ xs ->
          Stream.filter xs ~f:(fun it -> (* remove sequence of less than 1MB *)
              String.length it.Fasta.sequence > 1_000_000
            )
          |> Stream.map  ~f:(perform_insertions_aux te_seq inserts)
          |> Stream.iter ~f:(fun (description, sequence) ->
              fprintf oc ">%s\n%s\n" description sequence
            )
        )
    )

let write_insert_positions inserts out_path =
  Out_channel.with_file out_path ~f:(fun oc ->
      List.iter inserts ~f:(fun (chr, pos) ->
          List.iter pos ~f:(fun p ->
              fprintf oc "%s\t%d\n" chr p
            )
        )
    )

let main ~te:te_path ~genome:genome_path ~output:out_path () =
  let te_seq =
    Fasta.(
      let fmt = { default_fmt with allow_empty_lines = true } in
      with_file ~fmt te_path ~f:(fun _ xs ->
          (Stream.next_exn xs).sequence
        )
    )
  in
  let chr_size = chr_size genome_path in
  let inserts = random_positions chr_size in
  Unix.mkdir_p out_path ;
  perform_insertions te_seq inserts genome_path (Filename.concat out_path "genome.fa") ;
  write_insert_positions inserts (Filename.concat out_path "inserts.bed")

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Add insertions of a TE in a genome"
    [%map_open
      let te = flag "--te" (required file) ~doc:"PATH Transposable element (FASTA)"
      and genome = flag "--genome" (required file) ~doc:"PATH Genome file (FASTA)"
      and output = flag "--output" (required file) ~doc:"PATH Path where to write the modified genome"
      in
      main ~te ~genome ~output]
