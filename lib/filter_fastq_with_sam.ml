open Core.Std
open Biocaml_ez.Std
open CFStream

let chop s =
  String.sub s 0 (String.length s - 2)

let idset_of_sam sam_path =
  In_channel.with_file sam_path ~f:(fun ic ->
      let set = String.Hash_set.create () in
      Sam.read ic |> fun (_, items) ->
      Stream.iter items ~f:(fun al ->
          match al.Sam.qname, al.Sam.mapq with
          | Some qname, Some mapq when mapq > 30 ->
            let qname = chop qname in
            Hash_set.add set qname
          | _ -> ()
        ) ;
      set
    )

let filter_fastq set ic oc =
  Fastq.read ic
  |> Stream.filter ~f:(fun it ->
      let name = chop it.Biocaml_unix.Fastq.name in
      Hash_set.mem set name
    )
  |> Fastq.write oc

let main sam_path fq_path out_path () =
  let idset = idset_of_sam sam_path in
  Out_channel.with_file out_path ~f:(fun oc ->
      In_channel.with_file fq_path ~f:(fun ic ->
          filter_fastq idset ic oc
        )
    )

let command =
  let spec =
    let open Command.Spec in
    empty
    +> flag "--sam" (required file)      ~doc:"PATH Aligned reads (SAM)"
    +> flag "--fastq" (required file)    ~doc:"PATH Unaligned reads (FASTQ)"
    +> flag "--output" (required string) ~doc:"PATH Path where to write FASTQ output"
  in
  Command.basic
    ~summary:"Filter a FASTQ keeping only those having one fragment aligned in SAM"
    spec
    main
