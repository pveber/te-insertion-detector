open Core
open Biocaml_ez
open CFStream

let chop s =
  String.sub s ~pos:0 ~len:(String.length s - 2)

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

let filter_fastq pred set ic oc =
  Fastq.read ic
  |> Stream.filter ~f:(fun it ->
      let name = chop it.Biocaml_unix.Fastq.name in
      pred (Hash_set.mem set) name
    )
  |> Fastq.write oc

let main invert sam_path fq_path out_path () =
  let idset = idset_of_sam sam_path in
  let pred = if invert then Fn.non else Fn.id in
  Out_channel.with_file out_path ~f:(fun oc ->
      In_channel.with_file fq_path ~f:(fun ic ->
          filter_fastq pred idset ic oc
        )
    )

let command =
  let spec =
    let open Command.Spec in
    empty
    +> flag "--invert" no_arg            ~doc:" Invert output (reads that are not in SAM)"
    +> flag "--sam" (required file)      ~doc:"PATH Aligned reads (SAM)"
    +> flag "--fastq" (required file)    ~doc:"PATH Unaligned reads (FASTQ)"
    +> flag "--output" (required string) ~doc:"PATH Path where to write FASTQ output"
  in
  Command.basic
    ~summary:"Filter a FASTQ keeping only those having one fragment aligned in SAM"
    spec
    main
