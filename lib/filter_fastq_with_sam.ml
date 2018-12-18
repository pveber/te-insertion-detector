open Core
open Biocaml_ez
open CFStream

let left_side x =
  match String.lsplit2 ~on:' ' x with
  | Some (x, _) -> x
  | None -> x

let chop s =
  String.sub s ~pos:0 ~len:(String.length s - 2)

let normalize_id x =
  let x = left_side x in
  if String.is_suffix ~suffix:"/1" x && String.is_suffix ~suffix:"/2" x then
    chop x
  else x

let idset_of_sam sam_path =
  In_channel.with_file sam_path ~f:(fun ic ->
      let set = String.Hash_set.create () in
      Sam.read ic |> fun (_, items) ->
      Stream.iter items ~f:(fun al ->
          match al.Sam.qname, al.Sam.mapq with
          | Some qname, Some mapq when mapq > 30 ->
            let qname = normalize_id qname in
            Hash_set.add set qname
          | _ -> ()
        ) ;
      set
    )

let filter_fastq pred set ic oc =
  Fastq.read ic
  |> Stream.filter ~f:(fun it ->
      let name = normalize_id it.Biocaml_unix.Fastq.name in
      pred (Hash_set.mem set) name
    )
  |> Fastq.write oc

let main ~invert ~sam ~fq ~output () =
  let idset = idset_of_sam sam in
  let pred = if invert then Fn.non else Fn.id in
  Out_channel.with_file output ~f:(fun oc ->
      In_channel.with_file fq ~f:(fun ic ->
          filter_fastq pred idset ic oc
        )
    )

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Filter a FASTQ keeping only those having one fragment aligned in SAM"
    [%map_open
      let invert = flag "--invert" no_arg            ~doc:" Invert output (reads that are not in SAM)"
      and sam = flag "--sam" (required file)      ~doc:"PATH Aligned reads (SAM)"
      and fq = flag "--fastq" (required file)    ~doc:"PATH Unaligned reads (FASTQ)"
      and output = flag "--output" (required string) ~doc:"PATH Path where to write FASTQ output"
      in
      main ~invert ~sam ~fq ~output]
