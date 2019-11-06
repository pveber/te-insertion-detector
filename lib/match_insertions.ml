open Core
open Pipes_unix

let ok_exn = function
  | Stdlib.Ok x -> x
  | Error (`Msg x) -> failwith x

module Lines = Biocaml_base.Lines
module Macs2 = Biocaml_base.Macs2

(* Representation of the bipartite graph showing overlapping regions
   between the two sets we want to compare *)
module V = struct
  type t =
    | Left of Macs2.Xls.record
    | Right of Macs2.Xls.record

  let equal = ( = )
  let hash = Hashtbl.hash
  let compare = compare

  let to_peak = function
    | Left p -> p
    | Right q -> q

  let id_of_peak { Macs2.Xls.chr ; start ; end_ ; _ } =
    sprintf "%s:%d-%d" chr start end_

  let tag = function
    | Left _ -> "L"
    | Right _ -> "R"

  let id x =
    sprintf "%s:%s" (tag x) (id_of_peak (to_peak x))
end

module G = struct
  include Graph.Persistent.Graph.Concrete(V)

  let edges g =
    let f u v accu = (u, v) :: accu in
    fold_edges f g []

  let singletons g =
    let f u accu =
      if out_degree g u = 0 then
        u :: accu
      else
        accu
    in
    fold_vertex f g []

end

module Neato = Graph.Graphviz.Neato(
  struct
    open Graph.Graphviz
    let vertex_name x = sprintf {|"%s"|} (V.id x)
    let graph_attributes _ = [`Orientation `Portrait]
    let vertex_attributes _ = []
    let default_vertex_attributes _ = []
    let edge_attributes _ = []
    let default_edge_attributes _ = []
    let get_subgraph = function
      | V.Left _ -> Some { NeatoAttributes.sg_name = "Left" ; sg_attributes = [] ; sg_parent = None }
      | V.Right _ -> Some { NeatoAttributes.sg_name = "Right" ; sg_attributes = [] ; sg_parent = None }

    include G
  end
  )



(* MACS2 outputs spurious peaks that are several MB long. Here we keep
   only peaks that are no longer than 2 kb *)
let filter_peak { Macs2.Xls.length ; _ } =
  length < 2000

let line_reader = Pipe.loop Lines.Parser.step Lines.Parser.initial_state

let read_peaks macs_xls =
  let open Pipe in
  run (
    from_file macs_xls
    $$ line_reader
    $$ map Macs2.Xls.parse
    $$ all ()
  )
  |> ok_exn
  |> List.filter_map ~f:(function
      | `Record p when filter_peak p -> Some p
      | _ -> None
    )

let peak_overlap p q =
  let open Macs2.Xls in
  min p.end_ q.end_ - max p.start q.start + 1

let peak_distance p q =
  - (min 0 (peak_overlap p q))

let matching_graph peaks1 peaks2 =
  let open V in
  let g = List.fold peaks1 ~init:G.empty ~f:(fun accu p ->
      G.add_vertex accu (Left p)
    ) in
  let g = List.fold peaks2 ~init:g ~f:(fun accu p ->
      G.add_vertex accu (Right p)
    )
  in
  List.fold peaks1 ~init:g ~f:(fun accu p ->
      List.fold peaks2 ~init:accu ~f:(fun accu q ->
          if peak_distance p q < 500 then
            G.add_edge accu (Left p) (Right q)
          else
            accu
        )
    )

type match_stats = {
  left_only : int ;
  left_with_match : int ;
  right_only : int ;
  right_with_match : int ;
}
[@@deriving sexp]

let graph_counts g =
  let open V in
  let f v stats =
    let in_degree = G.in_degree g v in
    let out_degree = G.out_degree g v in
    match v, in_degree, out_degree with
    | Left _, _, 0 -> { stats with left_only = stats.left_only + 1 }
    | Left _, _, _ -> { stats with left_with_match = stats.left_with_match + 1 }
    | Right _, 0, _ -> { stats with right_only = stats.right_only + 1 }
    | Right _, _, _ -> { stats with right_with_match = stats.right_with_match + 1 }
  in
  G.fold_vertex f g { left_only = 0 ; left_with_match = 0 ; right_only = 0 ; right_with_match = 0 }

let compare_peaks p q =
  let open Macs2.Xls in
  compare (p.chr, p.start, p.end_) (q.chr, q.start, q.end_)

let compare_edges (p1, q1) (p2, q2) =
  match compare_peaks (V.to_peak p1) (V.to_peak p2) with
  | 0 -> compare_peaks (V.to_peak q1) (V.to_peak q2)
  | d -> d

let ordered_edges g =
  List.sort ~compare:compare_edges (G.edges g)

let compare_alignment_rows r1 r2 =
  match r1, r2 with
  | `Edge e1, `Edge e2 ->
    compare_edges e1 e2

  | `Singleton s1, `Singleton s2 ->
    compare_peaks (V.to_peak s1) (V.to_peak s2)

  | `Edge (v1, v2), `Singleton s ->
    if
      min
        (compare_peaks (V.to_peak s) (V.to_peak v1))
        (compare_peaks (V.to_peak s) (V.to_peak v2))
      = -1
    then 1 else -1
  | `Singleton s, `Edge (v1, v2) ->
    if
      min
        (compare_peaks (V.to_peak s) (V.to_peak v1))
        (compare_peaks (V.to_peak s) (V.to_peak v2))
      = -1
    then -1 else 1

let alignment g =
  let edges = List.map (G.edges g) ~f:(fun x -> `Edge x) in
  let singletons = List.map (G.singletons g) ~f:(fun x -> `Singleton x) in
  let rows = edges @ singletons in
  List.sort ~compare:compare_alignment_rows rows

let output_ordered_edges oc xs =
  let open Macs2.Xls in
  List.iter xs ~f:(function
      | (V.Left p, V.Right q)
      | (V.Right q, V.Left p) ->
        fprintf oc "%s\t%d\t%d%s\t%d\t%d\n" p.chr p.start p.end_ q.chr q.start q.end_
      | _ -> assert false
    )

let output_alignment oc xs =
  let open Macs2.Xls in
  List.iter xs ~f:(function
      | `Edge (V.Left p, V.Right q)
      | `Edge (V.Right q, V.Left p) ->
        fprintf oc "%s\t%d\t%d\t%f\t%s\t%d\t%d\t%f\n" p.chr p.start p.end_ p.log10qvalue q.chr q.start q.end_ q.log10qvalue
      | `Singleton (V.Left p) ->
        fprintf oc "%s\t%d\t%d\t%f\t.\t.\t.\t.\n" p.chr p.start p.end_ p.log10qvalue
      | `Singleton (V.Right q) ->
        fprintf oc ".\t.\t.\t.\t%s\t%d\t%d\t%f\n" q.chr q.start q.end_ q.log10qvalue
      | _ -> assert false
    )

let generate_dot_repr dir g =
  let dot = Filename.concat dir "match_graph.dot"
  and pdf = Filename.concat dir "match_graph.pdf" in
  Out_channel.with_file dot ~f:(fun oc -> Neato.output_graph oc g) ;
  ignore @@ Sys.command (sprintf "dot -Tpdf %s > %s" dot pdf)

let main macs_xls1 macs_xls2 out_path () =
  let peaks1 = read_peaks macs_xls1 in
  let peaks2 = read_peaks macs_xls2 in
  let g = matching_graph peaks1 peaks2 in
  let counts = graph_counts g in
  Unix.mkdir_p out_path ;
  Out_channel.with_file
    (Filename.concat out_path "stats")
    ~f:(fun oc -> Sexp.output_hum oc (sexp_of_match_stats counts)) ;
  Out_channel.with_file
    (Filename.concat out_path "alignment")
    ~f:(fun oc -> output_alignment oc (alignment g)) ;
  generate_dot_repr out_path g

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Match detected insertions"
    [%map_open
      let bed1 = anon ("BED1" %: Filename.arg_type)
      and bed2 = anon ("BED2" %: Filename.arg_type)
      and output = flag "--output" (required Filename.arg_type) ~doc:"PATH Path where to write the matching" in
      main bed1 bed2 output]
