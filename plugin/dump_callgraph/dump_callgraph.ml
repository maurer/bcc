open Core_kernel.Std
open Graphlib.Std
open Bin_prot.Std
open Bap.Std

include Self()

let info =
  let doc = "Dump callgraph in dot format"; in
  let man = [ `S "BUGS"; `P "File bug reports on the repo at https://github.com/maurer/bcc";] in
  Cmdliner.Term.info "bcc-dump" ~version:"0" ~doc ~man

let output =
  let doc = "Specifies the file to dump to." in
  Cmdliner.Arg.(value & opt string "cg.graphml" & info ["o"; "out"] ~docv:"OUT" ~doc)

let uid h find add =
  let n = ref 0 in
  fun x -> match find h x with
    | Some(v) -> v
    | None -> (incr n; add h ~key:x ~data:!n; !n)

module OG = Graphlib.To_ocamlgraph(Graphs.Callgraph)
let dump (proj : Project.t) (out_name : string) : unit =
  let prog = Project.program proj in
  let cg = Program.to_graph prog in
  let mem = Project.memory proj in
  let module OL = struct
    let vertex_properties : (string * string * string option) list = ["func_name","string",None]
    let edge_properties : (string * string * string option) list = []
    let map_edge (e : OG.E.t) : (string * string) list = []
    module Vhash = Graphs.Callgraph.Node.Table
    let vertex_uid = uid (Vhash.create ()) Vhash.find Vhash.add
    module Ehash = Graphs.Callgraph.Edge.Table
    let edge_uid = uid (Ehash.create ()) Ehash.find Ehash.add
    let map_vertex tid =
      let vx (o : sub term option) : sub term = Option.value_exn o in
      let vx' (o : word option) : word = Option.value_exn o in
      let name = Tid.name tid in
      let node_name = String.sub name 1 ((String.length name) - 1) in
      let sub = Program.lookup sub_t prog tid |> vx in
      let addr = Term.get_attr sub subroutine_addr |> vx' in
      ["func_name",(Memmap.lookup mem addr |> Sequence.find ~f:(fun (_, v) -> Value.is Image.symbol v) |> Option.map ~f:(fun (_,v) -> Value.get_exn Image.symbol v) |> Option.value ~default:node_name)]
  end in
  let module OGP = Graph.Graphml.Print(OG)(OL) in

  Out_channel.with_file out_name ~f:(fun oc ->
      let fmt = Format.formatter_of_out_channel oc in
      OGP.print fmt cg
    )

;;

Project.register_pass' (fun proj ->
    match Cmdliner.Term.eval ~argv (Cmdliner.Term.(const (dump proj) $ output), info) with
    | `Error _ -> exit 1
    | _ -> ())
