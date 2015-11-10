open Core_kernel.Std
open Bin_prot.Std
open Bap.Std

let info =
  let doc = "Dump callgraph in dot format"; in
  let man = [ `S "BUGS"; `P "File bug reports on the repo at https://github.com/maurer/bcc";] in
  Cmdliner.Term.info "bcc-dump" ~version:"0" ~doc ~man

let output =
  let doc = "Specifies the file to dump to." in
  Cmdliner.Arg.(value & opt string "cg.dot" & info ["o"; "out"] ~docv:"OUT" ~doc)

let dump (proj : Project.t) (out_name : string) : unit =
  let prog = Project.program proj in
  let cg = Program.to_graph prog in
  let mem = Project.memory proj in
 let string_of_node tid =
    let vx (o : sub term option) : sub term = Option.value_exn o in
    let vx' (o : word option) : word = Option.value_exn o in
    let name = Tid.name tid in
    let node_name = String.sub name 1 ((String.length name) - 1) in
    let sub = Program.lookup sub_t prog tid |> vx in
    let addr = Term.get_attr sub subroutine_addr |> vx' in
    Memmap.lookup mem addr |> Sequence.find ~f:(fun (_, v) -> Value.is Image.symbol v) |> Option.map ~f:(fun (_,v) -> Value.get_exn Image.symbol v) |> Option.value ~default:node_name in
  Graphlib.to_dot (module Graphlib.Callgraph) ~filename:out_name ~string_of_node cg

;;

Project.register_pass_with_args' "dump_callgraph" (fun argv proj ->
  match Cmdliner.Term.eval ~argv (Cmdliner.Term.(const (dump proj) $ output), info) with
    | `Error _ -> exit 1
    | _ -> ())
