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
  Graphlib.to_dot (module Graphlib.Callgraph) ~filename:out_name cg

;;

Project.register_pass_with_args' "dump_callgraph" (fun argv proj ->
  match Cmdliner.Term.eval ~argv (Cmdliner.Term.(const (dump proj) $ output), info) with
    | `Error _ -> exit 1
    | _ -> ())
