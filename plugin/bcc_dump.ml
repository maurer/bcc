open Core_kernel.Std
open Bap.Std
open Cmdliner

let info =
  let doc = "Extract constants, distances, and debug information for bcc"; in
  let man = [ `S "BUGS"; `P "File bug reports on the repo at https://github.com/maurer/bcc";] in
  Term.info "bcc-dump" ~version:"0" ~doc ~man

let output =
  let doc = "Specifies the file to dump to." in
  Arg.(value & opt string "dump" & info ["o"; "out"] ~docv:"OUT" ~doc)

let dump proj out_name =
  let oc = Out_channel.create out_name in
  Printf.fprintf oc "File: %s\n%!" (Option.value_exn (Project.get proj filename));
  Out_channel.close oc;
  proj

;;

Project.register_pass_with_args "bcc_dump" (fun argv proj ->
  match Term.eval ~argv (Term.(const (dump proj) $ output), info) with
    | `Error _ -> exit 1
    | `Ok proj' -> proj'
    | _ -> proj)
