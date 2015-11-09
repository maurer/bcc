open Core_kernel.Std
open Bap.Std

let info =
  let doc = "Extract constants, distances, and debug information for bcc"; in
  let man = [ `S "BUGS"; `P "File bug reports on the repo at https://github.com/maurer/bcc";] in
  Cmdliner.Term.info "bcc-dump" ~version:"0" ~doc ~man

let output =
  let doc = "Specifies the file to dump to." in
  Cmdliner.Arg.(value & opt string "dump" & info ["o"; "out"] ~docv:"OUT" ~doc)

let interesting const =
  if Word.(is_zero (const ++ 1)) then false else
  match Word.to_int64 const with
    | Ok(k) ->
        (match k with
          (* Common offsets *)
          | 0L | 1L | 2L | 4L | 8L
          (* Negative 1 in many widths. *)
          | 0xFFL | 0xFFFFL | 0xFFFFFFFFL
          (* Normal control codes *)
          | 0x10L -> false
          (* Printable ascii *)
          | x when (x >= 0x20L) && (x <= 0x7eL) -> false
          | _ -> true)
    | Error(_) -> true

let extract_const def =
  let extractor = object(self)
    inherit [word Sequence.t] Bil.visitor
    method! visit_int k seq =
      if interesting k
        then Sequence.shift_right seq k
        else seq
  end in
  let consts = Exp.fold extractor ~init:Sequence.empty (Def.rhs def) in
  if Sequence.is_empty consts
    then None
    else Some((Term.tid def, consts))

let dump_prog (prog : program term) (oc : Out_channel.t) : unit =
  let fmt = Format.formatter_of_out_channel oc in
  let consts = Sequence.concat_map (Term.to_sequence sub_t prog) ~f:(fun sub ->
    Sequence.filter_map
      (Sequence.concat_map (Term.to_sequence blk_t sub)
                           ~f:(Term.to_sequence def_t))
      ~f:extract_const) in
  Sequence.iter consts ~f:(fun (tid, consts) ->
    Format.fprintf fmt "%a: %a\n" Tid.pp tid Word.pp_seq consts)

let dump (proj : Project.t) (out_name : string) : unit =
  Out_channel.with_file out_name ~f:(dump_prog (Project.program proj))

;;

Project.register_pass_with_args "bcc_dump" (fun argv proj ->
  match Cmdliner.Term.eval ~argv (Cmdliner.Term.(const (dump proj) $ output), info) with
    | `Error _ -> exit 1
    | `Ok () -> proj
    | _ -> proj)
