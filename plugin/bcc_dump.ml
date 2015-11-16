open Core_kernel.Std
open Bin_prot.Std
open Bap.Std

let info =
  let doc = "Extract constants, distances, and debug information for bcc"; in
  let man = [ `S "BUGS"; `P "File bug reports on the repo at https://github.com/maurer/bcc";] in
  Cmdliner.Term.info "bcc-dump" ~version:"0" ~doc ~man

let output =
  let doc = "Specifies the file to dump to." in
  Cmdliner.Arg.(value & opt string "dump" & info ["o"; "out"] ~docv:"OUT" ~doc)

(* Predicate for whether a constant should be considered *)
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

let extract_const base def =
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
    else Some((base, Term.tid def, consts))

module Const = struct
  module T = struct
    let module_name = Some("Const")
    type t =
      | Word of Word.t
      | String of string with compare, sexp, bin_io
    let pp fmt k = match k with
      | Word w   -> Word.pp fmt w
      | String s -> (Format.pp_print_char fmt '"';
                     String.pp fmt (String.escaped s);
                     Format.pp_print_char fmt '"')
    let hash k = match k with
      | Word w   -> Word.hash w
      | String s -> String.hash s
  end
  include T
  include Regular.Make(T)
end

type const = Const.t


let string_tag (_, v) = (Value.tagname v) = "ascii_string"

let first_result rs = Option.map (Sequence.find rs ~f:Result.is_ok) ~f:Result.ok |> Option.join

let rec normalize (mem : value memmap) (k : word) : const =
  (* If this constant is not mapped, just use the constant *)
  if not (Memmap.contains mem k) then Const.Word(k) else
  (* Check if there is a string at the pointer *)
  match Memmap.lookup mem k |> Sequence.find ~f:string_tag with
    (* Extract the tag contents via jank *)
    | Some (_, v) -> Const.String(Value.to_string v)
    | None -> (match Memmap.lookup mem k |> Sequence.find ~f:(fun (_, v) -> Value.is Image.section v) with
      | Some (m, _) -> (
          (* TODO add `r64 conditionally to the front of the list on 64-bit systems *)
          match Sequence.map (Sequence.of_list Size.([`r32; `r16; `r8])) ~f:(fun scale -> Memory.get m ~scale ~addr:k) |> first_result with
            | Some v -> normalize mem v
            | _ -> Const.Word (k)
      )
      | None -> Const.Word (k))

let dump (proj : Project.t) (out_name : string) : unit =
  Out_channel.with_file out_name ~f:(fun oc ->
    let fmt  = Format.formatter_of_out_channel oc in
    let prog = Project.program proj in
    let mem  = Project.memory proj in
    let consts = Sequence.concat_map (Term.to_sequence sub_t prog) ~f:(fun sub ->
      Sequence.filter_map
        (Sequence.concat_map (Term.to_sequence blk_t sub)
                             ~f:(Term.to_sequence def_t))
        ~f:(extract_const (Tid.name (Term.tid sub)))) in
    let normed = Sequence.map consts ~f:(fun (base, tid, ks) ->
      (base, tid, Sequence.map ks (normalize mem))) in
    Sequence.iter normed ~f:(fun (base, tid, consts) -> (
      Format.fprintf fmt "%s:%a:[: " base Tid.pp tid;
      Sequence.iter ~f:(fun k -> Format.fprintf fmt "%a; " Const.pp k) consts;
      Format.fprintf fmt ":]@.")))

;;

Project.register_pass_with_args' "bcc_dump" (fun argv proj ->
  match Cmdliner.Term.eval ~argv (Cmdliner.Term.(const (dump proj) $ output), info) with
    | `Error _ -> exit 1
    | _ -> ())
