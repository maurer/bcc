open Core_kernel.Std
open Bap.Std

let ascii_string = Value.Tag.register (module String)
  ~name:"ascii_string"
  ~uuid:"2f9b7b0c-688f-4c67-b978-c3dd76ce8963"

let interesting str = (String.length str) > 4

let strings_segment (proj : Project.t) (seg : mem) : Project.t =
  let cur_string = ref [] in
  let cur_addr = ref (Memory.min_addr seg) in
  let base_addr = ref !cur_addr in
  let proj' = ref proj in
  while Word.(!cur_addr <= (Memory.max_addr seg)) do
    let k = Memory.get seg ~addr:(!cur_addr) |> Or_error.ok_exn |> Word.to_int |> Or_error.ok_exn |> Char.of_int_exn in
    if Char.is_print k then (
      cur_string := k :: !cur_string;
      base_addr := Word.succ !base_addr
    ) else if (Char.to_int k) = 0 then (
      let str = List.rev !cur_string |> String.of_char_list in
      proj' := (if interesting str then
        Project.tag_memory !proj' (Memory.range seg !base_addr !cur_addr |> Or_error.ok_exn) ascii_string str
      else
        !proj');
      base_addr  := Word.succ !cur_addr;
      cur_string := []
    ) else (
      base_addr  := Word.succ !cur_addr;
      cur_string := []
    );
    cur_addr := Word.succ !cur_addr;
  done;
  !proj'

let strings (proj : Project.t) : Project.t =
  let segments = Project.memory proj |> Memmap.filter ~f:(Value.is Image.section) |> Memmap.to_sequence |> Sequence.map ~f:fst in
  Sequence.fold segments ~f:strings_segment ~init:proj

;;

Project.register_pass "strings" strings
