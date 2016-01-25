#use "topfind";;
#require "bap.top";;
open Core_kernel.Std
open Bap.Std
open Or_error
open Format

let main () =
  Project.from_file Sys.argv.(1) >>= fun project ->

  (* [ex3-2.ml] *)
  let find_section_by_name name =
    let memory = Project.memory project in
    Memmap.to_sequence memory |> Seq.find_map ~f:(fun (m,x) ->
        Option.(Value.get Image.section x >>= fun n ->
                Option.some_if (n = name) m)) in

  let mem_from_addr addr mem =
    match Memory.view ~word_size:`r8 ~from:addr mem with
    | Ok r -> r
    | Error e -> failwith @@ sprintf "Failure: %s\n" @@ Error.to_string_hum e in

  let read_string mem =
    let (!) = Char.to_string in
    Memory.foldi ~word_size:`r8 mem ~init:(false,"")
      ~f:(fun addr word (set,acc) ->
          let char = Word.to_chars word LittleEndian |> Seq.hd_exn in
          match set,char with
          | (false,'\x00') -> (true,acc)
          | (false,c) -> (false,acc^(!c))
          | (true,c) -> (true,acc)) |> snd in

  let addr = Addr.of_string "0x400644:64" in

  let result =
    let open Option in
    find_section_by_name ".rodata" >>= fun mem ->
    Option.some_if (Memory.contains mem addr) (
      let mem' = mem_from_addr addr mem in
      read_string mem') in
  (match result with
   | Some s -> printf "%s\n%!" s
   | None -> failwith "No string could be found");

  return ()

let () =
  try main ()
      |> function
      | Ok o -> ()
      | Error e -> Format.printf "BAP error: %s\n" @@ Error.to_string_hum e
  with
  | Invalid_argument _ ->
    Format.printf "Please specify a file on the command line\n"
