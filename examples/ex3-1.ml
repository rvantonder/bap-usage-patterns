#use "topfind";;
#require "bap.top";;
open Core_kernel.Std
open Bap.Std
open Or_error

let main () =
  Project.from_file Sys.argv.(1) >>= fun project ->

  (* [ex3-1.ml] *)
  let find_section_by_name name =
    let memory = Project.memory project in
    Memmap.to_sequence memory |> Seq.find_map ~f:(fun (m,x) ->
        Option.(Value.get Image.section x >>= fun n ->
                Option.some_if (n = name) m)) in
  (match find_section_by_name ".rodata" with
   | Some mem -> Format.printf "%a" Memory.pp mem
   | None -> Format.printf "No memory for this section\n");

  return ()

let () =
  try main ()
      |> function
      | Ok o -> ()
      | Error e -> Format.printf "BAP error: %s\n" @@ Error.to_string_hum e
  with
  | Invalid_argument _ ->
    Format.printf "Please specify a file on the command line\n"
