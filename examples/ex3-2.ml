#use "topfind";;
#require "bap.top";;
open Core_kernel.Std
open Bap.Std
open Or_error

let main () =
  Project.from_file Sys.argv.(1) >>= fun project ->

  (* [ex3-2.ml] *)
  Project.memory project |> Memmap.to_sequence |> Seq.iter ~f:(fun (mem,x) ->
      Format.printf "%s(%a)@.%a@." (Value.tagname x) Value.pp x Memory.pp mem);

  return ()

let () =
  try main ()
      |> function
      | Ok o -> ()
      | Error e -> Format.printf "BAP error: %s\n" @@ Error.to_string_hum e
  with
  | Invalid_argument _ ->
    Format.printf "Please specify a file on the command line\n"
