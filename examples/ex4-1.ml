#use "topfind";;
#require "bap.top";;
open Core_kernel.Std
open Bap.Std
open Or_error

let main () =
  Project.from_file Sys.argv.(1) >>= fun project ->

  (* [ex4-1.ml] *)
  let program = Project.program project in
  let main_sub = Term.find sub_t program Tid.(!"@main") in
  (match main_sub with
   | Some sub -> Format.printf "%s\n" (Sub.to_string sub)
   | None -> Format.printf "Could not find the subroutine\n");

  return ()

let () =
  try main ()
      |> function
      | Ok o -> ()
      | Error e -> Format.printf "BAP error: %s\n" @@ Error.to_string_hum e
  with
  | Invalid_argument _ ->
    Format.printf "Please specify a file on the command line\n"
