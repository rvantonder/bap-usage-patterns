#use "topfind";;
#require "bap.top";;
open Core_kernel.Std
open Bap.Std
open Or_error

let main () =
  Project.from_file Sys.argv.(1) >>= fun project ->

  (* [ex2-2.ml] *)
  let module G = Graphlib.Int.Unit in
  let g = Graphlib.create (module G) ~edges:[0,1,();1,1,()] () in
  Graphlib.to_dot (module G) ~filename:"graph.dot" g;

  return ()

let () =
  try main ()
      |> function
      | Ok o -> ()
      | Error e -> Format.printf "BAP error: %s\n" @@ Error.to_string_hum e
  with
  | Invalid_argument _ ->
    Format.printf "Please specify a file on the command line\n"
