#use "topfind";;
#require "bap.top";;
open Core_kernel.Std
open Bap.Std
open Or_error

let main () =
  Project.from_file Sys.argv.(1) >>= fun project ->

    (* [ex1-1.ml] *)
    let callgraph = Program.to_graph @@ Project.program project in
    let string_of_node n =
      sprintf "%S" @@ Tid.name @@ Graphlib.Callgraph.Node.label n in
    Graphlib.to_dot (module Graphlib.Callgraph)
      ~string_of_node ~filename:"callgraph.dot" callgraph;

  return ()

let () =
  try main ()
      |> function
      | Ok o -> ()
      | Error e -> Format.printf "BAP error: %s\n" @@ Error.to_string_hum e
  with
  | Invalid_argument _ ->
    Format.printf "Please specify a file on the command line\n"
