#use "topfind";;
#require "bap.top";;
open Core_kernel.Std
open Bap.Std
open Or_error

let main () =
  Project.from_file Sys.argv.(1) >>= fun project ->

  (* [ex1-3.ml] *)
  let program = Project.program project in
  Option.(Term.find sub_t program Tid.(!"@main") >>= fun main_sub ->
          let string_of_node n = sprintf "\"\\%s\""
            @@ Tid.name @@ Graphlib.Tid.Tid.Node.label n in
          let string_of_edge e = Tid.name @@ Graphlib.Tid.Tid.Edge.label e in
          Graphlib.to_dot ~string_of_node ~string_of_edge
            ~filename:"main_with_tids.dot" (module Graphlib.Tid.Tid) @@
          Sub.to_graph main_sub; Some main_sub)
  |> Pervasives.ignore;

  return ()

let () =
  try main ()
      |> function
      | Ok o -> ()
      | Error e -> Format.printf "BAP error: %s\n" @@ Error.to_string_hum e
  with
  | Invalid_argument _ ->
    Format.printf "Please specify a file on the command line\n"
