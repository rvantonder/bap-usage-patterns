#use "topfind";;
#require "bap.top";;
open Core_kernel.Std
open Bap.Std
open Or_error

let main () =
  Project.from_file Sys.argv.(1) >>= fun project ->

  (* [ex2-1.ml] *)
  let callgraph = Program.to_graph @@ Project.program project in
  let scc_partition = Graphlib.strong_components
      (module Graphlib.Callgraph) callgraph in
  Format.printf "%d components found:\n"
  @@ Partition.number_of_groups scc_partition;

  Seq.iter (Partition.groups scc_partition) ~f:(fun group ->
      Group.enum group |> Seq.iter ~f:(fun x ->
          Format.printf "%s " @@ Tid.to_string x);
      Format.printf "\n");
  Some scc_partition
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
