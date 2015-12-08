#use "topfind";;
#require "bap.top";;
open Core_kernel.Std
open Bap.Std
open Or_error

let main () =
  Project.from_file Sys.argv.(1) >>= fun project ->

  (* [ex1-2.ml] *)
  let program = Project.program project in

  let left_justify =
    String.concat_map ~f:(fun c ->
        if c = '\n' then "\\l" else Char.to_string c) in

  (match Term.find sub_t program Tid.(!"@main") with
   | Some main_sub ->
     let node_attrs _ =
       [`Shape `Box] in
     let string_of_node node = sprintf "\"\\%s\""
       @@ Blk.to_string @@ Graphlib.Ir.Node.label node |> left_justify in
     Graphlib.to_dot (module Graphlib.Ir) ~string_of_node ~node_attrs
       ~filename:"main.dot" @@ Sub.to_cfg main_sub
   | None -> ());

  return ()

let () =
  try main ()
      |> function
      | Ok o -> ()
      | Error e -> Format.printf "BAP error: %s\n" @@ Error.to_string_hum e
  with
  | Invalid_argument _ ->
    Format.printf "Please specify a file on the command line\n"
