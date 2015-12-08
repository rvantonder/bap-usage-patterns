#use "topfind";;
#require "bap.top";;
open Core_kernel.Std
open Bap.Std
open Or_error

let main () =
  Project.from_file Sys.argv.(1) >>= fun project ->

  (* [ex4-3.ml] *)
  let program = Project.program project in
  let modified_program =
    Term.map sub_t program ~f:(fun sub ->
        Term.map blk_t sub ~f:(fun blk ->
            Term.map def_t blk ~f:(fun def ->
                if Def.lhs def = AMD64.CPU.rsp then
                  Def.with_rhs def (Bil.int (Word.of_int ~width:64 0x41414141))
                else
                  def))) in

  (** Print the resulting program *)
  Format.printf "%a\n" Program.pp modified_program;

  return ()

let () =
  try main ()
      |> function
      | Ok o -> ()
      | Error e -> Format.printf "BAP error: %s\n" @@ Error.to_string_hum e
  with
  | Invalid_argument _ ->
    Format.printf "Please specify a file on the command line\n"
