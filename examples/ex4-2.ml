#use "topfind";;
#require "bap.top";;
open Core_kernel.Std
open Bap.Std
open Or_error

let main () =
  Project.from_file Sys.argv.(1) >>= fun project ->

  (* [ex4-2.ml] *)
  let program = Project.program project in
  (** This copies each block in [sub], and then adds an argument
      [argc] of size 32 bits, and value of 1 *)
  let copy_and_add_arg sub =
    let builder = Sub.Builder.create ~name:("new_"^(Sub.name sub)) () in
    (** Copy all blks *)
    Seq.iter (Term.enum blk_t sub) ~f:(fun blk ->
        Sub.Builder.add_blk builder blk);
    (** Add the argument *)
    let var = Var.create "argc" reg32_t in
    let exp = Bil.int (Word.of_int ~width:32 1) in
    let arg0 = Arg.create ~intent:In var exp in
    Sub.Builder.add_arg builder arg0;
    Sub.Builder.result builder
  in

  (** Let's create an empty sub, in case building a new one fails for some
      reason (e.g. main does not exist) *)
  let empty_sub = Sub.Builder.create () |> Sub.Builder.result in

  let new_sub =
    Option.(Term.find sub_t program Tid.(!"@main") >>= fun main_sub ->
            Some (copy_and_add_arg main_sub)) |>
    Option.value ~default:empty_sub in

  (** Print the resulting sub *)
  Format.printf "%a\n" Sub.pp new_sub;

  return ()

let () =
  try main ()
      |> function
      | Ok o -> ()
      | Error e -> Format.printf "BAP error: %s\n" @@ Error.to_string_hum e
  with
  | Invalid_argument _ ->
    Format.printf "Please specify a file on the command line\n"
