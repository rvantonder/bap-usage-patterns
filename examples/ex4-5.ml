#use "topfind";;
#require "bap.top";;
open Core_kernel.Std
open Bap.Std
open Or_error

(* [ex4-5.ml custom_visitor *)
class ['a] custom_visitor = object
   inherit ['a * int list] Bil.visitor
end

let main () =
  Project.from_file Sys.argv.(1) >>= fun project ->

  (* [ex4-5.ml custom_visit *)
  let custom_visit bil_stmts =
    (object inherit [string] custom_visitor
      method! enter_stmt stmt state =
        Format.printf "Visiting %s\n" (Stmt.to_string stmt);
        ("still-user-defined",[3;2;1])
    end)#run bil_stmts ("user-defined",[1;2;3])
  in

  let syms = Project.symbols project in
  let main_fn = match Symtab.find_by_name syms "main" with
    | Some fn -> fn
    | None -> failwith "Could not find function main in symbol table"
  in
  let entry_block = Symtab.entry_of_fn main_fn in
  let block_insns = Block.insns entry_block in

  List.iter block_insns ~f:(fun (mem,insn) ->
      let bil = Insn.bil insn in
      custom_visit bil |> Pervasives.ignore);

  return ()

  let () =
    try main ()
        |> function
        | Ok o -> ()
        | Error e -> Format.printf "BAP error: %s\n" @@ Error.to_string_hum e
    with
    | Invalid_argument _ ->
      Format.printf "Please specify a file on the command line\n"
