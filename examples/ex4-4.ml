#use "topfind";;
#require "bap.top";;
open Core_kernel.Std
open Bap.Std
open Or_error

let main () =
  Project.from_file Sys.argv.(1) >>= fun project ->

  let normalize = String.filter ~f:(function
      | '\t' | '{' | '}' -> false
      | _ -> true) in

  let syms = Project.symbols project in
  let main_fn = match Symtab.find_by_name syms "main" with
    | Some fn -> fn
    | None -> failwith "Could not find function main in symbol table"
  in
  let entry_block = Symtab.entry_of_fn main_fn in
  let block_insns = Block.insns entry_block in

  (* [ex4-4.ml] visit_each_stmt *)
  let visit_each_stmt bil_stmts =
    (object inherit [unit] Bil.visitor
      method! enter_stmt stmt state =
        Format.printf "Visiting %s\n" (Stmt.to_string stmt)
    end)#run bil_stmts ()
  in

  (* [ex4-4.ml] collect_calls *)
  let collect_calls bil_stmts =
    (object inherit [Word.t list] Bil.visitor
      method! enter_int x state = if in_jmp then x :: state else state
    end)#run bil_stmts []
  in

  (* [ex4-4.ml] offset_41_mapper *)
  let offset_41_mapper bil_stmts =
    (object inherit Bil.mapper
      method! map_binop operator operand1 operand2 =
        let original_expression =
          Bil.binop operator operand1 operand2 in
        match operator with
        | Bil.PLUS | Bil.MINUS ->
          (match operand2 with
           | Bil.Int offset ->
             let new_operand2 = Bil.int (Word.of_int ~width:64 0x41) in
             Bil.binop operator operand1 new_operand2
           | _ -> original_expression)
        | _ -> original_expression
    end)#run bil_stmts in

  List.iter block_insns ~f:(fun (mem,insn) ->
      (*Format.printf "Raw disassembly: %a\n" Insn.pp insn;*)
      let bil = Insn.bil insn in
      (* Format.printf "Bil: %s\n" (Bil.to_string bil |> normalize);*)
      visit_each_stmt bil;
      let new_bil =
        offset_41_mapper bil in
      Format.printf "41-Bil: %s\n" (Bil.to_string new_bil |> normalize));

  return ()

  let () =
    try main ()
        |> function
        | Ok o -> ()
        | Error e -> Format.printf "BAP error: %s\n" @@ Error.to_string_hum e
    with
    | Invalid_argument _ ->
      Format.printf "Please specify a file on the command line\n"
