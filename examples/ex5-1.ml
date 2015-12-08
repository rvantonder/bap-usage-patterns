#use "topfind";;
#require "bap.top";;
open Core_kernel.Std
open Bap.Std
open Or_error
open Format

let main () =
  Project.from_file Sys.argv.(1) >>= fun project ->

  (* [ex5-1.ml *)
  let bil_stmts =
    let a = Var.create "a" reg32_t in
    let b = Var.create "b" reg32_t in
    let t = Var.create "t" reg32_t in
    let i32 x = Bil.int (Word.of_int ~width:32 x) in
    let (!) = Bil.var in

    Bil.([
        a := i32 9;
        b := i32 3;
        while_ (!b <> i32 0) [
          t := !b;
          b := !a mod !b;
          a := !t
        ];
      ]) in

  let ctxt = Stmt.eval bil_stmts (new Bili.context) in
  ctxt#bindings |> Seq.iter ~f:(fun (v,bil_result) ->
      let result = Bil.Result.value bil_result in
      match result with
      | Bil.Imm w -> printf "Var: %a = %a\n" Var.pp v Word.pp w
      | Bil.Mem s -> () (* Our example doesn't use memory *)
      | Bil.Bot -> () (* Our example has no undefined results *)
    );

  return ()

let () =
  try main ()
      |> function
      | Ok o -> ()
      | Error e -> Format.printf "BAP error: %s\n" @@ Error.to_string_hum e
  with
  | Invalid_argument _ ->
    Format.printf "Please specify a file on the command line\n"
