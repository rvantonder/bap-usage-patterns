#use "topfind";;
#require "bap.top";;
open Core_kernel.Std
open Bap.Std
open Or_error
open Format

(** that will rcord a trace of evaluation ... *)
(** Todo: our own interpreter /context *)
(** todo: SM.run vs SM.exec *)

(** all free vars in program *)
(** Initialize state, prepend registers *)
(** Storage: own class, store address *)

(** Not typically interested in the result of interpretation,
but the process (hence hooks) *)

class context program = object(self : 's)
  inherit Biri.context program as super
end

class ['a] debugger = object(self)
   constraint 'a = #context
   inherit ['a] biri as super

   method! enter_term cls t =
     let tid = Term.tid t in
     printf "Enter: %a\n%!" Tid.pp tid;
     super#enter_term cls t
end

let prime project sub =
  let open Option in
  let arch = Project.arch project in
  let module Target = (val target_of_arch arch) in
  let width =
    match Arch.addr_size arch with
    | `r32 -> 32
    | `r64 -> 64 in
  let res =
    Term.first blk_t sub >>= fun blk ->
    Set.fold ~init:blk Target.CPU.gpr ~f:(fun blk reg ->
        let def = Def.create reg (Bil.int @@ Word.of_int ~width  0) in
        Term.prepend def_t blk def) |> Term.update blk_t sub
    |> some in
  Option.value res ~default:sub

let main () =
  Project.from_file Sys.argv.(1) >>= fun project ->
  let program = Project.program project in
  let main_sub = Term.find sub_t program Tid.(!"@main") in
  begin
    match main_sub with
    | Some sub -> (* printf "%a\n" Sub.pp sub;*)
      (*let ctxt = new Biri.context program in*)
      let ctxt = new context program in
      (*let interpreter = new biri in*)
      let interpreter = new debugger in
      let sub' = prime project sub in
      printf "%a\n" Sub.pp sub';
      let start = interpreter#eval_sub sub' in
      let r,res = Monad.State.run start ctxt in
      (*List.iter (res#trace |> List.rev) ~f:(fun tid ->
          printf "Tid: %a\n" Tid.pp tid);*)
      res#bindings |> Seq.iter ~f:(fun (v,bil_result) ->
          let result = Bil.Result.value bil_result in
          match result with
          | Bil.Imm w -> printf "Var: %a = %a\n" Var.pp v Word.pp w
          | Bil.Mem s -> printf "Have dump\n"; s#dump ()
          | Bil.Bot -> ());
    | None -> ()
  end;
  return ()

let () =
  try main ()
      |> function
      | Ok o -> ()
      | Error e -> Format.printf "BAP error: %s\n" @@ Error.to_string_hum e
  with
  | Invalid_argument _ ->
    Format.printf "Please specify a file on the command line\n"
