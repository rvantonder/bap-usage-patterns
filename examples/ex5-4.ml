#use "topfind";;
#require "bap.top";;
open Core_kernel.Std
open Bap.Std
open Or_error
open Format

class memory : Bil.storage = object(self : 's)
  val storage = Bitvector.Map.empty

  method save x u =
    printf "Saving %a -> %a\n%!" Word.pp x Word.pp u;
    {< storage = Map.add storage ~key:x ~data:u >}

  method load x =
    printf "Loading %a\n%!" Word.pp x;
    Map.find storage x
end

class context program = object(self : 's)
  inherit Biri.context program as super
end

class ['a] debugger = object(self)
   constraint 'a = #context
   inherit ['a] biri as super

   method! empty = new memory

   method! enter_term cls t =
     let tid = Term.tid t in
     printf "Entered: %a\n%!" Tid.pp tid;
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
    | Some sub ->
      let ctxt = new context program in
      let interpreter = new debugger in
      let sub' = prime project sub in
      printf "%a\n%!" Sub.pp sub';
      let start = interpreter#eval_sub sub' in
      let res = Monad.State.exec start ctxt in

      (*List.iter (res#trace |> List.rev) ~f:(fun tid ->
          printf "Tid: %a\n" Tid.pp tid);*)

      res#bindings |> Seq.iter ~f:(fun (v,bil_result) ->
          let result = Bil.Result.value bil_result in
          match result with
          | Bil.Imm w -> printf "Var: %a = %a\n" Var.pp v Word.pp w
          | Bil.Mem s -> ()
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
