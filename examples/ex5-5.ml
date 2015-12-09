#use "topfind";;
#require "bap.top";;
open Core_kernel.Std
open Bap.Std
open Or_error
open Format

module SM = Monad.State
open SM.Monad_infix

class context program = object(self : 's)
  inherit Biri.context program as super

  val addrs = Bitvector.Set.empty

  (** getter *)
  method addrs = addrs

  method add addr = {< addrs = Set.add addrs addr >}
end

class ['a] debugger = object(self)
   constraint 'a = #context
   inherit ['a] biri as super

  method! store s a w =
    super#store s a w >>= fun r ->
    SM.get () >>= fun ctxt ->
    SM.put (ctxt#add a) >>= fun () ->
    SM.return r
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
  let open Or_error in
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

      Set.iter res#addrs ~f:(fun addr -> Format.printf "%a\n" Addr.pp addr);

      printf "Address -> Value:\n";
      res#bindings |> Seq.iter ~f:(fun (v,bil_result) ->
          let result = Bil.Result.value bil_result in
          match result with
          | Bil.Mem s -> Set.iter res#addrs ~f:(fun addr ->
              (match s#load addr with
              | Some word -> printf "\t%a -> %a\n" Addr.pp addr Word.pp word
              | None -> ()))
          | _ -> ());
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
