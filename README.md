# Bap Usage Patterns

You can substitute the following patterns in the `driver.ml` and run it in with the vanilla `ocaml` interpreter (after installing bap).

## Pretty graphs

> How do I output my program's callgraph in dot format?

```ocaml
let callgraph = Program.to_graph @@ Project.program project in
let string_of_node n = sprintf "%S" @@ Tid.name @@ Graphlib.Callgraph.Node.label n in
Graphlib.to_dot (module Graphlib.Callgraph) ~string_of_node ~filename:"callgraph.dot" callgraph;
```

<img src=/images/callgraph.png width=300 /><br>

> How do I output the CFG of of "main" in dot format?

```ocaml
let program = Project.program project in
Option.(Term.find sub_t program Tid.(!"@main") >>= fun main_sub ->
        let node_attrs _ =
          [`Shape `Box] in
        let string_of_node node = sprintf "\"\\%s\""
          @@ Blk.to_string @@ Graphlib.Ir.Node.label node in
        Graphlib.to_dot (module Graphlib.Ir) ~string_of_node ~node_attrs
          ~filename:"main.dot" @@ Sub.to_cfg main_sub; Some main_sub)
|> Pervasives.ignore;
```

<img src=/images/main.png width=300 /><br>

Note: `Tid.(!"@main")` looks for a function called `main`. Your program needs to be compiled with debugging symbols, or you need to use the `--use-ida` option if you what to use this notation. Alternatively, use `Tid.(!"@sub_400440")` where `400440` corresponds to the address (in hex) of your function (for example, entry point).

> What if I don't want all of the IR in my CFG, but rather nodes and edges labeled with identifiers?

```ocaml
let program = Project.program project in
Option.(Term.find sub_t program Tid.(!"@main") >>= fun main_sub ->
        let string_of_node n = sprintf "\"\\%s\""
            @@ Tid.name @@ Graphlib.Tid.Tid.Node.label n in
        let string_of_edge e = Tid.name @@ Graphlib.Tid.Tid.Edge.label e in
            Graphlib.to_dot ~string_of_node ~string_of_edge
              ~filename:"main_with_tids.dot" (module Graphlib.Tid.Tid) @@
          Sub.to_graph main_sub; Some main_sub)
|> Pervasives.ignore;
```

<img src=/images/tid_only_graph.png height=200 /><br>

Note: here we use `Sub.to_graph`, and the appropriate types for labels. See bap documentation for why you might want this instead.

## Graph Library

> How do I find strongly connected components in my program?

```ocaml
let callgraph = Program.to_graph @@ Project.program project in
Option.(let scc_partition = Graphlib.strong_components
            (module Graphlib.Callgraph) callgraph in
        Format.printf "%d components found:\n" @@ Partition.number_of_groups scc_partition;
        Seq.iter (Partition.groups scc_partition) ~f:(fun group ->
            Group.enum group |> Seq.iter ~f:(fun x ->
                Format.printf "%s " @@ Tid.to_name x);
            Format.printf "\n");
        Some scc_partition)
|> Pervasives.ignore;
```

```
Output:
7 components found:
@sub_4003e0
@sub_400410
@sub_400430
@h @g
@f
@main
@__libc_csu_init
```

## Memory

> How do I print the memory of an ELF section, such as '.rodata'?

```ocaml
let find_section_by_name name =
  let memory = Project.memory project in
  Memmap.to_sequence memory |> Seq.find_map ~f:(fun (m,x) ->
      Option.(Value.get Image.section x >>= fun n ->
              Option.some_if (n = name) m)) in
begin
  match find_section_by_name ".rodata" with
  | Some mem -> Format.printf "%a" Memory.pp mem
  | None -> Format.printf "No memory for this section\n"
end;
```

> How do I print all of the memory sections (with labels) in an ELF binary?

```ocaml
Project.memory project |> Memmap.to_sequence |> Seq.iter ~f:(fun (mem,x) ->
    Format.printf "%s(%a)@.%a@." (Value.tagname x) Value.pp x Memory.pp mem);
```

## IR

> How do I output the BAP IR of a specific function (a.k.a. subroutine)?

```ocaml
let program = Project.program project in
let main_sub = Term.find sub_t program Tid.(!"@main") in (** Find the subroutine called 'main' *)
begin
  match main_sub with
  | Some sub -> Format.printf "%s\n" (Sub.to_string sub)
  | None -> Format.printf "Could not find the subroutine\n"
end;
```

#### Manipulating IR

###### Builder example

In BAP, you can manipulate `Term`s in the IR in a number of ways. One of those is with `Builder`, a constrained mutable state similar to `Buffer` for OCaml. It operates over the `Term`s of an IR, by letting you build up a `Program`, `Sub`, or `Blk`. Things become clearer with an example. Let's say you want to manipulate the `main` function with the IR. Namely, create a copy and add a concrete argument. The following snippet does this:

* By using `Builder`, we create an empty subroutine, and copy all the blocks over with `add_blk`
* After copying the blocks, add an argument called `argc` of size 32 bits, and value 1

```ocaml
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
```

Result:
```
000000ad: argc :: in u32 = 0x1:32
00000032:
00000033: base_123 := SP
00000034: mem := mem with [base_123 - 0x4:32, el]:u32 <- LR
00000035: mem := mem with [base_123 - 0x8:32, el]:u32 <- R11
00000036: SP := SP - 0x8:32
00000037: R11 := SP + 0x4:32
00000038: SP := SP - 0x8:32
00000039: LR := 0x843C:32
0000003a: call @foo with return %0000003b

0000003b:
0000003c: R3 := mem[R11 - 0x8:32, el]:u32
0000003d: R3 := R3 + 0x1:32
0000003e: mem := mem with [R11 - 0x8:32, el]:u32 <- R3
0000003f: R0 := R3
00000040: SP := R11 - 0x4:32
00000041: base_382 := SP
00000042: R11 := mem[base_382, el]:u32
00000043: SP := SP + 0x8:32
00000044: goto mem[base_382 + 0x4:32, el]:u32000000ad: argc :: in u32 = 0x1:32
```
