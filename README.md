# Bap Usage Patterns

You can substitute the following patterns in `driver.ml` and run it with `run-on-example.sh` (after installing bap).

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

Note: Here we use `Sub.to_cfg`, which returns a graph corresponding to `Graphlib.Ir`. Nodes of `Graphlib.Ir` are `blk`s.

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

Note: here we use `Sub.to_graph`, and the appropriate types for labels: `tid`, as opposed to `blk` in the previous example. See bap documentation for why you might want this instead.

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

Result: (`test/example`)
```
00000161: sub new_main(argc)
00000160: argc :: in u32 = 0x1:32
000000a0:
000000a1: RSP := RSP - 0x8:64
000000a2: mem64 := mem64 with [RSP, el]:u64 <- RBP
000000a3: RBP := RSP
000000a4: t_114 := RSP
000000a5: RSP := RSP - 0x10:64
000000a6: CF := t_114 < 0x10:64
000000a7: OF := high:1[(t_114 ^ 0x10:64) & (t_114 ^ RSP)]
000000a8: AF := (((RSP ^ t_114) ^ 0x10:64) & 0x10:64) = 0x10:64
000000a9: PF := ~(low:1[let acc_115 = (RSP >> 0x4:64) ^ RSP in
let acc_115 = (acc_115 >> 0x2:64) ^ acc_115 in
(acc_115 >> 0x1:64) ^ acc_115])
000000aa: SF := high:1[RSP]
000000ab: ZF := RSP = 0x0:64
000000ac: mem64 := mem64 with [RBP - 0x8:64, el]:u32 <- 0x0:32
000000ad: RAX := pad:64[mem64[RBP - 0x8:64, el]:u32]
000000ae: RDI := pad:64[low:32[RAX]]
000000af: RSP := RSP - 0x8:64
000000b0: mem64 := mem64 with [RSP, el]:u64 <- 0x4005A2:64
000000b1: call @f with return %000000b2

000000b2:
000000b3: mem64 := mem64 with [RBP - 0x4:64, el]:u32 <- low:32[RAX]
000000b4: RAX := pad:64[mem64[RBP - 0x4:64, el]:u32]
000000b5: RSI := pad:64[low:32[RAX]]
000000b6: RDI := 0x400644:64
000000b7: RAX := 0x0:64
000000b8: RSP := RSP - 0x8:64
000000b9: mem64 := mem64 with [RSP, el]:u64 <- 0x4005B9:64
000000ba: call @sub_400410 with return %000000bb

000000bb:
000000bc: RSP := RBP
000000bd: RBP := mem64[RSP, el]:u64
000000be: RSP := RSP + 0x8:64
000000bf: ra_333 := mem64[RSP, el]:u64
000000c0: RSP := RSP + 0x8:64
000000c1: return ra_333
```

###### Mapping and changing Terms

In contrast to building up a subroutine, it is also possible to change IR terms selectively with the aid of a mapping function. Let's imagine that we wanted to clobber every assignment to `RSP` with the value `0x41414141` in the program. So, the following def

`000000a1: RSP := RSP - 0x8:64`<br>
would become
`000000a1: RSP := 0x41414141:64`<br>

Here's how we do that:

```ocaml
  let program = Project.program project in
  let modified_program =
    Term.map sub_t program ~f:(fun sub ->
        Term.map blk_t sub ~f:(fun blk ->
            Term.map def_t blk ~f:(fun def ->
                if Def.lhs def = AMD64.CPU.rsp then
                  Def.with_rhs def (Bil.int (Word.of_int ~width:64 0x41414141))
                else
                  def)))
  in

  (** Print the resulting program *)
  Format.printf "%a\n" Program.pp modified_program;
```

Note that we successively map across sub terms, starting with the program. The API supports many other useful functions across terms, such as `filter`, `filter_map`, `find`, `update`, and `remove` to name a few.

#### Manipulating BIL

Because BIL follows an AST representation, working with BIL necessitates the use of visitor the pattern in BAP. Visitors for BIL are extremely powerful, but also harder to grasp, depending on your familiarity with the O in Ocaml.

###### Syntax

######## Simple visitor

A simple visitor is given below. It simply visits each BIL statement in a list of BIL statments, and prints the current statement.

```ocaml
  let visit_each_stmt bil_stmts =
    (object inherit [unit] Bil.visitor
      method! enter_stmt stmt state =
        Format.printf "Visiting %s\n" (Stmt.to_string stmt)
    end)#run bil_stmts ()
```

A few things of note:
* This makes use of the `Bil.visitor` class. (This is important because there are other classes too).
* `[unit]` indicates the type of the state that we are passing along every time we enter a statement. This corresponds with the variable `state` for `enter_stmt`.
* The `#run` invocation operates over a `stmt list` by default.
* We pass unit `()` as the initial state.
* The return type of enter_stmt is that of our state: `unit`.

######## Simple visitor

This visitor collects all direct jumps for a list of BIL statements (this example can be found in the API documentation):

```ocaml
    (object inherit [Word.t list] Bil.visitor
      method! enter_int x state = if in_jmp then x :: state else state
    end)#run bil []
```

Note:
* This visitor uses a `Word.t list` as user-supplied state which stores jumps.
* Our callback triggers every time we enter an int; essentially, a constant
* We determine that this constant is a jump target with the `in_jmp` predicate: this state is implicitly included with each visit. See the `class state` in the API for other information passed along visits.

######## Simple mapper

We just addressed `class 'a visitor`, where `'a` is our inherited user-supplied state. But there's also `class mapper`. `class mapper` doesn't carry any user-supplied state with it. With mapper, you can transform the BIL statements and expressions in the AST.

Let's transform every binary operation with some constant offset to an offset of `- 0x41`. For instance:

`RSP := RSP - 0x8:64`
becomes
`RSP := RSP - 0x41:64`

Specifically, if the second operand of the binary operator `+` or `-` is a constant, we rewrite it to be `0x41`.

Here's the code:

```ocaml
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
```

Note:
* We reconstruct the `original_expression` and return that when our cases for substituting `0x41` are not matched.
* We pattern-match against the BIL operators `PLUS` and `MINUS`, and then pattern match the second operand against `Bil.Int`.
* We use `Bil.mapper`

###### Visitor flexibility

Examples to come, but note that:
* We can have multiple visit methods inside our visitor object
* We can visit any particular part of the BIL AST by replacing `#run` in previous examples with `#enter_stmt`, `#enter_exp`, `#enter_binop`, and so on. Note that `#run` accepts a `stmt list` by default.
* We can iterate, map, fold (and many more!) over BIL statements. For example, we can supply `Bil.fold` with a visitor object which is run over the AST with our own `init` state.
* We can create our own subclassing visitor, i.e. we don't have to use `class 'a visitor` or `class mapper`. For instance, we can pass our own implicit state a long with a custom visitor (and still allow anyone else to define a user-supplied state variable). Here's some quick syntax for defining your own visitor:

```ocaml
class ['a] visitor : object
  inherit ['a * int list ] Bil.visitor
end
```

Now you can write something like

```ocaml
  let custom_visit bil_stmts =
    (object inherit [string] custom_visitor
      method! enter_stmt stmt state =
        Format.printf "Visiting %s\n" (Stmt.to_string stmt);
        ("user-defined",[1;2;3])
    end)#run bil_stmts ("user-defined",[1;2;3])
```

Note:
* Our visitor inherits only the type of our user-defined state: a string.
* The `int list` is passed along any visitor we create using `custom_vistor`. This
is useful if the `int list` state is changed by another function as we fold over BIL
(for instance, for tracking depth in the AST, we might create a `depth_visitor` that
maintains a depth of the current traversal).

#### Scratchpad

> Find all subroutines that call the subroutine `src`

```ocaml
  let subs =
    Graphlib.depth_first_search (module Graphlib.Callgraph)
      ~enter_node:(fun i n s ->
          let sub = Graphlib.Callgraph.Node.label n |> Util.sub_of_tid in
          match sub with
          | Some sub ->
            if List.exists (Util.calls_of_sub sub) ~f:(fun call_tid ->
                Tid.name call_tid = src) then
              n :: s
            else s
          | None -> s) ~init:[] callgraph
```

Go through this example:

```
      {[Bil.([
          v := src lsr i32 1;
          r := src;
          s := i32 31;
          while_ (var v <> i32 0) [
            r := var r lsl i32 1;
            r := var r lor (var v land i32 1);
            v := var v lsr i32 1;
            s := var s - i32 1;
          ];
          dst := var r lsl var s;
        ])]}
      where [i32] is defined as
      [let i32 x = Bil.int (Word.of_int ~width:32 x)]
      and [v,r,s] are some variables of type [var]; and
      [src, dst] are expressions of type [exp].
```

Note that we can pattern-match based on multiple components of a visitor:

```
      method! enter_binop op o1 o2 state =
        match (op,o1,o2) with
        | (Bil.PLUS, Bil.Var v, Bil.Int off)
        | (Bil.MINUS, Bil.Var v, Bil.Int off) ->
          if AMD64.CPU.is_bp v then
            let offset =
              Word.to_int off |> ok_exn in
            let var_name = Format.sprintf "%s_%02x" (Exp.to_string o1) offset in
            Var.create ~tmp:false var_name reg32_t
          else
            state
        | _ -> state
```
