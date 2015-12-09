# Bap Usage Patterns

All of the following examples can be run with:

`./run-on-example [file-name]`

For example:

`./run-on-example ex1-1.ml`

## 1. Graphs and Dot output

> How do I output my program's callgraph in dot format?

```ocaml
(* [ex1-1.ml] *)
    let callgraph = Program.to_graph @@ Project.program project in
    let string_of_node n =
      sprintf "%S" @@ Tid.name @@ Graphlib.Callgraph.Node.label n in
    Graphlib.to_dot (module Graphlib.Callgraph)
      ~string_of_node ~filename:"callgraph.dot" callgraph;
```

<img src=/images/callgraph.png width=300 /><br>

> How do I output the CFG of of "main" in dot format?

```ocaml
  (* [ex1-2.ml] *)
  let program = Project.program project in

  let left_justify =
    String.concat_map ~f:(fun c ->
        if c = '\n' then "\\l" else Char.to_string c) in

  (match Term.find sub_t program Tid.(!"@main") with
   | Some main_sub ->
     let node_attrs _ =
       [`Shape `Box] in
     let string_of_node node = sprintf "\"\\%s\""
       @@ Blk.to_string @@ Graphlib.Ir.Node.label node |> left_justify in
     Graphlib.to_dot (module Graphlib.Ir) ~string_of_node ~node_attrs
       ~filename:"main.dot" @@ Sub.to_cfg main_sub
   | None -> ());
```

<img src=/images/main.png width=300 /><br>

Note: `Tid.(!"@main")` looks for a function called `main`. Your program needs to be compiled with debugging symbols, or you need to use the `--use-ida` option if you what to use this notation. Alternatively, use `Tid.(!"@sub_400440")` where `400440` corresponds to the address (in hex) of your function (for example, entry point).

Note: Here we use `Sub.to_cfg`, which returns a graph corresponding to `Graphlib.Ir`. Nodes of `Graphlib.Ir` are `blk`s.

> What if I don't want all of the IR in my CFG, but rather nodes and edges labeled with identifiers?

```ocaml
(* [ex1-3.ml] *)
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

## 2. Graph Library

> How do I find strongly connected components in my program?

```ocaml
  (* [ex2-1.ml] *)
  let callgraph = Program.to_graph @@ Project.program project in
  let scc_partition = Graphlib.strong_components
      (module Graphlib.Callgraph) callgraph in
  Format.printf "%d components found:\n"
  @@ Partition.number_of_groups scc_partition;

  Seq.iter (Partition.groups scc_partition) ~f:(fun group ->
      Group.enum group |> Seq.iter ~f:(fun x ->
          Format.printf "%s " @@ Tid.to_string x);
      Format.printf "\n");
  Some scc_partition
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

> How can I construct arbitrary graphs?

```ocaml
  (* [ex2-2.ml] *)
  let module G = Graphlib.Int.Unit in
  let g = Graphlib.create (module G) ~edges:[0,1,();1,1,()] () in
  Graphlib.to_dot (module G) ~filename:"graph.dot" g
```

## 3. Memory

> How do I print the memory of an ELF section, such as '.rodata'?

```ocaml
  (* [ex3-1.ml] *)
  let find_section_by_name name =
    let memory = Project.memory project in
    Memmap.to_sequence memory |> Seq.find_map ~f:(fun (m,x) ->
        Option.(Value.get Image.section x >>= fun n ->
                Option.some_if (n = name) m)) in
  (match find_section_by_name ".rodata" with
   | Some mem -> Format.printf "%a" Memory.pp mem
   | None -> Format.printf "No memory for this section\n");
```

> How do I print all of the memory sections (with labels) in an ELF binary?

```ocaml
  (* [ex3-2.ml] *)
  Project.memory project |> Memmap.to_sequence |> Seq.iter ~f:(fun (mem,x) ->
      Format.printf "%s(%a)@.%a@." (Value.tagname x) Value.pp x Memory.pp mem);
```

## 4. BAP IR

> How do I output the BAP IR of a specific function (a.k.a. subroutine)?

```ocaml
  (* [ex4-1.ml] *)
  let program = Project.program project in
  let main_sub = Term.find sub_t program Tid.(!"@main") in
  (match main_sub with
   | Some sub -> Format.printf "%s\n" (Sub.to_string sub)
   | None -> Format.printf "Could not find the subroutine\n");
```

#### Manipulating IR

> How do I manipulate the BAP IR?

###### Builder example

In BAP, you can manipulate `Term`s in the IR in a number of ways. One of those is with `Builder`, a constrained mutable state similar to `Buffer` for OCaml. It operates over the `Term`s of an IR, by letting you build up a `Program`, `Sub`, or `Blk`. Things become clearer with an example. Let's say you want to manipulate the `main` function with the IR. Namely, create a copy and add a concrete argument. The following snippet does this:

* By using `Builder`, we create an empty subroutine, and copy all the blocks over with `add_blk`
* After copying the blocks, add an argument called `argc` of size 32 bits, and value 1

```ocaml
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
```

Result: (`test/example`)
```asm
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
```

Note that we successively map across sub terms, starting with the program. The API supports many other useful functions across terms, such as `filter`, `filter_map`, `find`, `update`, and `remove` to name a few.


#### Manipulating BIL

> How do I manipulate BIL?

Because BIL follows an AST representation, working with BIL necessitates the use of visitor the pattern in BAP. Visitors for BIL are extremely powerful, but also harder to grasp, depending on your familiarity with the O in OCaml.

###### Simple visitor

A simple visitor is given below. It simply visits each BIL statement in a list of BIL statments, and prints the current statement.

```ocaml
  (* [ex4-4.ml] visit_each_stmt *)
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

###### Simple visitor

This visitor collects all direct jumps for a list of BIL statements (this example can be found in the API documentation):

```ocaml
  (* [ex4-4.ml] collect_calls *)
  let collect_calls bil_stmts =
    (object inherit [Word.t list] Bil.visitor
      method! enter_int x state = if in_jmp then x :: state else state
    end)#run bil_stmts []
```

Note:
* This visitor uses a `Word.t list` as user-supplied state which stores jumps.
* Our callback triggers every time we enter an int; essentially, a constant
* We determine that this constant is a jump target with the `in_jmp` predicate: this state is implicitly included with each visit. See the `class state` in the API for other information passed along visits.

###### Simple mapper

We just addressed `class 'a visitor`, where `'a` is our inherited user-supplied state. But there's also `class mapper`. `class mapper` doesn't carry any user-supplied state with it. With mapper, you can transform the BIL statements and expressions in the AST.

Let's transform every binary operation with some constant offset to an offset of `- 0x41`. For instance:

`RSP := RSP - 0x8:64`
becomes
`RSP := RSP - 0x41:64`

Specifically, if the second operand of the binary operator `+` or `-` is a constant, we rewrite it to be `0x41`.

Here's the code:

```ocaml
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
    end)#run bil_stmts
```

Note:
* We use `map_binop` instead of `enter_binop`.
* No user-state is passed a long. The return type for each expression is `exp`.
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
(* [ex4-5.ml custom_visitor *)
class ['a] custom_visitor = object
   inherit ['a * int list] Bil.visitor
end
```

Now you can write something like

```ocaml
  (* [ex4-5.ml custom_visit *)
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

## 5. Interpreters

> BAP has interpreters, what can I do with them?

BAP provides two default interpreters for BIL and BIR, both of which inherit the expression interpreter `Expi`. Note that there is a lot of documentation on these interfaces in [bap.mli](https://github.com/BinaryAnalysisPlatform/bap/blob/master/lib/bap/bap.mli). Here, we strive to show examples of how they can be used.

#### Interpreting BIL

Here's a BIL snippet for calculating the gcd of two numbers `a` and `b`.

```ocaml
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
        while_ (!a <> i32 0) [
          t := !b;
          b := !a mod !b;
          a := !t
        ];
      ])
  in ();
```

Note that `!` has been defined as a prefix operator that acts as a cast `var -> exp`. We're going to run this BIL code:

```asm
a := i32 9;
b := i32 3;
while_ (!a <> i32 0) [
  t := !b;
  b := !a mod !b;
  a := !t
]
```

using the BIL interpreter. We do this with `Stmt.eval bil_stmts (new Bili.context)`. Here's the entire snippet, which also prints the result of our run:

```ocaml
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
```

Our results:

```asm
Var: a = 0x3:32
Var: b = 0x0:32
Var: t = 0x3:32
```

Some things to note:
* We use the built in `Stmt.eval` to interpret the BIL code
* In the background, `Stmt.eval` instantiates an interpreter for us, using a `Bili.context`
* The resulting state can be accessed through `ctxt#bindings`, which contains `Bil.Result`s that can be queried

#### Interpreting BIR

Let's take things up a notch. Instead of interpreting a sequence of BIL instructions, we're now going to interpret BAP IR code.
The BAP IR is the preferred representation for program analysis. Instead of writing a sequence of BIR statements, (analogous
to the `gcd` routine in BIL from before), we are instead going to interpret the BIR generated when BAP lifts a binary. For this example, our binary will be compiled to the ARM architecture. Here's the source:

```c
// fib.c
// compile: arm-linux-gnueabi-gcc -std=c99 -g -o fib fib.c
// calculate the 10th fibonacci number (55)
int main() {
  int a = 1, b = 1, n = 10;
  for (int i = 3; i <= n; i++) {
      int c = a + b;
      a = b;
      b = c;
  }
  return b;
}
```

Here's the lifted BIR code of `main`:
```asm
# bap --no-byteweight -dbir test/fib-arm
00000051: sub main()
00000025:
00000026: mem := mem with [SP - 0x4:32, el]:u32 <- R11
00000027: SP := SP - 0x4:32
00000028: R11 := SP
00000029: SP := SP - 0x1C:32
0000002a: R3 := 0x1:32
0000002b: mem := mem with [R11 - 0x18:32, el]:u32 <- R3
0000002c: R3 := 0x1:32
0000002d: mem := mem with [R11 - 0x14:32, el]:u32 <- R3
0000002e: R3 := 0xA:32
0000002f: mem := mem with [R11 - 0xC:32, el]:u32 <- R3
00000030: R3 := 0x3:32
00000031: mem := mem with [R11 - 0x10:32, el]:u32 <- R3
00000032: goto %00000033

00000033:
00000034: R2 := mem[R11 - 0x10:32, el]:u32
00000035: R3 := mem[R11 - 0xC:32, el]:u32
00000036: CF := R3 <= R2
00000037: VF := high:1[(R2 ^ R3) & (R2 ^ (R2 - R3))]
00000038: NF := high:1[R2 - R3]
00000039: ZF := (R2 - R3) = 0x0:32
0000003a: when ZF | (NF <> VF) goto %0000003c
0000003b: goto %00000049

0000003c:
0000003d: R2 := mem[R11 - 0x18:32, el]:u32
0000003e: R3 := mem[R11 - 0x14:32, el]:u32
0000003f: R3 := R2 + R3
00000040: mem := mem with [R11 - 0x8:32, el]:u32 <- R3
00000041: R3 := mem[R11 - 0x14:32, el]:u32
00000042: mem := mem with [R11 - 0x18:32, el]:u32 <- R3
00000043: R3 := mem[R11 - 0x8:32, el]:u32
00000044: mem := mem with [R11 - 0x14:32, el]:u32 <- R3
00000045: R3 := mem[R11 - 0x10:32, el]:u32
00000046: R3 := R3 + 0x1:32
00000047: mem := mem with [R11 - 0x10:32, el]:u32 <- R3
00000048: goto %00000033

00000049:
0000004a: R3 := mem[R11 - 0x14:32, el]:u32
0000004b: R0 := R3
0000004c: SP := R11
0000004d: base_469 := SP
0000004e: R11 := mem[base_469, el]:u32
0000004f: SP := SP + 0x4:32
00000050: return LR
```

There are a few steps we need to take in order to interpret this BIR code. First, we need to initialize the registers with a blank state. There are a couple of ways to do this, but the simplest is to prepend BIR definition terms to the first block term. We can do this with a dedicated function called `prime` that primes our BIR snippet for interpretation:

```ocaml
(* [ex5-2.ml] *)
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
```

The start ouf our `main` subroutine will now look like this:

```asm
00000051: sub main()
00000025:
000000c6: SP := 0x0:32
000000c5: R9 := 0x0:32
000000c4: R8 := 0x0:32
000000c3: R7 := 0x0:32
000000c2: R6 := 0x0:32
000000c1: R5 := 0x0:32
000000c0: R4 := 0x0:32
000000bf: R3 := 0x0:32
000000be: R2 := 0x0:32
000000bd: R12 := 0x0:32
000000bc: R11 := 0x0:32
000000bb: R10 := 0x0:32
000000ba: R1 := 0x0:32
000000b9: R0 := 0x0:32
000000b8: LR := 0x0:32
...
```

We will instantiate a context for our BIR interpreter with `new Biri.context program`. We will instantiate the
interpreter itself with `new biri`. Since we want to evaluate our primed sub (`sub'` below), we call `interpreter#eval_sub` on
our interpeter.

Finally, we initiate execution with the state monad implementation provided in BAP, supplying it with our interpreter and context. This
corresponds to our `Monad.State.exec start ctxt` call.

```ocaml
  (* [ex5-2.ml] *)
  let program = Project.program project in
  let main_sub = Term.find sub_t program Tid.(!"@main") in
  begin
    match main_sub with
    | Some sub ->

      let ctxt = new Biri.context program in
      let interpreter = new biri in
      let sub' = prime project sub in
      printf "%a\n" Sub.pp sub';
      let start = interpreter#eval_sub sub' in
      let res = Monad.State.exec start ctxt in

      (* Print a trace of execution (tids), if desired *)
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
```

Here's the result of execution. Note how we observe the correct result in `R0` (and also `R3`).

```asm
Var: CF = true
Var: LR = 0x0:32
Var: NF = false
Var: R0 = 0x37:32
Var: R1 = 0x0:32
Var: R10 = 0x0:32
Var: R11 = 0x0:32
Var: R12 = 0x0:32
Var: R2 = 0xB:32
Var: R3 = 0x37:32
Var: R4 = 0x0:32
Var: R5 = 0x0:32
Var: R6 = 0x0:32
Var: R7 = 0x0:32
Var: R8 = 0x0:32
Var: R9 = 0x0:32
Var: SP = 0x0:32
Var: VF = false
Var: ZF = false
Var: base_469 = 0xFFFFFFFC:32
```

The `run-of-fib.sh` script shows this in action on three architectures: `ARM`, `x86`, and `x64`.

Some things to note:
* Here we use `Monad.State.exec` as opposed to `Stmt.eval` in the previous example. We supply it with our instantiated interpreter and context
* The final state can be accessed with `#bindings` in the same way as the previous example.
* There are some useful methods such as `#trace` which will give us the trace of term tids that were executed (see documentation for more)

#### Customizing Interpreters

Here's a very simple example of how one can customize an interpreter. We'll use the BIR interpreter for our example.

####### Debugger v1

Suppose we want to print a debug message when we enter a term, just before it is evaluated by the interpreter. We'll do this with a dedicated `debugger` interpreter which inherits the base `biri` interpreter. We declare it as follows:

```ocaml
(* [ex5-3.ml] *)
class ['a] debugger = object(self)
   constraint 'a = #context
   inherit ['a] biri as super

   method! enter_term cls t =
     let tid = Term.tid t in
     printf "Enter: %a\n%!" Tid.pp tid;
     super#enter_term cls t
end
```

Our interpreter will use a custom context (which simply inherits the default `Biri` context):

```ocaml
(* [ex5-3.ml] *)
class context program = object(self : 's)
  inherit Biri.context program as super
end
```

Now we just set our interpreter to `debugger` and run as before:

```ocaml
(* [ex5-3.ml] *)
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
```

And we observe the printed statements:

```asm
Entered: 00000051
Entered: 00000025
Entered: 000000c6
Entered: 000000c5
...
```

####### Debugger v2

Let's modify our debugger to print a message whenever a `save` or `load` operation occurs to our `storage` type (which acts as memory) during execution. Here we show how the `storage` type can be customized.

Note that such a message could *also* be be printed based on the IR term being evaluated (e.g., a `Store` or `Load` statement) with the corresponding callback `method! load` and ``method! store` in the interpreter class. In fact, that's probably more sensible. But I want to demonstrate the interface for declaring your own storage class.

We'll satisfy the `storage` interface's two methods, and include our debug message as follows:

```ocaml
(* [ex5-4.ml] *)
class memory : Bil.storage = object(self : 's)
  val storage = Bitvector.Map.empty

  method save x u =
    printf "Saving %a -> %a\n%!" Word.pp x Word.pp u;
    {< storage = Map.add storage ~key:x ~data:u >}

  method load x =
    printf "Loading %a\n%!" Word.pp x;
    Map.find storage x
end
```

In our debugger, we supply our `storage` class to `method! empty`:

```ocaml
(* [ex5-4.ml] *)
class ['a] debugger = object(self)
   constraint 'a = #context
   inherit ['a] biri as super

   method! empty = new memory

   method! enter_term cls t =
     let tid = Term.tid t in
     printf "Entered: %a\n%!" Tid.pp tid;
     super#enter_term cls t
end
```

Everything else remains as before. When we run this, we now see our debug messages:

```asm
...
Saving 0xFFFFFFFC:32 -> 0x0:8
Saving 0xFFFFFFFD:32 -> 0x0:8
Saving 0xFFFFFFFE:32 -> 0x0:8
Saving 0xFFFFFFFF:32 -> 0x0:8
...
```
