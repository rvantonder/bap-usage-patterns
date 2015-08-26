# Bap Usage Patterns

You can substitute the following patterns in the `driver.ml` and run it in with the vanilla `ocaml` interpreter (after installing bap).

## Pretty graphs

> How do I output my program's callgraph in dot format?

```ocaml
let callgraph = Program.to_graph @@ Project.program project in
let string_of_node n = sprintf "%S" @@ Tid.name @@ Graphlib.Callgraph.Node.label n in
Graphlib.to_dot (module Graphlib.Callgraph) ~string_of_node ~filename:"callgraph.dot" callgraph;
```

![](/images/callgraph.png)

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
![](/images/main.png)

Note: `Tid.(!"@main")` looks for a function called `main`. Your program needs to be compiled with debugging symbols, or you need to use the `--use-ida` option if you what to use this notation. Alternatively, use `Tid.(!"@sub_400440")` where `400440` corresponds to the address (in hex) of your function (for example, entry point).

> What if I don't want all of the IR in my CFG, but rather nodes and edges labeled with identifiers?

```
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

![](/images/tid_only_graph.png)

Note: here we use `Sub.to_graph`, and the appropriate types for labels. See bap documentation for why you might want this instead.

## Graphs

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
