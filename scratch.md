#### Scratchpad

State monad usage:

For BAP interfaces in the interpreter of the form:

`method store : Bil.storage -> addr -> word -> 'a r`, the return type needs to be `Bil.result`. Like so:

```
method! store s a w =
  super#store s a w >>= fun r -> SM.return r
```

If we want to change the context, we extract it like so:

```
method! store s a w =
  super#store s a w >>= fun r ->
  SM.get () >>= fun ctxt ->
  SM.put ctxt >>= fun () ->
  SM.return r
```

More things to include that still need examples.

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

> How can I remove back edges in a graph

```
let process (module G : Graphlib.Graph with type edge =
Graphlib.Int.Unit.edge and type node =
int and type t = Graphlib.Int.Unit.t) graph =
  List.iter [0;1;2;3;4] ~f:(fun i ->
      Format.printf "Current: %d. Succs:" i;
      Seq.iter (G.Node.succs i graph) ~f:(fun succ ->
          Format.printf " %d" succ);
      Format.printf "\n"
    )

let () =
  let module G = Graphlib.Int.Unit in
  let g = Graphlib.create (module G)
      ~edges:[0,1,();
              1,2,();
              2,3,();
              3,4,();
              3,1,()] () in

  (** Collect back edges *)
  let edge_set = G.Edge.Hash_set.create () in
  Graphlib.depth_first_search (module G)
    ~enter_edge:(fun k e _ ->
        if k = `Back then
          Hash_set.add edge_set e
        else ()) ~init:() g;

  (** Create view with back edges removed *)
  let graph_view =
    Graphlib.filtered (module G)
      ~skip_edge:(fun e -> Hash_set.mem edge_set e) () in

  Graphlib.to_dot (module G) ~filename:"normal.dot" g;
  Graphlib.to_dot graph_view ~filename:"no_back_edges.dot" g;

  Format.printf "Normal:\n";
  process (module G) g;

  Format.printf "\nWith graph view filter:\n";
  process graph_view g
```

(* Data Writers *)

```
let to_bytes = ... in
let writer = Data.Writer.create ~to_bytes () in
Addr.add_writer ~ver:"0.1" ~desc:"better printer" "dpr";
Addr.set_default_printer "dpr"
```
