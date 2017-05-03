open Format

open Global
open Helper
open SimplQueue
open IntSets


(* A directed graph module *)


(* int as encoding of graph nodes *)
module NodeEnc = OrdInt
type node = OrdInt.t
module NodeSet = IntSet
type nodeSet = NodeSet.t

module NodeMap=IntMap


(* graph edge *)
type edge = {src:node; dst:node}


exception FoundCircuit of node * node

module type DiGraphSig =
  sig
    type t   (* the digraph *)
    type graphobj

    val decode: g:t -> node -> graphobj
    val encode: g:t -> graphobj -> node
    val allnodes: g:t -> node list

    val create: nodes:graphobj list
                -> pred:(src:graphobj -> dst:graphobj -> bool) -> t

    val find: g:t -> node -> NodeSet.t
    val bindings: g:t -> (node*node list) list

    val topological_sort: t -> graphobj list

    val print_digraph: nodesuffix:(node->string) -> t -> unit
  end

module type GraphObjSig =
  sig
    include Set.OrderedType
    val print: t -> unit
  end

module Make(GraphObj: GraphObjSig) : DiGraphSig
with type graphobj = GraphObj.t =
  struct
    type graphobj = GraphObj.t

    module EncM = Map.Make(GraphObj)
    module DecM=IntMap

    type t = { edges: (node list) NodeMap.t;
               encm:node EncM.t; decm:graphobj DecM.t; encbase:node; maxenc: node}

    let decode ~g i = DecM.find i g.decm
    let encode ~g o = EncM.find o g.encm
    let allnodes ~g = interval g.encbase (g.maxenc-1)

    let gid = ref 0  (* global id counter for nodes *)
    let inc () = let i = !gid in let i=i+1 in gid:=i; i

    let print_digraph ~nodesuffix g =
      let printv v = print_int v; print_string (nodesuffix v) in
      print_string "## Dependency Digraph:"; print_space ();
      NodeMap.iter (fun v us ->
        printv v; print_string " ~>"; print_space (); print_string "{";
        print_list ~print:printv ~sep:"," us; print_string "};"; print_space ())
        g.edges;
      print_string ("where the encoding w/ gid="^(string_of_int g.encbase)^
                    ".."^(string_of_int g.maxenc)^":"); force_newline ();
      DecM.iter (fun i ob ->
          printv i; print_string " = ";
          GraphObj.print ob; print_space ()) g.decm;
      force_newline ()

    let create ~nodes ~pred =
      let encbase = !gid in
      let maxenc, encm, decm = List.fold_left
            (fun (i,encm,decm) gobj ->
              let nextgid =
                try let _ = EncM.find gobj encm in
                  debug_format (fun () -> open_box 0;
                      print_string "Make.create: unexpected duplicates";
                      print_space ();
                      print_list ~print:GraphObj.print ~sep:"," nodes;
                      force_newline (); close_box ()); assert false
                with Not_found -> inc () in
              nextgid, EncM.add gobj i encm, DecM.add i gobj decm)
            (encbase,EncM.empty,DecM.empty) nodes in
      let enc n = EncM.find n encm in
      let encs ns = List.map enc ns in
      let add_dests ~edgem ~src dests =
        let dests = dests @
              (try NodeMap.find src edgem with Not_found -> []) in
        NodeMap.add src dests edgem in
      let edgem0 = DecM.fold
        (fun i _ m -> NodeMap.add i [] m) decm NodeMap.empty in
      let edgem = List.fold_left
            (fun edgem src ->
              let dests = List.filter (fun dst -> pred ~src ~dst) nodes in
              match dests with
              | [] -> edgem
              | dests -> let src = enc src and dests = encs dests in
                         add_dests ~edgem ~src dests) edgem0 nodes in
      let g = { edges=edgem; encm=encm; decm=decm; encbase=encbase; maxenc=maxenc } in
      g

    let find ~g key = NodeSet.abstract
          (try NodeMap.find key g.edges with Not_found -> [])

    let bindings ~g = NodeMap.bindings g.edges

    (* topological sorting *)
    type dfs_color = White | Gray | Black
    exception FoundWhite of node

    let topological_sort g =
      let encbase = g.encbase and maxenc = g.maxenc in
      let colors = Array.make (maxenc-encbase) White in (* array per node w/ Side effect *)
      let lookupColor v = colors.(v-encbase) in
      let updateColor v c = colors.(v-encbase)<-c in
      let rec dfs ~sorted v =
        (* let () = debug_int ~mes:(fun v -> "TopSort: visiting "^v) v in *)
        if lookupColor v = Black then sorted
        else
          let () = updateColor v Gray in
          let children = NodeMap.find v g.edges in
          let sorted =
            List.fold_left
              (fun sorted u ->
                if lookupColor u=Gray then raise (FoundCircuit (v,u))
                else dfs ~sorted u)
              sorted children in
          let () = updateColor v Black in
          v::sorted in
      let rec find_unseen () =
        try
          Array.iteri
            (fun i col -> match col with
            | White -> raise (FoundWhite i)
            | Black -> ()
            | _ -> assert false) colors; None
        with FoundWhite i -> Some (i+encbase) in
      let rec loop sorted =
        match find_unseen () with
        | None -> sorted
        | Some v -> let sorted = dfs ~sorted v in loop sorted in
      let sorted = loop [] in
      List.map (fun v -> DecM.find v g.decm) sorted

  end
