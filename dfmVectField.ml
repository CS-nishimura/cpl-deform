open Format

open Global
open Helper
open Proc
open AbstractSimplicialComplex
open SimplicialComplex
open DfmVect
open IntSets
open SimplQueue
open Bipartite
open SetSystem
open HyperGraphImpl
open HyperGraph
open DirectedGraph

(* deformation vector field *)
module DVectField = Set.Make(struct
  type t = vect
  let compare = compare end)

let print_vectfld sp =
  print_list ~print:print_dfmvect ~sep:"," (DVectField.elements sp)

let heads vfld = DVectField.fold
    (fun {head=head} hs -> Complex.add_simplex head hs)
    vfld Complex.null
let tails vfld = DVectField.fold
    (fun {tail=tail} ts -> Complex.add_simplex tail ts)
    vfld Complex.null
let bothends vfld = Complex.union (heads vfld) (tails vfld)

let orphaned_heads ~root vfld =
  Complex.diff (Complex.diff (heads vfld) root) (tails vfld)

let partition_orphanedhds ~orphaned vfld =
  DVectField.partition
    (fun {head=head} -> Complex.mem_facet head orphaned) vfld

let partition_byVtail ~vfld tail =
  DVectField.partition
    (fun v -> Simplex.compare tail v.tail = 0) vfld

let partition_bySubsumpt ~vfld coface =
  DVectField.partition
    (fun v -> Simplex.sub v.tail coface) vfld

let inducedFaces vfld =
  DVectField.fold
    (fun {head=head;tail=tail} faces ->
       Complex.union faces
         (Complex.all_faces (Complex.of_simplex (Simplex.union head tail))))
    vfld Complex.null

(*
let find_tails ~vecfld ~head =
  let flt = DVectField.filter
              (fun v -> Simplex.compare head v.head = 0) vecfld in
  List.map (fun v -> v.tail) (DVectField.elements flt)

let find_Vhead ~vecfld ~tail =
  let flt = DVectField.filter
              (fun v -> Simplex.compare tail v.tail = 0) vecfld
  in flt
 *)

(* topological sort *)
module VGraphNode = struct
  type t = vect
  let compare = compare
  let print = print_dfmvect
  end

module VDigraph = DirectedGraph.Make(VGraphNode)
(* open VDigraph *)

let topological_sort vecfld =
  let nodes = DVectField.elements vecfld in
  let pred ~src ~dst =
    let srcspl = Simplex.union src.head src.tail in
    let dstspl = Simplex.union dst.head dst.tail in
    Simplex.compare srcspl dstspl !=0 && Simplex.sub dst.tail srcspl
  in
  let g = VDigraph.create ~nodes ~pred in
  let () =
    debug_format (fun () -> open_box 0;
                   VDigraph.print_digraph ~nodesuffix:(fun x->"") g; close_box ())
  in VDigraph.topological_sort g



(*** nondeterministic choice of vector field ***)

let decForest g = (* g assumed to be a 2-unif. hyperforest *)
  let decm = g.decode and edges = g.edges in
  EdgeSet.fold
    (fun fe es ->
      let decs = List.map (decFacet ~decm) (HyperEdge.elements fe) in
      match decs with
      | [x;y] -> (x,y)::es
      | err ->
        debug_format
          (fun () -> open_box 0;
            print_string "!!! DfmVectField.decForest encoutered [";
            print_list ~print:Printer.print_vispl ~sep:";" decs;
            print_endline "]"; close_box ());
        assert false)
    edges []

(* shrinking hyperforest: output = list of edges of the shurnken forest *)
let isGoal ~maxvs spl = not (Simplex.is_null (Simplex.inter maxvs spl))
let goals ~maxvs cpl = Complex.filter (isGoal ~maxvs) cpl
let isStart ~inputCpl spl = Complex.mem spl inputCpl
(* let starts ~inputCpl base = Complex.filter (isStart ~inputCpl) base *)

let shrink ~maxvs ~base ~inputCpl =
  let goalcpl = goals ~maxvs base in
  let goalfcpl = goals ~maxvs (Complex.all_faces goalcpl) in
  let startcpl = Complex.filter (fun spl -> Complex.mem spl base) inputCpl in
  (*
  let () = debug_format
      (fun () -> open_hovbox 4;
        print_string "Base cpl=";
        Printer.print_vicpl base; print_newline ();
        print_string "inputCpl=";
        Printer.print_vicpl inputCpl; print_newline ();
        print_string "starting Cpl=";
        Printer.print_vicpl startcpl; print_newline ();
        print_string "goal face Cpl=";
        Printer.print_vicpl goalfcpl; print_newline ();
        close_box ()) in *)
  let g = HyperG.create ~startcpl ~goalcpl ~goalfcpl base in
  let decm = g.decode (* and origedges=g.edges *)
  and roots = g.roots and goals=g.goals and goalfaces=g.goalfaces in
  let () = debug_format (fun () -> open_box 0;
    print_encoding ~maxvs ~roots ~goalfaces (DecM.bindings decm);
    HyperG.summarize_hyperg ~name:"Hypergraph " ~goals g; close_box ()) in

  let () = BipartG.alloc g in (* side effect *)
  let hypforest =
    if strongHall ~vertices:g.vertices ~edges:g.edges then g
    else
      let forest = HyperG.hyperdeforest g in
      let () = debug_format (fun () -> HyperG.summarize_hyperg ~name:"Hyperdeforest" ~goals forest) in
      forest in

  let () = debug_message "Shrinking hyperforest..." in
  let forest = HyperG.shrink_hyperforest hypforest in
  let () = debug_format
      (fun () -> open_box 0;
        print_string "##Shrinked Forest## Verts=";
        print_int (HyperEdge.cardinal (EdgeSet.unions g.edges));
        print_string ", Edges=";
        print_int (EdgeSet.cardinal g.edges); print_newline ();
        close_box ()) in
  decForest forest

module PrintableSimplex =
  struct
    include Simplex
    let print = Printer.print_vispl
  end

module FaceDG = DirectedGraph.Make(PrintableSimplex)
module NodeSet = DirectedGraph.NodeSet


let create_faceDigraph ~bases fpairs = (* digraphs of coherent edges & anti edges *)
  let nodes =
    let base = List.fold_left Complex.union_simpset Complex.null bases in
    Complex.elements (Complex.all_faces base) in
  let add_edge ~src ~dst fmap =
    let dsts = try dst::(FaceMap.find src fmap) with Not_found -> [dst] in
    FaceMap.add src dsts fmap in
  let fmap = List.fold_left (fun fmap (s1,s2) ->
    let cpn = compareFacets s1 s2 in  (* add coherent-or-tie edges: src=head, dst=tail *)
    let fmap = if cpn>=0 then add_edge ~src:s2 ~dst:s1 fmap else fmap in
    let fmap = if cpn<=0 then add_edge ~src:s1 ~dst:s2 fmap else fmap in fmap)
    FaceMap.empty fpairs in
  (* let antimap = List.fold_left (fun fmap (s1,s2) ->
    let cpn = compareFacets s1 s2 in  (* add anti-coherent edges: src=tail, dst=head *)
    let fmap = if cpn<0 then add_edge ~src:s2 ~dst:s1 fmap else fmap in
    let fmap = if cpn>0 then add_edge ~src:s1 ~dst:s2 fmap else fmap in fmap)
    FaceMap.empty fpairs in *)
  let fpred ~src ~dst =
    try
      let dsts = FaceMap.find src fmap in
      List.exists (fun spl -> Simplex.compare dst spl=0) dsts
    with Not_found -> false in
  let apred ~src ~dst =
    try
      let dsts = FaceMap.find src fmap in
      List.exists (fun spl -> Simplex.compare dst spl=0) dsts
    with Not_found -> false in
  FaceDG.create ~nodes ~pred:fpred, FaceDG.create ~nodes ~pred:apred



type bfsq = {maxv:vertex; root:complex; depth:int;
             vfield:DVectField.t; visited:nodeSet}

let softConnected ~g ~visited = (* list of soft connectable extension: (src,dst) list *)
  let nonvisited dsts = NodeSet.diff (NodeSet.abstract dsts) visited in
  (* let cpl = NodeSet.fold (fun n cpl ->
        Complex.add_simplex (FaceDG.decode ~g n) cpl)
        visited Complex.null in *)
  let bds = FaceDG.bindings ~g in
  List.fold_left (fun cands (src,dsts) ->
      if not (NodeSet.mem src visited) then
        let srcf = FaceDG.decode ~g src in
        let yet2Visit = nonvisited dsts in
        NodeSet.fold (fun dst srcdst ->
          let coface = Simplex.union srcf (FaceDG.decode ~g dst) in
          let intersect = NodeSet.filter
                (fun i -> Simplex.sub (FaceDG.decode ~g i) coface) visited in
          if not (NodeSet.is_empty intersect)
          then (NodeSet.choose intersect, src,dst)::srcdst else srcdst)
          yet2Visit cands
      else cands) [] bds

let softLink ~g ~visited  =
  let cands = softConnected ~g ~visited in
  let measure (_,src,dst) =
      let fs = FaceDG.decode ~g src and fd = FaceDG.decode ~g dst in
      compareFacets fs fd in
  minimum_elem ~measure cands

let decodeVect ~g ~head ~tail =
  {head = FaceDG.decode ~g head; tail = FaceDG.decode ~g tail}

let starVF ~g ~node nodeset =
  NodeSet.fold
    (fun m vf -> DVectField.add (decodeVect ~g ~head:node ~tail:m) vf)
    nodeset DVectField.empty

(*
let connected1 ~g ~visited ~fronts =
  NodeSet.fold
    (fun node (vf,fr)->
       let newfr = NodeSet.diff (FaceDG.find ~g node) visited in
       let newvf = starVF ~g ~node newfr in
       DVectField.union vf newvf, NodeSet.union fr newfr)
    fronts (DVectField.empty, NodeSet.empty)
 *)

let newadjs ~g ~visited =
  NodeSet.fold
    (fun node (nodes,vecfld) ->
       if not (NodeSet.mem node visited) then nodes,vecfld
       else
         let dsts = NodeSet.filter
             (fun m -> not (NodeSet.mem m visited) && not (NodeSet.mem m nodes))
             (FaceDG.find ~g node) in
         NodeSet.union dsts nodes, DVectField.union (starVF ~g ~node dsts) vecfld)
    visited (NodeSet.empty, DVectField.empty)

let coherentSpan1 ~g ~starts ent =
  let vfield = ent.vfield and visited = ent.visited in
  let newnodes, newvfld = newadjs ~g ~visited in  (* connected1 ~g ~visited ~fronts in *)
  (* let () = debug_format
      (fun () -> open_box 0;
        print_string "*coherentaSpan1.newnodes="; print_IntSet newnodes;
        print_newline (); close_box ()) in *)
  if NodeSet.is_empty newnodes || NodeSet.subset starts visited
  then None
  else
    let () = debug_format
        (fun () -> open_box 2;
          print_string "->->- Spanning... ";
          print_string "visited   = ";
          print_list ~print:print_int ~sep:"," (NodeSet.elements visited);
          print_space ();
          print_string "newvisits = ";
          print_list ~print:print_int ~sep:"," (NodeSet.elements newnodes);
          print_newline ();          close_box ()) in
    Some { ent with vfield=DVectField.union newvfld vfield;
                    visited=NodeSet.union newnodes visited}

let coherentSpan ~g ~starts entry =
  let rec recur entry =
    match coherentSpan1 ~g ~starts entry with
      None -> entry | Some entry -> recur entry in
  match coherentSpan1 ~g ~starts entry with
    None -> None | Some entry -> Some (recur entry)

let newSpanRoot ~g entry =
  let visited = entry.visited in
  let spannedFaces = inducedFaces entry.vfield in
  let spanNodes = Complex.fold
      (fun face ns -> NodeSet.add (FaceDG.encode ~g face) ns)
      spannedFaces NodeSet.empty in
  let is_restart ~src ~dsts =
    not (NodeSet.mem src visited) && NodeSet.mem src spanNodes in
  let restarts =
    List.fold_left
      (fun rets (src,dsts) ->
         if is_restart ~src ~dsts then NodeSet.add src rets else rets)
      NodeSet.empty (FaceDG.bindings ~g) in

  restarts

(* pruning vector field *)

let prune ~inputCpl vfld =
  let () = debug_message "DfmVectField.Prune" in

  let rec span ~spanned ~vfld restVF =  (* spanned = faces (support vfld) *)
    let spanable vec = Complex.mem_facet vec.tail spanned  in
    let spanVF, restVF = DVectField.partition spanable restVF in
    if DVectField.is_empty spanVF
    then vfld, restVF
    else
      let spanned = DVectField.fold
          (fun vec spn ->
             Complex.union_simpset (Complex.faces (support vec)) spn)
          spanVF spanned in
      let vfld = DVectField.union vfld spanVF in
      span ~spanned ~vfld restVF in
  span ~spanned:inputCpl ~vfld:DVectField.empty vfld

(* the best possible dfm. vect. field: the minimum one for the moment... *)
let sort_best_vfld vflds =
  let vflds = List.map
      (fun (maxv,vfld,_) -> DVectField.cardinal vfld, (maxv, vfld)) vflds in
  let sorted = List.sort (fun x y -> (fst x) - (fst y)) vflds in
  List.map snd sorted

(* Guess an acyclic coherent deformation vector space *)
(* bases = the list of the cone bases (relative to minv, dim=n-1), partitioned   *)
(* inputCpl = the starting complex at base (dim=n-1)  *)
(* throw exception VectFieldNotFound, when failed. *)

module Q = SimpleQueue

let guess ~minv ~cfam ~inputCpl =
  (* shrunken forest *)
  let base = List.fold_left
      (fun base (_,cpl) -> Complex.union_simpset base cpl) Complex.null cfam in
  let forest =
    let maxvs = List.fold_left
        (fun maxvs (maxv,_) -> Simplex.add maxv maxvs) Simplex.null cfam in
    shrink ~maxvs ~base:(Complex.linkv minv base) ~inputCpl in

  let () = debug_format (fun () ->
      open_box 0;
      print_string "[Forest Edges] "; print_newline ();
      print_list
        ~print:(fun (e1,e2) ->
          Printer.print_vispl e1; print_string "-"; Printer.print_vispl e2)
        ~sep:"," forest;
      force_newline (); close_box ()) in

  let bases = List.map (fun (_,cpl) -> Complex.linkv minv cpl) cfam in
  let g,ag = create_faceDigraph ~bases forest in
  let decodeV ~g = FaceDG.decode ~g in
  let decodeNSet ~g nset =
    NodeSet.fold (fun n cpl ->
        Complex.add_simplex (decodeV ~g  n) cpl)
      nset Complex.null in
  let encodeCpl cpl =
    Complex.fold (fun spl nodes ->
        (* let () = debug_format
            (fun () -> open_box 0;
              print_string "+ENCODING: ";
              Printer.print_vispl spl; print_newline (); close_box ()) in *)
        NodeSet.add (FaceDG.encode ~g spl) nodes) cpl NodeSet.empty in
  let allnodes = FaceDG.allnodes ~g in
  let goals = List.map
      (fun (maxv,cpl) ->
         let faces = Complex.all_faces (Complex.linkv minv cpl) in
         let goalfaces = Complex.filter
             (isGoal ~maxvs:(Simplex.of_vertex maxv)) faces in
         maxv, encodeCpl goalfaces)
      cfam in
  let goalcpl = List.fold_left
      (fun gcpl (_,cpl) -> Complex.union gcpl
          (Complex.filter (Simplex.exist_vertex Vertex.full_inout)
             (Complex.all_faces (Complex.linkv minv cpl))))
      Complex.null cfam in
  let starts = NodeSet.abstract
        (List.filter (fun n -> isStart ~inputCpl (decodeV ~g n)) allnodes) in
  let () = debug_format
      (fun () ->
         let goals = encodeCpl goalcpl in
         open_box 0;
         let nodesuffix n =
           if NodeSet.mem n goals then "#"
           else if NodeSet.mem n starts then "*"
           else "" in
        FaceDG.print_digraph ~nodesuffix g; print_newline ();
        close_box ()) in

  (* bfs search: backward from goal faces to start faces *)
  let maxdepth = List.length allnodes in

  let init_bfsentry ~g maxv nset =
    {maxv=maxv; root=decodeNSet ~g nset; depth=0;
     vfield=DVectField.empty; visited=nset} in

  let init_queue ~g cfam =
    List.fold_left
      (fun q (maxv,nset) -> Q.enqueue (init_bfsentry ~g maxv nset) q)
      Q.empty cfam in

  let rec bfs ~cands q =
    if Q.is_empty q then cands
    else begin
      let ent, q = Q.dequeue q in
      let depth = if ent.depth > maxdepth
        then raise (Failure "Too deep DfmVectField.guess.bfs iteration.")
        else ent.depth+1 in
      if NodeSet.subset starts ent.visited
      then bfs ~cands:(ent::cands) q (* register as a candidate *)
      else
        let () = debug_format
            (fun () -> open_box 0;
              print_string "->- Direct link. w/ maxv="; Printer.print_vivert ent.maxv;
              print_newline (); print_string "visited = ";
              print_list ~print:print_int ~sep:"," (NodeSet.elements ent.visited);
              force_newline (); close_box ()) in
        match coherentSpan ~g ~starts ent with
        | Some ent ->  bfs ~cands (Q.enqueue {ent with depth=depth} q)
        | None -> begin
            match softLink ~g ~visited:ent.visited with (* try softlink *)
            | None ->
              (* try restarting with new root, if no softlink *)
              let newroot = newSpanRoot ~g ent in
              if not (NodeSet.is_empty newroot)
              then
                let () = debug_format
                    (fun () -> open_box 0;
                      print_string "->- Contin. w/ newroots = ";
                      print_list ~print:print_int ~sep:"," (NodeSet.elements newroot);
                      force_newline (); close_box ()) in
                let rootcpl = NodeSet.fold
                    (fun nd cpl -> Complex.add_simplex (FaceDG.decode ~g nd) cpl)
                    newroot Complex.null in
                let newent =
                  { ent with depth=depth; root=rootcpl;
                             visited=NodeSet.union ent.visited newroot } in
                bfs ~cands (Q.enqueue newent q)
              else (* throw away the current bfs branch, if no softlink *)
                let () = debug_format
                    (fun () -> open_box 0;
                      print_string "-!- Discarded maxv="; Printer.print_vivert ent.maxv;
                      force_newline (); close_box ()) in
                bfs ~cands q
            | Some (v,src,dst) -> (* src=head; dst=tail *)
              let decvect = decodeVect ~g ~head:src ~tail:dst in
              let newent =
                { ent with depth=depth;
                           vfield=DVectField.add decvect ent.vfield;
                           visited=NodeSet.add src (NodeSet.add dst ent.visited) } in
              let () = debug_format
                  (fun () -> open_box 0;
                    print_string "->- Softlinking: ";
                    print_string "linkedFace="; print_int v; (* print_string ":"; *)
                    (* Printer.print_vispl (decodeV ~g v); *) print_space ();
                    print_string "dfm.Vector=";
                    print_int dst; print_string "~>"; print_int src; (* print_string ":";
                    print_dfmvect decvect; *) print_newline (); close_box ()) in
              bfs ~cands (Q.enqueue newent q) end
    end in

  let initq = init_queue ~g goals in
  let () = debug_format
      (fun () -> open_box 0;
        print_string "BFS search for a tree w/ leaves: ";
        print_IntSet starts; print_newline ();
        print_string "candidate roots are: ";
        print_list ~print:print_IntSet ~sep:","
          (List.map (fun ent -> ent.visited) (Q.elements initq)); print_newline ();
        close_box ()) in

  let cands = bfs ~cands:[] initq in
  let vflds = List.fold_left
      (fun cands {maxv=maxv; root=root; vfield =fat_vfld} ->
         let () = debug_format
             (fun () -> open_box 0;
               print_string "Pruning vect. fld for maxv=";
               Printer.print_vivert maxv; print_newline ();
               print_vectfld fat_vfld; print_newline (); close_box ()) in
         let vfld, restVF = prune ~inputCpl fat_vfld in
         let () = debug_format
             (fun () -> open_box 0;
               let n = DVectField.cardinal restVF in
               if n>0 then begin
                 print_string ("@ Pruned out "^
                               (string_of_int n)^" vectors:"); print_newline ();
                 print_list ~print:print_dfmvect ~sep:","
                   (DVectField.elements restVF); print_newline () end;
               close_box ()) in
         (maxv,vfld,fat_vfld)::cands)
      [] cands in
  let () = debug_format
      (fun () -> open_box 0;
        print_endline
          ("! "^(string_of_int (List.length cands))^" candidates of vect. flds.");
        List.iteri (fun i (maxv,x,fat) ->
            print_string "-------- #"; print_int (i+1);
            print_string " maxv="; Printer.print_vivert maxv;
            print_string " of size "; print_int (DVectField.cardinal x);
            print_string " pruned from of size "; print_int (DVectField.cardinal fat);
            force_newline ();
            (* print_string "Fat vect. fld. ->->-> ";
            print_vectfld fat; force_newline (); *)
            print_string "Vect. fld. (pruned)->->-> ";
            print_vectfld x;
            force_newline ())
          vflds;
        close_box ()) in

  sort_best_vfld vflds
