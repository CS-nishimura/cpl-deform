open Format

open Global
open Helper
open Proc
open AbstractSimplicialComplex
open SimplicialComplex
open IntSets
open Bipartite
open Graph
open SetSystem

open SimplQueue
module Q = SimpleQueue

(* Simplicial complex as a hyper graph *)
open HyperGraphImpl

exception NotShrunkableHyperG of hypergraph

let from_setsystem ps =
  let vertices = IntPowerSet.unions ps in
  let max_venc = IntSet.max_elt vertices
  in {vertices=vertices; edges=ps; roots=HyperEdge.empty;
      goals=EdgeSet.empty; goalfaces=HyperEdge.empty;
      encode=EncM.empty; decode=DecM.create ~size:0; max_venc=max_venc}



module type HyperGSig =
sig
  val create: startcpl:complex -> goalcpl:complex -> goalfcpl:complex -> complex -> hypergraph
  val hyperdeforest: hypergraph -> hypergraph
  val shrink_hyperforest: hypergraph -> hypergraph
  val summarize_hyperg: name:string -> goals:EdgeSet.t -> hypergraph -> unit
 end


module HyperG : HyperGSig =
  struct
    let summarize_hyperg ~name ~goals g =
      let print_edge e =
        print_IntSet e;
        if EdgeSet.exists (HyperEdge.subset e) goals then print_string "#"
      in
      open_box 0;
      print_string ("["^name^":: ");
      print_list ~print:print_int ~sep:"," (HyperEdge.elements g.vertices);
      print_string "]"; print_space ();
      print_string ((string_of_int (HyperEdge.cardinal g.vertices))^" vertices, "^
        (string_of_int (EdgeSet.cardinal g.edges))^" edges:");
      force_newline ();
      print_string "{";
      print_list
        ~print:print_edge ~sep:"," (EdgeSet.elements g.edges);
      print_string "}"; force_newline ();
      (* print_string "roots="; print_IntSet g.roots; force_newline (); *)
      close_box ()

    (* create edge-hypergraph *)
    let create ~startcpl ~goalcpl ~goalfcpl cpl =
    (* dim = dim cpl = 1+ dim startcpl, faces = skel^(dim-1) cpl *)
      let edges, encm, decm, fcount = encComplex cpl in
      let vertices = EdgeSet.unions edges in
      let roots = encHyperEdge ~encm startcpl in
      let goals = encEdgeSet ~encm goalcpl in
      let goalfaces = encHyperEdge ~encm goalfcpl in
      {vertices=vertices; edges=edges; roots=roots;
       goals=goals; goalfaces=goalfaces;
       encode=encm; decode=decm; max_venc=fcount-1}



    (* greedy search for hypertree in hypergraph *)
    let debug_cardinal_hyper ~fname vertices edges =
      let vertn = HyperEdge.cardinal vertices in
      let edgen = EdgeSet.cardinal edges in
      let prelude = "....." in
      debug_format (fun () ->
          print_string (prelude^" "^fname^":Verts=");
          print_int vertn;
          print_string ",Edges=";
          print_int edgen; force_newline ())

    let greedyforest ~vertices ~unseen forest =
      snd
        (EdgeSet.fold
          (fun e (vertices, forest) ->
            let () = debug_cardinal_hyper ~fname:"greedy" vertices forest in
            let newforest = EdgeSet.add e forest in
            let newverts = HyperEdge.union e vertices in
            if strongHall ~vertices:newverts ~edges:newforest
            then newverts,newforest else vertices,forest)
        unseen (vertices,forest))

    let rec expandforest ~vertices ~unseen ~subsumed forest =
      let () = debug_cardinal_hyper ~fname:"expand" vertices forest in
      let sub, unseen = EdgeSet.partition
            (fun e -> HyperEdge.subset e vertices) unseen in
      let subsumed = EdgeSet.union sub subsumed in
      if EdgeSet.is_empty unseen then (vertices, subsumed, forest)
      else
        let inter,nointer = EdgeSet.partition
              (fun e -> HyperEdge.disjoint e vertices) unseen in
        let newedge = if EdgeSet.is_empty inter
          then EdgeSet.choose nointer else EdgeSet.choose inter in
        let unseen = EdgeSet.remove newedge unseen in
        let forest = EdgeSet.add newedge forest in
        let vertices = HyperEdge.union newedge vertices
        in expandforest ~vertices ~unseen ~subsumed forest

    let hyperdeforest g =
      let () = BipartG.alloc g in
      let roots = g.roots in
      let unseen, forest =
        EdgeSet.partition (fun e -> HyperEdge.disjoint e roots) g.edges in
      let vertices = EdgeSet.unions forest in
      let () = assert(strongHall ~vertices ~edges:forest) in (* must be a hyperforest... *)
      let vertices, unseen, forest =
            expandforest ~vertices ~unseen ~subsumed:EdgeSet.empty forest in
      let () = assert(HyperEdge.subset g.vertices vertices) in (* every vetex should be covered *)

      let forest = greedyforest ~vertices ~unseen forest in
      let vertices = EdgeSet.unions forest in
      {g with vertices=vertices; edges=forest}

    (* shrinking hyperedges *)
    let rec shrink_loop ~vertices ~unseen ~forest ~comp edges =
      (* INV: unseen = U edges \ (initVerts u U forest) *)
      if EdgeSet.is_empty edges || HyperEdge.is_empty unseen
      then EdgeSet.union forest edges
      else
        let v = HyperEdge.fold    (* choose most prior to remove *)
            (fun v champ -> if comp v champ < 0 then v else champ)
            unseen (HyperEdge.choose unseen) in
        let deledges = EdgeSet.delv v edges in
        let delverts = HyperEdge.remove v vertices in
        if strongHall ~vertices:delverts ~edges:deledges
        then (* v is removable *)
          let subforest, edges = EdgeSet.partition is_forestedge deledges in
          let unseen = HyperEdge.diff (HyperEdge.remove v unseen)
                                      (EdgeSet.unions subforest) in
          let forest = EdgeSet.union forest subforest in
          shrink_loop ~vertices:delverts ~unseen ~forest ~comp edges
        else (* v is not removable *)
          shrink_loop ~vertices ~unseen:(HyperEdge.remove v unseen) ~forest ~comp edges

    let remove_incoherents ~vertices ~edges ~roots ~is_coherent =
      let startedges = EdgeSet.elements
          (EdgeSet.filter
             (fun e -> not (Edge.is_empty (Edge.inter e roots))) edges) in
      List.fold_left
        (fun (vertices,edges) e ->
           let rootvset = Edge.inter e roots in
           let () = assert (Edge.cardinal rootvset=1) in
           let rootv = Edge.choose rootvset in
           let incoherents = Edge.filter
               (fun v -> not (is_coherent ~rootv v)) (Edge.remove rootv e) in
           let restEdges = EdgeSet.remove e edges in
           let restVerts = EdgeSet.unions restEdges in
           let dele = Edge.fold
               (fun v e ->
                  let () = assert (Edge.mem v e) in
                  let dele = Edge.remove v e in
                  let deledges = EdgeSet.add dele restEdges in
                  let delverts = Edge.union restVerts dele in
                  if EdgeSet.for_all (fun e -> not (is_loopedge e)) deledges &&
                     strongHall ~vertices:delverts ~edges:deledges
                  then dele (* v is removable *)
                  else  (* otherwise *)
                    let () = debug_format
                        (fun () -> open_box 0;
                          print_string "!!!Warning: hypernode "; print_int v;
                          print_string " is incoherent w.r.t. "; print_int rootv;
                          print_string ", but not removable."; print_newline ();
                          close_box ()) in
                    e)
               incoherents e in
           Edge.union dele restVerts, EdgeSet.add dele restEdges)
        (vertices,edges) startedges

    let remove_lonely ~vertices ~edges ~roots =
      let occur v = EdgeSet.fold
            (fun e n -> if HyperEdge.mem v e then n+1 else n) edges 0 in
      let lonely = HyperEdge.filter
            (fun v -> let occ = occur v in begin assert(occ>0);
                if occ > 1 then false
                else EdgeSet.exists
                      (fun e -> HyperEdge.mem v e && not (is_forestedge e))
                    edges end)
            (HyperEdge.diff vertices roots) in
      List.fold_left
        (fun (vertices,edges) v ->
          let deledges = EdgeSet.delv v edges in
          let delverts = HyperEdge.remove v vertices in
          if EdgeSet.for_all (fun e -> not (is_loopedge e)) deledges &&
             strongHall ~vertices:delverts ~edges:deledges
          then (delverts,deledges) (* v is removable *)
          else (vertices,edges))   (* otherwise *)
        (vertices,edges) (HyperEdge.elements lonely)

    (* NO WAY! Not allowed to remove a vertex in multiple edges at once! *)
(*
    let remove_atonce ~vertices ~edges ~roots =
      let forest, hyper = EdgeSet.partition is_forestedge edges in
      let unseen = HyperEdge.diff
                (HyperEdge.diff vertices (EdgeSet.unions forest)) roots in
      let edges = shrink_loop ~vertices ~unseen ~forest ~comp:(fun x y ->0) hyper in
      let vertices = EdgeSet.unions edges
      in vertices,edges
 *)
    let remove_mostdistant ~roots ~goalfaces edges =
      let remove_v ~dmap ~d v =
        let () = debug_message ("remove_mostdistant: "^(string_of_int v)^
                                " of distant "^(string_of_int d)) in
        let offset = distOffset ~roots v in
        let vedges = Edges.elements
            (Edges.filter
               (fun e -> not (is_forestedge e) && Edge.mem v e
                         && d = (IntSetMap.find e dmap)+offset)
               edges) in
        let rec loop = function
          | [] -> None
          | e::others ->
            let deledge = Edge.remove v e in
            let deledges = EdgeSet.add deledge (EdgeSet.remove e edges) in
            let vertices = EdgeSet.unions deledges in
            let hall = strongHall ~vertices ~edges:deledges in
            let () = debug_format
                (fun () -> open_box 0;
                  print_string (if hall then "...Failed " else "...SUCCESS ");
                  print_string ("for removing "^(string_of_int v)^" in ");
                  print_IntSet e; force_newline ();
                  close_box ()) in
            if hall then Some deledges else loop others in
        loop vedges in

      let rec vsloop ~dmap ~d vs =
        match vs with
        | [] -> None
        | v::vs ->
          begin match remove_v ~dmap ~d v with
            | None -> vsloop ~dmap ~d vs
            | Some edges -> Some edges
          end in

      let rec eloop ~dmap distvs =
        match distvs with
        | [] -> None
        | (d,v)::others ->
          begin match vsloop ~dmap ~d v with
            | None -> eloop ~dmap others
            | Some edges -> Some edges
          end in

      if is_forest edges then Some edges
      else
        let universe = EdgeSet.unions edges in
        let dmap = connSetsDist ~roots edges in
        let vdmap = connElemDist ~roots ~universe ~dmap in
        let distvs = sorted_distvs ~vdmap in
        let () = debug_format
            (fun () -> open_box 0;
              print_string "=== distance: verts"; print_newline ();
              print_distvs distvs; print_newline ();
              close_box ()) in
        eloop ~dmap distvs

    let rec remove_distants_first ~roots ~goalfaces edges =
      let () = debug_format
          (fun () -> open_box 0;
            print_string "+++Trying to remove most distant in"; print_space ();
            print_IntPowerSet edges; print_newline (); close_box ()) in
      if is_forest edges then true, edges
      else
        match remove_mostdistant ~roots ~goalfaces edges with
        | None -> false, edges
        | Some newedges -> remove_distants_first ~roots ~goalfaces newedges


    let shrink_hyperforest g =
      let () = BipartG.alloc g in (* side effect *)
      let vertices=g.vertices and edges=g.edges and goals=g.goals in
      let roots = g.roots and goalfaces = g.goalfaces in
      let modhg ~vertices ~edges g = {g with vertices=vertices; edges=edges} in
      let edgeshg ~edges g =
        let vertices = EdgeSet.unions edges in modhg ~vertices ~edges g in

      let compare_coherence v1 v2 =
        let decode v = DecM.find v g.decode in
        DfmVect.compareFacets (decode v1) (decode v2) in


      (* hyperforest is shrunken with some heuristical order on vertices *)

      (* 1: First remove incoherent vertexes connected w/ root nodes *)
      let vertices,edges =
        let is_coherent ~rootv v = compare_coherence rootv v >= 0 in
        if not (is_forest edges) then
          let  vertices,edges =
            remove_incoherents ~vertices ~edges ~roots ~is_coherent in
          let () = debug_format (fun () ->
              summarize_hyperg ~name:"Remove incoherents " ~goals (modhg ~vertices ~edges g)) in
          vertices,edges
        else
          let () = debug_message "SKIPPED: [Remove incoherents]" in
          vertices,edges in

      (* 2: remove lonely vertices (i.e., belonging to a single edge), if possible. *)
      let vertices,edges =
        if not (is_forest edges) then
          let  vertices,edges = remove_lonely ~vertices ~edges ~roots in
          let () = debug_format (fun () ->
              summarize_hyperg ~name:"Remove Lonely " ~goals (modhg ~vertices ~edges g)) in
          vertices,edges
        else
          let () = debug_message "SKIPPED: [Remove Lonely]" in
          vertices,edges in

      let () = assert (strongHall ~vertices ~edges) in

      (* 2: eagerly try to remove vertices one by one, most distant first. *)
      let modif, newedges = remove_distants_first ~roots ~goalfaces edges in
      let g = edgeshg ~edges:newedges g in
      if not modif then raise (NotShrunkableHyperG g)
      else
        let () = debug_format (fun () ->
            summarize_hyperg ~name:"Remove one-by-one " ~goals g);
          assert (strongHall ~vertices:g.vertices ~edges:g.edges)
        in g

      (* 2: eagerly try to remove vertices in each edge one by one. *)
      (* let prio u v = - (compare_coherence u v) in
      let edges = remove_onebyone ~roots ~goalfaces
          ~comp:prio ~edgeshg:(fun edges -> edgeshg ~edges g) edges in
      let g = edgeshg ~edges g in
      let () =
        let vertices = EdgeSet.unions edges in
        let () = debug_format (fun () ->
            summarize_hyperg ~name:"Remove one-by-one " ~goals g) in
        let () = assert (strongHall ~vertices:g.vertices ~edges:g.edges) in ()
      in
      g *)
  end
