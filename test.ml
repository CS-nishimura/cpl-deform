#print_length 1000;;

#load "global.cmo"
#load "proc.cmo"
#load "helper.cmo"
#load "intSets.cmo"
#load "abstractSimplicialComplex.cmo"
#load "carrierMap.cmo"
#load "prioQueue.cmo"
#load "printer.cmo"
#load "graph.cmo"
#load "hyperGraphImpl.cmo"
#load "bipartite.cmo"
#load "setSystem.cmo"
#load "dfmVect.cmo"
#load "hyperGraph.cmo"

open Format

open Global
open Proc
open Helper
open IntSets
open AbstractSimplicialComplex
open SimplicialComplex
open CarrierMap
open SimplQueue
open HyperGraphImpl
open Bipartite
open SetSystem
open HyperGraph
open Printer

let () = debug:=true

let nul = 0 and uno =1
let black = "B" and white = "W"
let cvi0 = [inproc black nul]  let vi0 = Vertex.abstract cvi0
let cvi1 = [inproc white uno]  let vi1 = Vertex.abstract cvi1
let cvo0 = [outproc black nul]  let vo0 = Vertex.abstract cvi0
let cvo1 = [outproc white uno]  let vo1 = Vertex.abstract cvi1
let cvi01 = [inproc black nul; inproc white uno]  let vi01 = Vertex.abstract cvi01
let cvo01 = [outproc black nul; outproc white uno]  let vo01 = Vertex.abstract cvo01


(* ------------------------------ *)

(* let concrete_simp =  [ cvi0, [cvo0]; cvi1, [cvo1]; cvi01, [cvo01] ]

let cmap_simp = abstract_cmap concrete_simp *)

(*
let pred1 = List.concat (List.map (pred_map ~cmap:cmap_simp) max_simp)
let pred2 = List.concat (List.map (pred_map ~cmap:cmap_simp) pred1)
let pred3 = List.concat (List.map (pred_map ~cmap:cmap_simp) pred2)
*)

(* test for the first deformation step *)
(*
let complex = complex_simp
let minfixvs = List.hd minfixvss_simp
let cpl = init_complex

let partitions, complement = partition_complex ~complex ~minfixvs cpl

let (starv, minv, fixdst, pcpl)::_ = partitions
let moved=VS.null
let hist = []
let link = Complex.null
let star = pcpl
let retvs =
  let ret = retractable ~complex ~minv star in
    match fixdst with
      None -> ret
    | Some dstv ->
	List.filter (fun v -> Vertex.compare dstv v=0) ret
let vects = deform_vectors ~complex ~minv ~moved star
let (srcv,dstv)::_ = vects
let link, star = deformv ~minv ~srcv ~dstv link star
let moved = VS.add srcv moved
let hist = (Defm (srcv,dstv))::hist

let retvs =
  let ret = retractable ~complex ~minv star in
    match fixdst with
      None -> ret
    | Some dstv ->
	List.filter (fun v -> Vertex.compare dstv v=0) ret

let deformin1 = deformin ~complex ~minv ~fixdst ~moved ~hist ~link ~star

let (cont1, hist1)::_ = cont_simp;;

let () = Complex.visualize cont1

let () = visualize_hist hist1
*)

module Q=SimpleQueue

let add n q = Q.enqueue n q
let adds ns q = List.fold_left  (fun q n -> add n q) q ns

let qq = adds [11;15;12;4;8;13;19;14] Q.empty

let n1,qq1 = Q.dequeue qq
let n2,qq2 = Q.dequeue qq1
let n3,qq3 = Q.dequeue qq2
let n4,qq4 = Q.dequeue qq3
let n5,qq5 = Q.dequeue qq4
let n6,qq6 = Q.dequeue qq5

(*
let update15 n = if n=15 then Some 9 else None

let updateodd n = if n mod 2 =1 then Some (n/2) else None

let evq15 = Q.elevate ~update:update15 qq

let evqodd = Q.elevate ~update:updateodd qq

let m1,qqq1 = Q.dequeue evqodd
let m2,qqq2 = Q.dequeue qqq1
let m3,qqq3 = Q.dequeue qqq2
let m4,qqq4 = Q.dequeue qqq3
let m5,qqq5 = Q.dequeue qqq4
let m6,qqq6 = Q.dequeue qqq5


module ISet = Set.Make(struct type t=int let compare=compare end)
module IS = struct include ISet let compare_elt=Pervasives.compare end
module GI = DirectedGraph.Make(IS)

let graph =
  let nodes = ISet.add 0 (ISet.add 1 (ISet.add 2 ISet.empty)) in
  let pred ~src:u ~dst:v  = (u=1 && v=2) || (u=2 && v=1)
  in GI.create ~nodes ~pred
let srcs = [0; 1]
let treepath = GI.shortest_tree_path ~srcs ~graph
*)


(*
let g = HyperG.create ~dim:3 complex_simp
*)

type bprec =
  { maxsdr:int; sHall:bool }

let bptest ss =
  let edges = IntPowerSet.abstract ss in
  let hg = from_setsystem edges in
  let () = HyperG.summarize_hyperg ~name:"TEST" ~goals:EdgeSet.empty hg in
  let () = BipartG.alloc hg in
  let bg = BipartG.from_hypergraph hg in
  let maxsdr = BipartG.maximum_matching bg in
  let sHall = strongHall ~vertices:hg.vertices ~edges:hg.edges in
  {maxsdr=maxsdr; sHall=sHall}


let ss0 = [ [1;2];  [2;3];  [3;1]]
let ss1 = [ [1;2];  [2;3];  [3;1]; [1;4]]
let ss2 = [ [1;2];  [2;3];  [3;1]; [1;4;5]]

let ss8 = [ [1;2;11];  [2;3;12];  [3;1;13]]
let ss9 = [ [1;2];  [2;3];  [3;1;13]]

let sstest roots ss =
  let ss = IntPowerSet.abstract ss in
  let universe = IntPowerSet.unions ss in
  let roots = IntSet.abstract roots in
  let dmap = connSetsDist ~roots ss in
  let vdmap = connElemDist ~roots ~universe ~dmap in
  let dvs = sorted_distvs ~vdmap in
  open_box 0;
  print_dmap dmap; print_newline ();
  print_vdmap vdmap; print_newline ();
  (* print_vdmap dvs; print_newline (); *)
  close_box ()



(* let ss0 = [[1;2];[2;3;4];[3;1]]
let ps = IntPowerSet.abstract ss0
let hg = from_setsystem ps
let () = BipartG.alloc hg; !BipartG.bigraph
let bg = BipartG.from_hypergraph hg
let () = BipartG.strongHall bg
let hgs = HyperG.shrink_hyperforest hg
let () = IntPowerSet.visualize hgs.edges *)
(* let adjm = List.map (fun (u,vs) -> u, IntSet.elements vs)
              (BipartG.AdjUW.bindings bg.BipartG.adjUW) *)
(* let () = maxSDR ss0 *)

(* let ss1 = [[1;2;3]; [3;4]; [4;2]] *)
(* let () = List.mapi (fun i ss -> i, maxSDR ss)
  [
  [[1;2;3]; [3]] ;
  [[1;3]; [3;4]] ;
  [[1;2;3]; [2]] ;
  [[1;2]; [4;2]] ;
  [[3;4]; [4;2]] ;
  [[3;4]; [4]] ;
  [[4]; [4;2]]
  ] *)
(* let () = maxSDR ss1
let () = strongHall ss1
let () = !BipartG.bigraph
let ss2 = [[1;2]; [2;3;4]; [4;1]]
let () = maxSDR ss2
let () = strongHall ss2
let ss3 = [[1;2]; [2;4]; [4;1]; [1;2;4]]
let () = maxSDR ss3
let () = strongHall ss3
let ss4 = [[1;2]; [2;4]; [4;1]; [2;6]; [4;8]]
let () = maxSDR ss4
let () = strongHall ss4
let ss5 = [[1;2]; [2;4]; [4;3;1]; [1;3]; [3;2]]
let () = maxSDR ss5
let () = strongHall ss5
let ss6 = [[1;2]; [2;3;4]; [4;1]; [1;3]]
let () = maxSDR ss6
let () = strongHall ss6 *)
