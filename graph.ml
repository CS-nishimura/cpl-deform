open Format

open Global
open Helper
open IntSets
open SimplQueue

(* generic (normal/hyper/super) graphs *)

module Edge = IntSet
module Edges = IntPowerSet

let is_loopedge e = (Edge.cardinal e)<=1
let is_forestedge e = (Edge.cardinal e)<=2
let is_forestedge_strict e = (Edge.cardinal e)=2
let is_forest = Edges.for_all is_forestedge
let is_forest_strict = Edges.for_all is_forestedge_strict

let tree_of edges = Edges.filter is_forestedge_strict edges
let treeedge src dst = Edge.add src (Edge.singleton dst)

let is_incident v edge = Edge.mem v edge
let incident_edges ~edges v = Edges.filter (is_incident v) edges
let nbr ~edges v = Edge.remove v (Edges.unions (incident_edges ~edges v))

module VertMap = IntMap
module EdgeMap = IntSetMap

let depthM ~init ~roots edges =
  let rec rank_f ~visited ~r ~m children =
    List.fold_left
      (fun (visited,m) v ->
        let m = VertMap.add v r m in
        rank_t ~visited ~r:(r+1) ~m v)
      (visited,m) children
  and rank_t ~visited ~r ~m v =
    (* let () = debug_int ~mes:(fun v -> "GraphSpan.depthM.rank_t visits "^v) v in *)
    let nbrs = nbr ~edges v in
    let children = Edge.diff nbrs visited in
    let visited = Edge.union children visited in
    rank_f ~visited ~r ~m (Edge.elements children)
  in
  let _,m = rank_f ~visited:roots ~r:init
              ~m:VertMap.empty (Edge.elements roots) in
  m

let level depm = VertMap.fold (fun _ -> max) depm 0

let height ~roots edges =
  let m = depthM ~init:0 ~roots edges in
  level m
