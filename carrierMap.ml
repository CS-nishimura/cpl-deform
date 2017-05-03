open Helper
open Proc
open AbstractSimplicialComplex
open SimplicialComplex

(* A carrier map is represented by a list of pairs of
   input simplex and possible output simplices (with no failure) *)

module VSet = Simplex
module PreSimplex = Vertex

module CMap = Map.Make(PreSimplex)
module OrdT = Map.Make(PreSimplex)

let mk_iocmap cm =
  List.map (fun (i,os) ->
    List.map (fun v -> I v) i, List.map (List.map (fun v -> O v)) os) cm

let abstract_cmap cm =
  let concretecmap = mk_iocmap cm in
  List.fold_left
    (fun m (i,os) ->
      let ispl = PreSimplex.abstract i
      and ospls = List.map PreSimplex.abstract os in
      let newbind =
	      if CMap.mem ispl m then ospls@(CMap.find ispl m) else ospls in
      CMap.add ispl newbind m)
    CMap.empty concretecmap

let visualize_cmap = List.map
    (fun (i,o) -> PreSimplex.visualize i, PreSimplex.visualize o)

let map_dim cmap =
  CMap.fold (fun i _ n -> max (PreSimplex.cardinal i) n) cmap (-1)

let facet_map cmap =
  let dim = map_dim cmap in
  CMap.filter (fun i _ -> PreSimplex.cardinal i = dim) cmap

let cmap_dom cmap = List.map fst (CMap.bindings cmap)

let dom_facets cmap = cmap_dom (facet_map cmap)

let out_maxvs cmap =
  let fmap = facet_map cmap in
  CMap.fold
    (fun _ facet maxvs -> VSet.add (PreSimplex.proj_out facet) maxvs)
    fmap VSet.null

let subsume i o cmap =
  try
    let os = CMap.find i cmap in
    List.exists (fun out -> PreSimplex.sub o out) os
  with Not_found -> false
