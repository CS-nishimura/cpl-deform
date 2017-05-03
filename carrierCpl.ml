open Helper
open Proc
open AbstractSimplicialComplex
open SimplicialComplex

(* Construct carrier complex from carrier map Vertex -> Set(Vertex) *)
open CarrierMap

let saturated_chain ~minv ~facet ospl =
  let diffv = Vertex.diff facet minv in
  let elms = (Vertex.visualize diffv)@(Vertex.visualize ospl) in
  let rec accuml ~pred vs = match vs with
  | [] -> [Vertex.abstract pred]
  | v::us -> (Vertex.abstract pred)::(accuml ~pred:(v::pred) us) in
    let vss = List.map
      (fun vs -> List.map (Vertex.union minv) (accuml ~pred:[] vs))
      (perm elms)
  in List.map Simplex.of_vertices vss

let ordMinMaxCpl ~in_facets ~minv ~maxv =  (* minv:inputOnly, maxv:outputOnly of equal dim. *)
  Complex.from_facets
    (List.concat
       (List.map
          (fun facet ->
             let facet = PreSimplex.union facet maxv in
             saturated_chain ~minv ~facet PreSimplex.empty)
          in_facets))

(* family of carrier subcomplexes: list of (minv, (maxv, list of MinMaxfacets)) *)
let carrierCplFamily cmap =
  let in_facets = dom_facets cmap in
  List.map
    (fun (minv,outvs) ->
       let outs =
         let in_facets = List.filter (Vertex.sub minv) in_facets in
         let maxvs = List.map (Vertex.union minv)  outvs in
         List.map
           (fun maxv -> maxv, ordMinMaxCpl ~in_facets ~minv ~maxv)
           maxvs in
       minv, outs)
    (CMap.bindings cmap)

let inputCpl cmap =
  let in_facets = dom_facets cmap in
  let spls = List.concat
      ((List.map
          (fun facet ->
             let minvs =
               PreSimplex.fold
                 (fun p vs -> (PreSimplex.singleton p)::vs) facet [] in
             List.concat
               (List.map
                  (fun minv -> saturated_chain ~minv ~facet PreSimplex.empty)
                  minvs)))
         in_facets) in
  Complex.from_facets spls
