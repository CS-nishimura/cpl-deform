open Format

open Global
open Helper
open Proc
open AbstractSimplicialComplex
open SimplicialComplex
open IntSets
open Graph

(* hypergraph data structure *)

(* int as encoding of simplices *)
module NodeEnc = OrdInt
(* map module for  simplex -> int *)
module EncM = Map.Make(Simplex)

(* map module for  int -> simplex *)
module type DecMSig =
  sig
    type t
    type value
    val create : size:int -> t
    val set : int -> value -> t -> t
    val find : int -> t -> value
    val bindings : t -> (int * value) list
  end
module DecM : DecMSig
with type value = simplex =
  struct
    type value = simplex
    type t = value array
    let create ~size = Array.make size Simplex.null
    let set k v m = m.(k) <- v; m
    let find k m =
        let spl = m.(k) in
        assert (not (Simplex.is_null spl)); spl
    let bindings m = List.mapi (fun i a -> i,a) (Array.to_list m)
  end

(* hyper graph with int encoding *)
module HyperEdge = Edge
module EdgeSet = Edges
type hypergraph =
  { vertices: HyperEdge.t; edges: EdgeSet.t;
    roots: HyperEdge.t; goals: EdgeSet.t; goalfaces: HyperEdge.t;
    encode: int EncM.t; decode: DecM.t; max_venc: int }

exception NoSpanningTree of EdgeSet.t

let print_encoding ~maxvs ~roots ~goalfaces binding =
  let print_id i = print_int i;
    if IntSet.mem i roots then print_string "*";
    if IntSet.mem i goalfaces then print_string "#" in
  let print (i,spl) = print_id i; print_string " = "; Printer.print_vispl spl
  in open_box 0;
  print_string "[Facet encodings] maxvs= "; Printer.print_vispl maxvs;
  print_newline ();
  print_list ~print ~sep:";" binding; print_newline ();
  close_box ()

(* let hypersize hg = hg.max_venc + (EdgeSet.cardinal hg.edges) *)

let decFacet ~decm i   = DecM.find i decm
let encFacet ~encm spl = try
    EncM.find spl encm
  with Not_found ->
    debug_format (fun () -> open_box 0;
        print_string "!!! HyperGraphImpl.encFacet has no encoding for ";
                   Printer.print_vispl spl; force_newline (); close_box ());
    assert false

let decHyperEdge ~decm iset = HyperEdge.fold
  (fun i cpl -> Complex.add_simplex (decFacet ~decm i) cpl) iset Complex.null
let encHyperEdge ~encm cpl = Complex.fold
  (fun spl iset -> HyperEdge.add (encFacet ~encm spl) iset) cpl HyperEdge.empty

let encEdgeSet ~encm cpl = Complex.fold
  (fun spl es ->
    let encSimplex = encHyperEdge ~encm (Complex.faces spl) in
    EdgeSet.add encSimplex es)
  cpl EdgeSet.empty

let decFace ~decm iset = Complex.common_simplex (decHyperEdge ~decm iset)
let encFacetM ~encm ~decm cpl =
  Complex.fold
    (fun spl (encm,decm,i) -> EncM.add spl i encm, DecM.set i spl decm, i+1)
    cpl (encm,decm,0)

let encComplex cpl =
  let faces = Complex.all_faces cpl in
  let size = Complex.facets faces in
  let encm, decm, fcount =
      encFacetM ~encm:EncM.empty ~decm:(DecM.create ~size) faces in
  let edges =
    Complex.fold
      (fun facet edges ->
        let ffaces = Complex.faces facet in
        let edge = encHyperEdge ~encm ffaces in
        EdgeSet.add edge edges)
      cpl EdgeSet.empty
  in (edges,encm,decm,fcount)
