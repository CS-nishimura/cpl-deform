(* directed ordered simplicial complex *)

open Proc
open Helper
open IntSets

module type VisibleOrderedType =
  sig
    type t
    type visible

    val compare: t -> t -> int
    val visualize : t -> visible
    val abstract : visible -> t
  end


module type PreVertexSig =
    sig
      include VisibleOrderedType

      type elt
      val empty: t
      val is_empty: t -> bool
      val cardinal: t -> int
      val singleton: elt -> t
      val sub: t -> t -> bool
      val mem: elt -> t -> bool
      val add: elt -> t -> t
      val union: t -> t -> t
      val diff: t -> t -> t
      val remove: elt -> t -> t
      val filter : (elt -> bool) -> t -> t
      val partition : (elt -> bool) -> t -> t*t
      val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
      val for_all : (elt -> bool) -> t -> bool
      val exists : (elt -> bool) -> t -> bool
    end

module type VertexSig =
    sig
      include PreVertexSig

      val colorSet: t -> ColorSet.t
      val proj_in: t -> t
      val proj_out: t -> t
      val count_in: t -> int
      val full_inout: t -> bool
    end

module MakePreVertex(V: VisibleOrderedType):  PreVertexSig
with type visible = V.t list
and  type elt = V.t =
  struct
    module PS = Set.Make(V)
    type t = PS.t
    type elt = V.t

    let compare s1 s2 =
      let c1 = PS.cardinal s1 and c2 = PS.cardinal s2 in
      if c1 < c2 then -1
      else if c2 < c1 then 1
      else PS.compare s1 s2

    let empty = PS.empty
    let is_empty = PS.is_empty
    let cardinal = PS.cardinal
    let sub = PS.subset

    let singleton = PS.singleton
    let mem = PS.mem
    let add = PS.add
    let union = PS.union
    let diff = PS.diff
    let remove = PS.remove

    let filter = PS.filter
    let partition = PS.partition
    let fold = PS.fold
    let for_all = PS.for_all
    let exists = PS.exists

    type visible = V.t list
    let visualize s = PS.elements s
    let abstract xs = List.fold_left (fun s e -> PS.add e s) PS.empty xs

  end


module type SimplicialComplexSig =
  sig
    type vertex
    type simplex
    type vset = simplex  (* synonym, but the structre is forgotten *)
    type complex
    type simpset = complex (* synonym, but the structre is forgotten *)

    module Simplex:
    sig
      type t = simplex
      type elt = vertex
      val compare : simplex -> simplex -> int
      val compare_elt : vertex -> vertex -> int
      (* val print: simplex -> unit *)

      val null : simplex
      val is_null : simplex -> bool
      val is_empty : simplex -> bool
      val of_vertex : vertex -> simplex
      val singleton: vertex -> simplex
      val of_edge : vertex -> vertex -> simplex
      val of_plane : vertex -> vertex -> vertex -> simplex
      val of_vertices : vertex list -> simplex
      val add : vertex -> simplex -> simplex
      val delv : vertex -> simplex -> simplex
      val delvs : vertex list -> simplex -> simplex
      val diff: simplex -> simplex -> simplex
      val union: vset -> vset -> vset
      val mem : vertex -> simplex -> bool
      val inter : simplex -> simplex -> simplex
      val filter : (vertex -> bool) -> simplex -> simplex
      val fold : (vertex -> 'a -> 'a) -> simplex -> 'a -> 'a
      val partition: (vertex -> bool) -> simplex -> simplex * simplex
      val forall_vertices: (vertex -> bool) -> simplex -> bool
      val exist_vertex: (vertex -> bool) -> simplex -> bool
      val choose: simplex -> vertex
      val sub : simplex -> simplex -> bool
      val simplicial_map : f:(vertex -> vertex) -> simplex -> simplex
      val vertexSet : simplex -> vset
      val vertices : simplex -> vertex list
      val elements : simplex -> vertex list
      val dim : simplex -> int
      val colorSet: simplex -> ColorSet.t
      val skel : int -> simplex -> complex
      val facets: simplex -> complex
      val minimum : simplex -> vertex
      val maximum : simplex -> vertex
      val null : simplex
      val add_virtual : simplex -> simplex
      val is_virtual : simplex -> bool

      type visible
      val visualize : simplex -> visible
      val abstract : visible -> simplex
    end

    module Complex :
    sig
      type t = complex
      type elt = simplex
      val compare : complex -> complex -> int
      val compare_elt : simplex -> simplex -> int

      val null : complex
      val of_simplex : simplex -> complex
      val of_vertex : vertex -> complex
      val is_null : complex -> bool
      val is_empty : complex -> bool
      val mem_facet : simplex -> simpset -> bool (* just a set mem *)
      val mem : simplex -> complex -> bool
      val memv : vertex -> complex -> bool
      val add_simplex : simplex -> simpset -> simpset (* just a set add *)
      val add : simplex -> complex -> complex
      val diff: complex -> complex -> complex
      val union : complex -> complex -> complex
      val from_facets : simplex list -> complex (* construct a complex from facets *)
      val union_simpset : simpset -> simpset -> simpset (* just a set union *)
      val inter_simpset : simpset -> simpset -> simpset (* just a set intersection *)
      val delv: vertex -> complex -> complex
      val delvs: vertex list -> complex -> complex
      val remove_facet: simplex -> complex -> complex
      val fold : (simplex -> 'a -> 'a) -> complex -> 'a -> 'a
      val sub : complex -> complex -> bool
      val facets_sub : simpset -> simpset -> bool  (* just set inclusion of facets *)
      val eq : complex -> complex -> bool
      val filter : (simplex -> bool) -> complex -> complex
      val partition : (simplex -> bool) -> complex -> complex * complex
      val map : f:(simplex -> simplex) -> complex -> complex
      val mapunion : f:(simplex -> complex) -> complex -> complex
      val joinv : vertex -> complex -> complex  (* join  v*complex, assuming complex does not contain v *)
      val forall_facets: (simplex -> bool) -> complex -> bool
      val exist_facets: (simplex -> bool) -> complex -> bool
      val choose: complex -> simplex
      val vertexSet : complex -> vset
      val vertices : complex -> vertex list
      val elements : complex -> simplex list
      val diffv: complex -> complex -> vertex list
      val diff_simplex: complex -> simplex -> complex
      val inter_simplex: complex -> simplex -> complex
      val common_simplex : complex -> simplex
      val colorSet: complex -> ColorSet.t
      val dim : complex -> int
      val facets : complex -> int
      val faces : simplex -> complex
      val all_faces : complex -> complex
      val skel : int -> complex -> complex
      val star : simplex -> complex -> complex
      val link : simplex -> complex -> complex
      val linkv : vertex -> complex -> complex
      val is_freeface : complex:complex -> simplex -> bool
      val minimum : complex -> simplex
      val maximum : complex -> simplex
      val minimal : complex -> vertex list
      val closure_of_simplex : simplex -> complex
      val closure : complex -> complex
      val union_simplex: complex -> simplex

      type visible
      val visualize : complex -> visible
      val abstract : visible -> complex
    end

  end


module Vertex: VertexSig
with type visible=Process.t list
and  type elt=Process.t   =
  struct
    include MakePreVertex(Process)

    let colorSet ps = fold
        (fun p cs -> ColorSet.add (color_of p) cs) ps ColorSet.empty

    let proj_in ps = filter is_input ps
    let proj_out ps = filter (fun p -> not (is_input p)) ps

    let count_in ps =
      fold
        (fun p n -> n + (if is_input p then 1 else 0))
        ps 0

    let full_inout ps =
      let ins,outs = partition is_input ps in
      cardinal ins = cardinal outs
  end

module SimplicialComplex : SimplicialComplexSig
with type vertex = Vertex.t
and  type Simplex.visible = Vertex.visible list
and  type Complex.visible = Vertex.visible list list =
  struct
    type vertex = Vertex.t

    (* A directed simplex is a set of _TOTALLY ordered_ vertices. *)
    module OrdSimplex =	Set.Make(Vertex)
    type simplex = OrdSimplex.t
    (* synonym, simplex whose structure is forgotten *)
    module VSet = OrdSimplex
    type vset = VSet.t

    (* A directed complex is the set of _nonempty FACETS_. *)
    module OrdComplex =	Set.Make(OrdSimplex)
    type complex = OrdComplex.t
    (* synonym, complex whose structure is forgotten *)
    module SimpSet = OrdComplex
    type simpset = SimpSet.t

    (* shorthand *)
    module S=OrdSimplex
    module C=OrdComplex

    (* functions for complex: transient *)
    let simplex_is_null = S.is_empty
    let null_complex = C.empty
    let complex_of_simplex = C.singleton
    let complex_mem spl cpl = C.exists (fun s -> S.subset spl s) cpl
    let complex_add spl cpl =
      if simplex_is_null spl || complex_mem spl cpl then cpl
      else C.fold
	  (fun s c -> if S.subset s spl then c else C.add s c)
	  cpl (complex_of_simplex spl)
    let complex_map ~f cpl =
      C.fold (fun spl c -> complex_add (f spl) c) cpl null_complex

    let simplex_abstract xs =
      List.fold_left (fun s e -> S.add (Vertex.abstract e) s) S.empty xs
    let complex_abstract xs =
      List.fold_left (fun s e -> C.add (simplex_abstract e) s) C.empty xs


    module Simplex =
      struct
    	type t = S.t
      	type elt = S.elt
      	let compare = S.compare
      	let compare_elt = Vertex.compare
        (* let print _ = print_string "%%% some simplex %%%"   *)

      	let null_simplex = S.empty (* transient *)
        let is_null = simplex_is_null
        let is_empty = simplex_is_null
      	let add = S.add
      	let delv = S.remove
      	let delvs vs spl = List.fold_left (fun s v -> delv v s) spl vs
      	let mem = S.mem
      	let inter = S.inter
      	let union = VSet.union  (* not necessarily an ordered simplex *)
      	let diff = S.diff
      	let filter = S.filter
       	let fold = S.fold
      	let forall_vertices = S.for_all
        let exist_vertex = S.exists
        let partition = S.partition
        let choose = S.choose

        let of_vertex = S.singleton
        let singleton = S.singleton
      	let of_edge v1 v2 = add v1 (of_vertex v2)
      	let of_plane v1 v2 v3 = add v1 (add v2 (of_vertex v3))
      	let of_vertices vs =
      	  List.fold_left (fun s v -> add v s) null_simplex vs

      	(* subsimplex? *)
      	let sub = S.subset

        let minimum = S.min_elt
        let maximum = S.max_elt

      	let simplicial_map ~f spl = fold (fun v s -> add (f v) s) spl null_simplex

      	let elements = S.elements
      	let vertexSet spl = spl
      	let vertices spl = elements (vertexSet spl)
        let dim spl = (S.cardinal spl) - 1

        let colorSet spl = S.fold
            (fun v cs -> ColorSet.union (Vertex.colorSet v) cs)
            spl ColorSet.empty

        let skel d spl =
          let rec sk d spl =
            if d=0 then complex_of_simplex null_simplex
            else try
              let v = S.choose spl in
              let subspl = delv v spl in
              C.union
                (sk d subspl)
                (C.fold
                  (fun spl cpl -> C.add (S.add v spl) cpl)
                  (sk (d-1) subspl) null_complex)
              with Not_found -> null_complex
          in
          if d<0 || (dim spl)<d then null_complex else sk (d+1) spl

      	let null = null_simplex

      	type visible = Vertex.visible list
      	let visualize spl = List.map Vertex.visualize (S.elements spl)
      	let abstract = simplex_abstract

        let facets spl =
          if is_empty spl then null_complex
          else complex_abstract (del1 (visualize spl))

        let add_virtual = add Vertex.empty
        let is_virtual = exist_vertex Vertex.is_empty
      end

    module Complex =
      struct
      	type t = C.t
        type elt = C.elt
      	let compare = C.compare
        let compare_elt = S.compare

        let null = null_complex
        let of_simplex = complex_of_simplex
        let singleton = complex_of_simplex
      	let of_vertex v = of_simplex (Simplex.of_vertex v)
        let is_null = C.is_empty
        let is_empty = C.is_empty
        let choose = C.choose

        let mem_facet = C.mem
      	let mem = complex_mem
      	let memv v cpl = C.exists (fun s -> Simplex.mem v s) cpl
        let add_simplex = C.add
      	let add = complex_add

      	let delv v cpl = C.fold
      	    (fun spl c -> let s = Simplex.delv v spl
      	    in if Simplex.is_null s then c else C.add s c) cpl null
      	let delvs vs cpl = C.fold
      	    (fun spl c -> let s = Simplex.delvs vs spl
      	    in if Simplex.is_null s then c else C.add s c) cpl null

        let remove_facet facet cpl = C.remove facet cpl

        let diff = C.diff
      	let union cp1 cp2 = C.fold add cp1 cp2

        let union_simpset = C.union
        let inter_simpset = C.inter

        let from_facets fs = List.fold_left
          (fun cpl spl -> add_simplex spl cpl) null fs
      	let forall_facets = C.for_all
      	let exist_facets = C.exists

      	let fold = C.fold

      	(* subcomplex? *)
      	let sub cp1 cp2 = C.for_all (fun s -> mem s cp2) cp1

      	let facets_sub = C.subset

      	let eq cp1 cp2 = C.compare cp1 cp2 = 0


      	let filter = C.filter
      	let partition = C.partition

      	let map = complex_map
        let mapunion ~f cpl = C.fold (fun spl c -> C.union (f spl) c) cpl null
        let joinv v cpl = complex_map ~f:(S.add v) cpl

      	let vertexSet cpl = C.fold S.union cpl S.empty
      	let vertices cpl = S.elements (vertexSet cpl)

        let elements = C.elements

      	let vertexSet cpl = C.fold S.union cpl S.empty

      	let diffv cpl1 cpl2 =
      	  S.elements (S.diff (vertexSet cpl1) (vertexSet cpl2))

      	let diff_simplex cpl spl =
      	  C.fold (fun s c -> add (Simplex.diff s spl) c) cpl null

      	let inter_simplex cpl spl =
      	  C.fold (fun s c -> add (Simplex.inter s spl) c) cpl null

      	let common_simplex cpl =
          let spl0 = C.choose cpl in
          C.fold (fun s i -> Simplex.inter s i) cpl spl0

      	let dim cpl = C.fold (fun spl n -> max n (Simplex.dim spl)) cpl (-1)

        let colorSet cpl = C.fold
            (fun spl cs -> ColorSet.union (Simplex.colorSet spl) cs)
            cpl ColorSet.empty

      	let facets = C.cardinal

      	let faces spl = (* skel ((dim spl) -1) spl *)
      	  Simplex.fold (fun v cpl -> add (Simplex.delv v spl) cpl) spl null

        let all_faces cpl = mapunion faces cpl

      	let skel d cpl = C.fold (fun spl c -> union (Simplex.skel d spl) c) cpl null

        let star tau cpl = filter (fun s -> Simplex.sub tau s) cpl

        let link tau cpl = map ~f:(fun s -> S.diff s tau) (star tau cpl)

      	let linkv v cpl = C.fold (fun spl c ->
              if Simplex.mem v spl
              then add_simplex (Simplex.delv v spl) c else c)
           cpl null_complex

        let is_freeface ~complex spl =
          let cofaces = C.fold
              (fun facet k -> if S.subset spl facet then k+1 else k)
              complex 0
          in cofaces=1

        let minimum = C.min_elt
        let maximum = C.max_elt

        let minimal cpl =
      	  S.elements
      	    (C.fold (fun spl xs -> S.add (Simplex.minimum spl) xs) cpl S.empty)

      	let closure_of_simplex spl =
      	  let d = Simplex.dim spl in
      	  let rec subsimplexes d spl =
      	    if d=0 then (of_simplex spl)
      	    else C.union (of_simplex spl)
      		(Simplex.fold (fun v c -> C.union (subsimplexes (d-1) (Simplex.delv v spl)) c) spl null)
      	  in if d=0 then null else subsimplexes d spl

      	let closure cpl = (* downward closure of a complex: what we usually call `complex'. *)
      	  C.fold (fun spl c -> C.union (closure_of_simplex spl) c) cpl null

      	let union_simplex cpl =
              (* union of all simplices in cpl -- not necessarily a simplex *)
      	  fold (fun s i -> VSet.union s i) cpl VSet.empty

      	type visible = Simplex.visible list
      	let visualize cpl = List.map Simplex.visualize (C.elements cpl)
        let abstract = complex_abstract
    end

  end
