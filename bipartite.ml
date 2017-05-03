open Format

open Global
open Helper
open IntSets
open SimplQueue
open HyperGraphImpl


(* maximum matching for bipartite graph *)

module BipartG =
struct
  (* bipartite graph G=(U,W) width edge map adj: U->P(W) *)
  type bipartG =
    { dummyv:int; infty:int; edge_minv:int; pair: int array; level: int array}

  let bigraph = ref { dummyv=0; infty=0; edge_minv=0;
                      pair = Array.make 0 0; level = Array.make 0 0 }

  (* The function alloc allocate a bipartite graph from hypergraph, and it *)
  (* must be called before any matching on a hypergraph and its derivatives. *)
  let alloc hypergraph =
    let vertsize = hypergraph.max_venc in
    let edgesize = EdgeSet.cardinal hypergraph.edges in
    let dummyv = vertsize+edgesize+1 in
    let infty = dummyv+2 in
    let pair = Array.make (dummyv+1) dummyv in
    let level = Array.make (dummyv+1) infty in
    bigraph := {dummyv=dummyv; infty=infty; edge_minv=vertsize+1;
                pair=pair; level=level }

  (* int encoding of intSet *)
  module EncU = IntSetMap (* mapping: intSet -> encoding *)
  module AdjUW = IntMap (* mapping: encoding -> intSet *)

  (* hypergraph H --> G=(U,W)=(E(H),V(H)) if |E(H)|<=|V(H)| *)
  (*                  G=(U,W)=(V(H),E(H)) if |V(H)|<|E(H)| *)
  type 'a encodingU = EncV of 'a | EncE of 'a
  type bipartitehyper=
    { vertU: IntSet.t; vertW: IntSet.t; adjUW: IntSet.t AdjUW.t;
      encUW: (int EncU.t) encodingU }

  let from_setspec ~vertices ~edges =
    let dummyv = !bigraph.dummyv in
    let vertV = vertices in
    let cdiff = (EdgeSet.cardinal edges)-(HyperEdge.cardinal vertV) in
    let n, vertE, encU, adjM =
      EdgeSet.fold (fun s (n, vertE, encU, adjM) ->
        n+1, HyperEdge.add n vertE, EncU.add s n encU, AdjUW.add n s adjM)
        edges (!bigraph.edge_minv,HyperEdge.empty,EncU.empty,AdjUW.empty) in
    let () = assert(n<=dummyv) in
    if cdiff <= 0 then
      { vertU=vertE; vertW=vertV; adjUW=adjM; encUW=EncE encU }
    else (* cdiff >0 *)
      let revadjM =
        IntSet.fold
          (fun v revadjM ->
            AdjUW.add v
              (IntSet.filter (fun e -> IntSet.mem v (AdjUW.find e adjM)) vertE)
              revadjM)
          vertV AdjUW.empty
      in { vertU=vertV; vertW=vertE; adjUW=revadjM; encUW=EncV encU }

  let from_hypergraph hypergraph =
    from_setspec ~vertices:hypergraph.vertices ~edges:hypergraph.edges

  module Q = SimpleQueue

  let init_matching ~pair initv =
    Array.iteri (fun i _ -> pair.(i)<-initv) pair

  let level_graph ~vertU ~adjUW ~dummyv ~infty ~pair ~level =
    let rec loop q =
      if not (Q.is_empty q) then
        let u, q = Q.dequeue q in
        if level.(u) >= level.(dummyv)
        then loop q
        else
          loop
            (IntSet.fold
              (fun v q ->
                let pair_v = pair.(v) in
                if level.(pair_v)=infty
                then (level.(pair_v)<-level.(u)+1; Q.enqueue pair_v q)
                else q)
              (AdjUW.find u adjUW) q)
    in
    let q = IntSet.fold (fun u q ->
              if pair.(u)=dummyv
              then (level.(u)<-0; Q.enqueue u q)
              else (level.(u)<-infty; q)) vertU Q.empty in
    let () = level.(dummyv)<-infty; loop q
    in level.(dummyv) <> infty

  let augmenting ~adjUW ~dummyv ~infty ~pair ~level u =
    let rec loop u =
      if u=dummyv then true
      else if
        IntSet.exists
          (fun v ->
            let pair_v = pair.(v) in
            if level.(pair_v)=level.(u)+1 && loop pair_v
            then (pair.(v)<-u; pair.(u)<-v; true) else false)
          (AdjUW.find u adjUW)
      then true
      else (level.(u)<-infty; false)
    in loop u


  (* maximum matching cardinality by Hopcroft-Karp algorithm *)
  let max_card ~vertU ~adjUW =
    let {dummyv=dummyv; infty=infty; edge_minv=edge_minv; pair=pair; level=level } = !bigraph in
    let () = init_matching ~pair dummyv in
    (* let prnbg () =
      for u=0 to dummyv do
        if pair.(u)<>dummyv then begin
        print_string ((string_of_int u)^"["^(string_of_int level.(u))^"]->");
        print_int pair.(u); print_string ";" end
      done; print_string "! " in *)
    let rec loop m = (* print_endline ("Trying match "^(string_of_int (m+1))); *)
      let lv = level_graph ~vertU ~adjUW ~dummyv ~infty ~pair ~level in
      if not lv then m
      else
        loop
          (IntSet.fold
            (fun u m -> (* prnbg (); print_endline (string_of_int u); *)
              if pair.(u)=dummyv && augmenting ~adjUW ~dummyv ~infty ~pair ~level u
              then m+1 else m)
            vertU m)
  in loop 0

  let maximum_matching {vertU=vertU; adjUW=adjUW} = max_card ~vertU ~adjUW

(* strong Hall == U-simplicity *)
    (* let strongHall {vertU=vertU;adjUW=adjUW} =
      let cardU = IntSet.cardinal vertU in
      AdjUW.for_all
        (fun u vs ->
           IntSet.for_all
             (fun v ->
                let adjUW = AdjUW.map (fun vs -> IntSet.remove v vs) (AdjUW.remove u adjUW) in
                let vertU = IntSet.remove u (IntSet.remove v vertU) in
                let maxmatch = max_card ~vertU ~adjUW
                in maxmatch >= cardU-1)
          vs)
      adjUW *)

end
