open Format

open Global
open Helper
open IntSets
open Bipartite

(* strong Hall condition *)
let strongHall ~vertices ~edges =
  (* let () = debug_format
      (fun () -> open_box 0;
        print_string "! check strong Hall condition for ";
        print_int (Edge.cardinal vertices);
        print_string " vertices and  ";
        print_int (EdgeSet.cardinal edges);
        print_endline " edges.";       close_box ()) in *)
  let maxmatch ~vertices ~edges =
    let bg = BipartG.from_setspec ~vertices ~edges in
    BipartG.maximum_matching bg in
  (* let vn = IntSet.cardinal vertices in *)
  let en = IntPowerSet.cardinal edges in
  IntSet.for_all
    (fun v ->
       let vertices = IntSet.remove v vertices in
       let edges = IntPowerSet.map ~f:(IntSet.remove v) edges in
       let mx = maxmatch ~vertices ~edges in
       let hall = en<=mx in
(*
           let () = debug_format
               (fun () -> open_box 0;
                 print_int v;
                 if minve<=mx
                 then begin
                   print_string ".......Hall!!!! min(#v,#e)="; print_int en;
                   print_string "<= "; print_int mx
                 end
                 else begin
                   print_string "...not Hall min(#v,#e)="; print_int en;
                   print_string "> "; print_int mx
                 end;
                 print_string " of maximum match for ";
                 print_IntPowerSet edges; print_newline ();
                 close_box ()) in
 *)
       hall)
    vertices

let max_dist ss = 1 + (IntPowerSet.cardinal ss)

let connSetsDist ~roots ss =  (* connection distance map of set system *)
  let maxd = max_dist ss in
  let rec loop ~d ~reachv ~unvisited ~dmap =
    let () = assert (d <= maxd) in
    if IntPowerSet.is_empty unvisited then dmap
    else
      let newvisit = IntPowerSet.filter
          (fun s -> not (IntSet.disjoint reachv s)) unvisited in
      if IntPowerSet.is_empty newvisit then
        IntPowerSet.fold    (* enough large distance for unreachables *)
          (fun s m -> IntSetMap.add s maxd m) unvisited dmap
      else
        let d = d+1 in
        let dmap =
          IntPowerSet.fold
            (fun s m -> IntSetMap.add s d m) newvisit dmap in
        let reachv = IntSet.union reachv (IntPowerSet.unions newvisit) in
        let unvisited = IntPowerSet.diff unvisited newvisit in
        loop ~d ~reachv ~unvisited ~dmap in

  let unvisited, rootsets = IntPowerSet.partition (IntSet.disjoint roots) ss in
  let reachv = IntPowerSet.unions rootsets in
  let dmap = IntPowerSet.fold
      (fun s m -> IntSetMap.add s 0 m) rootsets IntSetMap.empty in
  loop ~d:0 ~reachv ~unvisited ~dmap

let distOffset ~roots v = if IntSet.mem v roots then 0 else 1

let setsDistOf ~roots ~dmap v =
  IntSetMap.fold
    (fun s d mx ->
       if IntSet.mem v s then
         let d = d + (distOffset ~roots v) in min d mx
       else mx)
    dmap max_int

let connElemDist ~roots ~universe ~dmap =
  let () = assert (not (IntSetMap.is_empty dmap)) in
  let vdmap = IntSet.fold
      (fun v m -> IntMap.add v (setsDistOf ~roots ~dmap v) m) universe IntMap.empty in
  vdmap

let sorted_distvs ~vdmap =  (* list of (dist * vertex list) w/ decreasing distance *)
  let maxd = IntMap.fold (fun _ d mx -> max d mx) vdmap 0 in
  if maxd=0 then [ 0, List.map fst (IntMap.bindings vdmap) ]
  else (* radix sort *)
    let a = Array.make (maxd+1) [] in
    let () = IntMap.iter (fun v d -> a.(d) <- v::a.(d)) vdmap in
    let _, ds = Array.fold_left
        (fun (d,xs) vs -> (d+1,(d,vs)::xs))
        (0,[]) a in
    ds

let print_dmap dmap =
  let print (ss,d) = print_string "e"; print_IntSet ss; print_string "~>"; print_int d in
  print_list ~print ~sep:"," (IntSetMap.bindings dmap)

let print_vdmap vdmap =
  let print (v,d) = print_string "v"; print_int v; print_string "~>"; print_int d in
  print_list ~print ~sep:"," (IntMap.bindings vdmap)

let print_distvs dvs =
  print_list ~print:(fun (d,vs) -> print_string "d="; print_int d; print_string ": {";
                      print_list ~print:print_int ~sep:";" vs;
                      print_string "}") ~sep:"," dvs
