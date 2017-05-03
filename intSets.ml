open Global

module OrdInt = struct type t=int let compare=compare end

module IntSet =
  struct
    include Set.Make(OrdInt)

    let mapint ~f set = fold
      (fun i s -> add (f i) s) set empty
    let empty_or_not set =
      if is_empty set then None
      else let a = choose set in let rest=remove a set in Some (a, rest)
    let induct ~empty ~nonempty set =
      if is_empty set then empty ()
      else let a = choose set in let rest=remove a set in nonempty a rest

    let disjoint s1 s2 = is_empty (inter s1 s2)

    let visualize = elements
    let abstract = List.fold_left (fun s x -> add x s) empty
  end

module IntPowerSet =
  struct
    include Set.Make(IntSet)

    let unions ps = fold IntSet.union ps IntSet.empty
    let inters ps = match  elements ps with
    | [] -> assert false
    | s0::others -> List.fold_left IntSet.inter s0 others
    let map ~f ps = fold
      (fun set ps -> add (f set) ps) ps empty
    let mapint ~f ps = fold
      (fun set ps -> add (IntSet.mapint ~f set) ps) ps empty
    let delv v ps = fold (fun set ps -> add (IntSet.remove v set) ps) ps empty
    let sum_cardinals ps =
      fold (fun s n -> n + (IntSet.cardinal s)) ps 0

    (* set partitioning by dfs *)
(*
    let set_partitions ~universe ps =
      let sets =
        let s = List.map (fun set -> IntSet.cardinal set, set) ps in
        let compare (n1,s1) (n2,s2) =
          if n1=n2 then IntSet.compare s1 s2<=0 else n1<n2 in
        List.map (fun (_,s) -> s) (Sort.list compare s) in
      let rec dfs ~part ~uni sets =
        if IntSet.equal uni universe then [part]
        else match sets with
        | [] -> []
        | set::rest ->
            let tryhead =
              if IntSet.is_empty (IntSet.inter set uni) then
                let part = set::part in
                let uni = IntSet.union set uni in
                dfs ~part ~uni rest
              else [] in
            tryhead@(dfs ~part ~uni rest)
      in dfs ~part:[] ~uni:IntSet.empty sets
 *)
    let visualize ps = List.map IntSet.visualize (elements ps)
    let abstract = List.fold_left
        (fun ps xs -> add (IntSet.abstract xs) ps) empty
  end

(* generic int/intSet map *)
module IntMap = Map.Make(OrdInt)
module IntSetMap = Map.Make(IntSet)

(* integer with +/-infinity *)
module InfInt =
struct
  type t = Bot | Top | Fin of int
  let compare x y =
    match x,y with
    | Bot, Bot -> 0
    | Top,Top -> 0
    | Bot, _ -> -1
    | Top, _ -> 1
    | _, Bot -> 1
    | _, Top -> -1
    | Fin n, Fin m -> n-m
end

(* printers *)

open Format

let print_IntSet_alt ~print s =
  print_string "{";
  print_list ~print ~sep:"," (IntSet.elements s);
  print_string "}"

let print_IntSet s = print_IntSet_alt ~print:print_int s

let print_IntPowerSet ps =
  print_string "{";
  print_list ~print:print_IntSet ~sep:"," (IntPowerSet.elements ps);
  print_string "}"

let print_InfInt n =
  match n with
  | InfInt.Bot -> print_string "-inf"
  | InfInt.Top -> print_string "+inf"
  | InfInt.Fin n -> print_int n
