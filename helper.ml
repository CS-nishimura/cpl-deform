(*
let rec combi ~i xs =
  match xs with
    [] -> if i<=0 then [[]] else []
  | a::ys -> if i<=0 then [[]]
	else List.append (combi ~i ys) (List.map (fun zs -> a::zs) (combi ~i:(i-1) ys))
*)

let is_empty_list = function [] -> true | _ -> false

let rec cprod xs ys =
  List.concat (List.map (fun x -> List.map (fun y -> x,y) ys) xs)

let rec lprod xss =
  match xss with
    [] -> [[]]
  | xs::yss ->
      List.concat (List.map (fun x -> List.map (fun ys -> x::ys) (lprod yss)) xs)

let rec all_pairs xs =
  match xs with
  | [] -> []
  | [_] -> []
  | [a;b] -> [a,b]
  | x::xs -> (List.map (fun y -> x,y) xs)@ (all_pairs xs)

let rec del1 = function
  [] -> []
  | [x] ->  [[]]
  | x::xs -> xs::(List.map (fun ys -> x::ys) (del1 xs))

let rec interval n m =  (* [n; ... m] *)
  if n > m then []
  else n::(interval (n+1) m)

let minimum_elem ~measure xs =
  let minimum mx ys = List.fold_left
      (fun (x,m) y -> let mm = measure y in if mm<m then y,mm else x,m)
      mx ys
  in
  match xs with
  | [] -> None
  | x::ys -> let e,_ = minimum (x,measure x) ys in Some e

let maximum_elem ~measure xs =
  let measure x = - (measure x) in
  minimum_elem ~measure xs

let rec combi xs =
  match xs with
  | [] -> [[]]
  | x::ys -> let yss = combi ys in
      yss@(List.map (fun xs -> x::xs) yss)

let rec perm xs =
  let rec insall a = function
  | [] -> [[a]]
  | b::xss -> (a::b::xss)::(List.map (fun xs -> b::xs) (insall a xss)) in
  match xs with
    [] -> [[]]
    | x::ys -> List.concat (List.map (insall x) (perm ys))

let segments ~n xs =
  let xa = Array.of_list xs in
  let len = Array.length xa in
  let intv = interval 0 (len-n)
  in  List.fold_right
        (fun i yss -> (Array.to_list (Array.sub xa i n))::yss)
        intv []

let rec compaction ~eq ~prior ds =
  let rec replace d ds =
    match ds with [] -> [d]
    | e::es -> if not (eq d e) then e::(replace d es) else (prior d e)::es
  in
  match ds with
    [] -> []
  | e::es -> replace e (compaction ~eq ~prior es)
