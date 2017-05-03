open Format

open Global
open Helper
open Proc
open AbstractSimplicialComplex
open SimplicialComplex
open IntSets
open CarrierMap

let string_of_color color = color
let string_of_value = string_of_int

let print_proc {color=color; value=value} =
  print_string ((string_of_color color)^" "^(string_of_value value))

let rec split_io xs =
  match xs with
    [] -> [], []
  | x::ys -> let is, os = split_io ys in
    match x with
      I p -> p::is, os | O p -> is, p::os

let print_procs vertex =
  let is, os = split_io vertex in
  let print = print_proc and sep="," in
  begin
    open_hbox ();
    print_string "[";
    print_list ~print ~sep is; print_string "|";
    if List.length os>0 then
      begin print_space (); print_list ~print ~sep os end;
    print_string "]";
    close_box ()
  end

let pp_procs vertex =
  let is, os = split_io vertex in
  let print = print_proc and sep () = print_string ", " in
  begin
    open_hbox ();
    print_string "[";
    pp_list ~print ~sep is; print_string "|";
    if List.length os>0 then
      begin print_space (); pp_list ~print ~sep os end;
    print_string "]";
    close_box ()
  end

let print_simplex vertices =
  let print = print_procs and sep=";" in
  begin
    print_string "{ ";
    print_list ~print ~sep vertices;
    print_string " }";
  end

let pp_simplex vertices =
  let print = print_procs and sep () = print_string ";" in
  begin
    print_string "{ ";
    pp_list ~print ~sep vertices;
    print_string " }";
  end

let print_complex spls =
  let print = print_simplex and sep="," in
  begin
    print_string "{";
    print_list ~print ~sep spls; print_string "}";
  end

let print_vivert v = print_procs (Vertex.visualize v)
let print_vispl s = print_simplex (Simplex.visualize s)
let print_vicpl c = print_complex (Complex.visualize c)

let pp_vivert v = pp_procs (Vertex.visualize v)
let pp_vispl s = pp_simplex (Simplex.visualize s)


(*
let print_flowvert = function
    Fsrc -> print_string "s"
  | Fsink -> print_string "t"
  | Fvert v -> print_int v
  | Fset vs ->
      print_string "{";
      print_list ~print:print_int ~sep:"," vs;
      print_string "}"
let print_flowedge (v1,v2) =
  print_string "[";
  print_flowvert v1; print_string "->"; print_flowvert v2;
  print_string "]"

let print_flowgraph g =
  open_box 0;
    print_list ~print:print_flowedge ~sep:"," g;
  close_box ()

let print_collapse_path path =
  print_string "Collapse: ";
  print_vicpl path.dstfcs; print_space ();
  print_string "~>"; print_space ();
  print_vicpl path.srcfcs; force_newline ()

let print_collapse_sortedpath path =
  let print i p =
    print_string "("; print_int (i+1); print_string ") "; print_collapse_path p
  in List.iteri print path

let print_collapse_encpath path =
  print_IntSet path.dstfcs;
  print_string " ~> ";
  print_IntSet path.srcfcs; print_string ";"; print_space ()

let print_collapse_sorted_encpath pathtree =
  List.iter print_collapse_encpath pathtree

let print_minfixvs vs =
  let print (iv,opov) =
    print_procs iv (* (Vertex.visualize iv) *);
    (match opov with None -> () |
       Some ov -> print_string "->"; print_procs ov (* (Vertex.visualize ov) *))
  and sep="," in
  begin open_vbox 0; print_list ~print ~sep vs; close_box () end

let print_minfixvss vss =
  List.iteri (fun i vs ->
      open_box 0; print_int (i+1); print_string "th:";
      print_minfixvs vs; close_box (); force_newline ()) vss *)
