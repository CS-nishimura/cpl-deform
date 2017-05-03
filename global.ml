open Format

(* global switches *)
let verbose = ref false
let debug = ref false
let bestonly = ref false

let cutoff_default = 32
let cutoff = ref cutoff_default

let maxdim = ref 9999  (* max dimension for convenience *)

(* debugging functions *)
let tmpout f =
  let ch = stdout in (*   let ch = stderr in *)
  set_formatter_out_channel ch; open_box 0;
  f (); flush ch; set_formatter_out_channel stdout; close_box ()


let warn_message s =
  tmpout (fun () -> open_box 0;
           print_string ("!!!Warning: "^s); print_newline (); close_box ())

let debug_message s =
  if !debug then
    tmpout (fun () -> open_box 0;
             print_string ("!!!DEBUG: "^s); print_newline (); close_box ())

let debug_int ?(mes=fun s -> s) i =
  if !debug then
    tmpout (fun () -> open_box 0;
      print_string ("!!!DEBUG: "^(mes (string_of_int i))); print_newline ();
      close_box ())

let debug_format f =  if !debug then tmpout f


(* global type definitions *)

(* local deformations: Retraction or Deformation *)
type ('v,'s) deformop = Retr of 'v * 'v | Defm of 's * 's
(* deformation result & history of local deformations *)
type ('v,'s,'c) deform_rec = {link:'c; hist: (('v,'s) deformop) list}

type flowV = Fsrc | Fsink | Fvert of int | Fset of int list

(* generic printing *)
let rec print_list ~print ~sep ps =
  match ps with
    [] -> ()
  | [p] -> print p
  | p::qs ->
    print p; print_string sep; print_space (); print_list ~print ~sep qs

let rec pp_list ~print ~sep ps =
  match ps with
    [] -> print_newline ()
  | [p] -> print p
  | p::qs ->
    print p; sep (); pp_list ~print ~sep qs
