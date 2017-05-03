(* memorizing location for lexing/parsing *)

let start_loc = ref 0 and end_loc = ref 0

let reset () = start_loc := 0; end_loc := 0
