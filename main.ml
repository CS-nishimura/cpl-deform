open Format

open Global
open Proc
open CarrierMap
open AbstractSimplicialComplex
open SimplicialComplex
open CarrierMap
open HyperGraphImpl
open HyperGraph

open Arg
open Filename


let () =
  let make_errmsg str = "Error: "^str in

  (* let shownth = ref 1 in *)
  let files = ref [] in
  let infile = ref "" in

  let keywords =
    [
    "-verbose",
     Set verbose,
    "Let it more talkative.";
    "-debug",
    Set debug,
    "Debug mode";
    "-best",
    Set bestonly,
    "Try only best deformation vector fields";
    ] in

  let errmsg = "Usage: "^Sys.argv.(0)^" [-verbose] [-debug] [-best] [infile]" in

    Arg.parse keywords (fun s -> files := s::!files) errmsg;
    try
      (* read task definition *)
      let ch =
        let len = List.length !files in
        if len=0 then (infile:="<stdin>"; stdin)
        else if len=1 then (infile:=List.hd !files; open_in !infile)
        else
        	begin
        	  prerr_endline errmsg; exit 1
        	end      in
      let bf = Lexing.from_channel ch in
      let colors, incmap =
        try  Cmapparser.main Cmaplexer.token bf
        with Failure s -> raise Parsing.Parse_error    in
      let () = close_in ch in

      let cmap = abstract_cmap incmap in
      let () = Checker.wellformed ~cmap in

      (* carrier complex family *)
      let ccfam = CarrierCpl.carrierCplFamily cmap in

      let init_complex = CarrierCpl.inputCpl cmap in
      let dim = Complex.dim init_complex in
      let () = if dim > !maxdim
        then raise (Failure ("Dimension (d="^(string_of_int dim)^") too high.")) in

      let () = if !verbose then begin
          open_box 0;
          List.iter (fun (minv,cf) -> open_box 0;
                      print_string "minv= ";
                      Printer.print_vivert minv; print_break 999 0;
                      List.iter (fun (maxv,cpl) ->
                          open_hvbox 4;
                          print_string "  maxv=";
                          Printer.print_vivert maxv; print_break 999 0;
                          open_hovbox 2; print_string   " {";
                          List.iter (fun spl -> Printer.print_vispl spl; print_break 2 0)
                            (Complex.elements cpl); print_string " }";
                          close_box ();  print_newline ();
                          close_box (); print_break 999 0) cf;
                      close_box ()) ccfam;
          print_string "*** initial complex (dim+1=";
          print_int (dim+1); print_string ") ***"; force_newline ();
          Printer.print_complex (Complex.visualize init_complex);
          force_newline ();
          close_box ()
        end in

      (** search solution by deformation **)
      try
        let cplK = Deformin.search ~dim ~init_complex ~ccfam in
          begin
            open_box 0;
            print_string "[::: Found solution :::]"; print_newline ();
            print_string "{ ";
            open_hovbox 2;
            List.iter
              (fun spl-> Printer.pp_vispl spl; print_break 0 0)
              (Complex.elements cplK);
            close_box ();
            print_string "}"; print_newline ();
            close_box ();
          end
      with
      | HyperGraph.NotShrunkableHyperG g -> begin open_box 0;
          print_endline "_o_ _o_ No solutions _o_ _o_";
          print_string "* Hypergraph not shrunkable"; print_space ();
          HyperG.summarize_hyperg ~name:"Unshrukable" ~goals:EdgeSet.empty g;
          force_newline (); close_box () end
      | Deformin.NoVectField minv -> begin open_box 0;
          print_endline "_o_ _o_ No solutions _o_ _o_";
          print_string "* Failed to find a deformation vector field";
          print_string " for eliminating the minimal vertex";
          print_space (); Printer.print_vivert minv;
          force_newline (); close_box () end

    with
    | Sys_error str -> prerr_endline (make_errmsg str); exit 1
    | Failure str -> prerr_endline (make_errmsg str); exit 1
    | Parsing.Parse_error ->
      prerr_endline
        ("Parse Error: "
         ^"around characters "
         ^(string_of_int !Locat.start_loc)
         ^"--"
         ^(string_of_int !Locat.end_loc)
         ^" in "^ !infile ); exit 1
