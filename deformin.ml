open Format

open Global
open Helper
open Proc
open AbstractSimplicialComplex
open SimplicialComplex
open DfmVect

module VF = DfmVectField.DVectField

exception NoVectField of vertex

exception CircularProjs  of vertex*complex*int*int
exception NonRetractable of vertex*complex
exception NonCollapsible of vertex*complex

let minvElim1 ~dim ~minv ~maxv ~vectField cplL =
  let () = if !verbose then begin
      open_box 0; print_string "%%% Vect. Field  -- maxv= ";
      Printer.print_vivert maxv; print_newline ();
      DfmVectField.print_vectfld vectField; print_newline ();
      close_box () end in

  (* iterated lateral projections *)
  let sorted_vfs = try DfmVectField.topological_sort vectField
    with DirectedGraph.FoundCircuit(v,u) ->
      raise (CircularProjs(minv,cplL,v,u)) in
  let cplL, cplK =List.fold_left
      (fun (cplLat, cplBase) vec ->
         let () = debug_format
             (fun () -> open_box 0;
               print_string "#Lateral proj. w/ minv= ";
               Printer.print_vivert minv; print_space ();
               print_string "along defm. vector"; print_newline ();
               print_dfmvect vec; print_newline ();
               close_box ()) in
         let () = assert (Complex.mem_facet
                            (Simplex.add minv vec.tail) cplLat) in
         let basecpl, projcpl = latProj ~minv ~cplLat vec in
         let () = assert (not (Complex.mem_facet
                                 (Simplex.add minv vec.tail) projcpl)) in
         let () = debug_format
             (fun () -> open_box 0;
                print_string "... add faces "; print_newline ();
                Printer.print_vicpl (Complex.diff projcpl cplLat);
                print_newline (); close_box ()) in
         projcpl, Complex.union basecpl cplBase)
      (cplL,Complex.null) sorted_vfs in
  let burrLat, nonburrLat = Complex.partition (Simplex.mem minv) cplL in

  let () = debug_format
      (fun () ->
         open_box 0; print_string "Burr laterals ="; print_newline ();
         open_hvbox 2; print_string   " {";
         List.iter (fun spl -> Printer.pp_vispl spl; print_break 2 0)
           (Complex.elements burrLat);
         close_box ();          print_string " }"; print_newline ();
         close_box ()) in

  (* iterated lateral retractions *)
  let latRetrdown ~dim ~latBase =
    let rec loop ~dim ~nextBase ~latBase =
      let () = debug_format
          (fun () -> open_box 0;
            print_string ("[Lateral retraction dim="^(string_of_int dim)^"]" );
            print_newline ();
            (* print_string " ==== laterals to be retracted: ";
            Printer.print_vicpl latBase; print_newline ();
            print_string "   ==== retracted laterals: ";
            Printer.print_vicpl nextBase; print_newline (); *)
            close_box ()) in
      if dim<=1 then latBase
      else if Complex.is_empty latBase
      then loop ~dim:(dim-1) ~nextBase:Complex.null ~latBase:nextBase
      else
        let retr, noretr = partitionRetractables ~latBase in
        let () = if is_empty_list retr
          then raise (NonRetractable (minv, Complex.joinv minv latBase)) in
        let nextBase, latBase =
          List.fold_left
            (fun (nb,nl) (coface,face) ->
               let nextfaces = Complex.remove_facet face (Complex.faces coface) in
               let () = debug_format
                   (fun () -> open_box 0;
                     print_string "@ removing face "; Printer.print_vispl face; print_space ();
                     print_string "of coface"; print_space ();
                     Printer.print_vispl coface;  print_newline ();
                     (* print_string "w/ new faces"; print_space ();
                     Printer.print_vicpl nextfaces; print_newline (); *)
                     close_box ()) in
               let latBase = Complex.remove_facet coface nl in
               let rmfaces = Complex.filter
                   (fun face -> not (Complex.mem face latBase)) nextfaces in
               Complex.union rmfaces nb, latBase)
            (nextBase,latBase) retr in
        loop ~dim ~nextBase ~latBase in
    loop ~dim ~nextBase:Complex.null ~latBase in

  (* final lateral collapse *)
  let () =
    if dim>1 then
      let latBase = Complex.linkv minv burrLat in
      let () = debug_format
          (fun () -> open_box 0;
            print_string "@@@ collapsing residue ";
            Printer.print_vicpl latBase; print_newline ();
            close_box ()) in
      let retrBase = latRetrdown ~dim ~latBase in
      let () = debug_format
          (fun () -> open_box 0;
            print_string "[Lateral collapse] ";
            Printer.print_vicpl retrBase; print_newline ();
            close_box ()) in
      let () = assert (Complex.dim retrBase=0) in
      let minc = Vertex.count_in minv in
      if Complex.forall_facets
          (fun spl -> Simplex.forall_vertices
              (fun v -> Vertex.count_in v <= minc) spl) retrBase
      then ()
      else
        raise (NonCollapsible (minv,Complex.joinv minv retrBase)) in
  Complex.union cplK nonburrLat

let minvElim ~dim ~minv ~cfam cplL =
  (* guess vector field *)
  let inputCpl = Complex.linkv minv cplL in
  let () = if !verbose then begin
      open_box 0; print_string "%%% minvElim: minv = ";
      Printer.print_vivert minv;
      print_string " FOR"; print_newline (); Printer.print_vicpl inputCpl;
      print_newline ();
      close_box () end in
  let vfcands = DfmVectField.guess ~minv ~cfam ~inputCpl in
  let vfcands = match vfcands with
    | [] -> []
    | vf::_ -> if !bestonly then [vf] else vfcands in
  let rec first_success vfcands =
    match vfcands with
    | [] -> raise (NoVectField minv)
    | (maxv,vectField)::rests ->
      begin try
        minvElim1 ~dim ~minv ~maxv ~vectField cplL
      with
      | CircularProjs (minv,cpl,u,v) ->
        let () = if !verbose then
            begin open_box 0;
              print_string "****XXXXX circular lateral projections for minv=";
              Printer.print_vivert minv; print_space ();
              print_string "maxv="; Printer.print_vivert maxv; print_newline ();
              print_string "Circular dependency btwn. nodes ";
              print_int u; print_string " and "; print_int v;
              print_newline (); close_box () end in
        first_success rests
      | NonRetractable (minv,cpl) ->
        let () = if !verbose then
            begin open_box 0;
              print_string "****XXXXX Non-retractable lateral faces for minv=";
              Printer.print_vivert minv; print_space ();
              print_string "maxv="; Printer.print_vivert maxv; print_newline ();
              open_hvbox 2; print_string   " {";
              List.iter (fun spl -> Printer.pp_vispl spl; print_break 2 0)
                (Complex.elements cpl);
              close_box (); print_string " }";
              print_newline (); close_box () end in
        first_success rests
      | NonCollapsible (minv,cpl) ->
        let () = if !verbose then
            begin open_box 0;
              print_string "****XXXXX Non-collapsible lateral for minv=";
              Printer.print_vivert minv; print_space ();
              print_string "maxv="; Printer.print_vivert maxv; print_newline ();
              open_hvbox 2; print_string   " {";
              List.iter (fun spl -> Printer.pp_vispl spl; print_break 2 0)
                (Complex.elements cpl);
              close_box (); print_string " }";
              print_newline (); close_box () end in
        first_success rests
      end in
  first_success vfcands

let search ~dim ~init_complex ~ccfam =
  (* let minCpl = CarrierCpl.from_CCFam ccfam in *)
  List.fold_left (fun cpl (minv,cfam) ->
      let () = debug_format (fun () ->
          print_string "[[[Deformation loop]]]"; print_space ();
          print_string "cpl being defmd.="; Printer.print_vicpl cpl; force_newline ();
          (* print_string "minv to be elimntd.="; Printer.print_vivert minv; *)
          force_newline ()) in

      (* let () = assert (Simplex.dim minv=0) in
      let minv = Simplex.minimum minv in *)
      if Complex.memv minv cpl
      then
        let cpl, stable = Complex.partition
            (fun spl -> Vertex.compare minv (Simplex.minimum spl) = 0) cpl in
        let dfm = minvElim ~dim ~minv ~cfam cpl in
        Complex.union_simpset stable dfm
      else cpl)
      init_complex ccfam
