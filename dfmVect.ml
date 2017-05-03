open Format

open Global
open Helper
open Proc
open AbstractSimplicialComplex
open SimplicialComplex

module FaceMap = Map.Make(Simplex)

type vect = {tail: simplex; head: simplex}

let print_dfmvect vec =
  Printer.print_vispl vec.tail; print_string " -> "; Printer.print_vispl vec.head

let support vec = Simplex.union vec.tail vec.head

let maxInv spl = Simplex.fold
    (fun v (maxv,m) ->
       let n=Vertex.count_in v in if m<n then v,n else maxv,m)
    spl (Vertex.empty, -1)

let minInv spl = Simplex.fold
    (fun v (minv,m) ->
       let n=Vertex.count_in v in if m>n then v,n else minv,m)
    spl (Vertex.empty, !maxdim+2)

let compareFacets head tail = (* #_of_maxv(head) - #_of_maxv(tail) *)
  let _,maxhead = maxInv head and _,maxtail = maxInv tail in
  maxhead - maxtail
  (* let vtail = asymDiff tail head and vhead =asymDiff head tail in
  Vertex.count_in vhead - Vertex.count_in vtail *)

let is_coherentFace ~coface face =
  (* let () = debug_format
      (fun () -> open_box 0;
        print_string "... coherence coface & face"; print_newline ();
        Printer.print_vispl coface; print_newline ();
        Printer.print_vispl face; print_newline ();
        close_box ()) in *)
  let v =
    let vs = Simplex.diff coface face in
    let () = assert (Simplex.dim vs >=0) in
    Simplex.choose vs in
  let _, mx = maxInv face in
  mx >= Vertex.count_in v


(* lateral projection *)
let latProj ~minv ~cplLat vec =
  let skeldown spl = Simplex.fold
        (fun v cpl ->
          Complex.add_simplex (Simplex.delv v spl) cpl)
        spl Complex.null in
  let splK = support vec in
  let delspl = Simplex.add minv vec.tail in
  let () = assert (Complex.mem_facet delspl cplLat) in
  let cplLat = Complex.remove_facet delspl
      (Complex.union cplLat (Complex.joinv minv (skeldown splK))) in
  let () = assert (not (Complex.mem_facet delspl cplLat)) in
  Complex.of_simplex splK, cplLat

(* lateral retraction *)
let partitionRetractables ~latBase =
  let faceM =
    let reg face m =
      let freq = try FaceMap.find face m with Not_found -> 0 in
      FaceMap.add face (freq+1) m in
    Complex.fold
      (fun coface m -> Complex.fold reg (Complex.faces coface) m)
      latBase FaceMap.empty in
  let retrFaces = let is_freeface face freq = freq = 1 in
    List.map fst (FaceMap.bindings (FaceMap.filter is_freeface faceM)) in
  let coface face =
    let cofaces = Complex.filter (Simplex.sub face) latBase in
    let () = assert (Complex.facets cofaces = 1) in
    Complex.choose cofaces in
  let cofaceM = List.fold_left
      (fun m face ->
         let cof = coface face in
         let faces = try FaceMap.find cof m with Not_found -> []
         in FaceMap.add cof (face::faces) m)
      FaceMap.empty retrFaces in
  let retrcofaces = FaceMap.bindings cofaceM in
  let coherentFaces ~coface faces =
    let coherents = List.filter (is_coherentFace ~coface) faces in
    match coherents with
    | [] -> None
    | face::_ -> Some face in

  List.fold_left
    (fun (retr,noretr) (coface,faces) ->
       match coherentFaces ~coface faces with
       | None -> retr,noretr
       | Some face ->
         let () = assert (Complex.mem_facet coface noretr) in
         (coface,face)::retr, Complex.remove_facet coface noretr)
    ([],latBase) retrcofaces

(* deformation vector field *)
module DVectField = Set.Make(struct
  type t = vect
  let compare = compare end)
