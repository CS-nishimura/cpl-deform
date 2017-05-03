open Format

open Global
open Helper
open Proc
open AbstractSimplicialComplex
open SimplicialComplex

module Vert = Process
module Spl = Vertex
module Cpl =
struct
  type t = Simplex.t
  let null = Simplex.null
  let mem spl = Simplex.exist_vertex (Spl.sub spl)
  let add = Simplex.add
  let filter = Simplex.filter
  let fold = Simplex.fold
  let partition = Simplex.partition

  let vertexSet spl = fold Spl.union spl Spl.empty
  let vertices spl = Spl.visualize (vertexSet spl)
  let colorSet = Simplex.colorSet
  let vstar p spl = filter (Spl.mem p) spl
end

let error f =
  tmpout (fun () -> open_box 0;
           print_string "Input not well-formed: "; f ();
           close_box ())

let wellformed ~cmap = ()
