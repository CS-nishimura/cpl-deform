type binop = Oplus | Ominus | Omult | Omod
type exp = Evalue | Enum of int | Euminus of exp | Ebin of binop * exp * exp

let rec interp ~value exp =
  match exp with
  | Evalue  -> value
  | Enum n -> n
  | Euminus e -> - (interp ~value e)
  | Ebin(Oplus,e1,e2) -> (interp ~value e1) + (interp ~value e2)
  | Ebin(Ominus,e1,e2) -> (interp ~value e1) - (interp ~value e2)
  | Ebin(Omult,e1,e2) -> (interp ~value e1) * (interp ~value e2)
  | Ebin(Omod,e1,e2) -> (interp ~value e1) mod (interp ~value e2)
