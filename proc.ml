type 'a inout = I of 'a | O of 'a

type color = string
type value = int

type proc_desc = {color: color; value: value}
type proc = proc_desc inout

type concrete_cmap = (proc_desc list * proc_desc list list) list

let mkproc color value = {color=color; value=value}
let inproc color value = I {color=color; value=value}
let outproc color value = O {color=color; value=value}

let color_of = function I desc -> desc.color | O desc -> desc.color
let is_input = function I _ -> true | O _ -> false


module ColorSet = Set.Make(struct type t=color let compare=compare end)

module Process =
  struct
    type t = proc
    let compare = compare

    type visible = t
    let visualize x = x
    let abstract x = x
  end
