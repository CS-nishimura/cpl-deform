(* A simple functional queue using a pair of lists *)

module type QueueCoreSig =
  sig
    type 'a t

    exception Empty

    val empty: 'a t
    val is_empty: 'a t -> bool
    val length: 'a t -> int

    val enqueue: 'a -> 'a t -> 'a t
    val peek: 'a t -> 'a
    val dequeue: 'a t -> 'a * 'a t
  end

module type SimpleQueueSig =
  sig
    include QueueCoreSig

    val map: f:('a -> 'a) -> 'a t -> 'a t
    val fold: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all: ('a -> bool) -> 'a t -> bool
    val elements: 'a t -> 'a list
    val length: 'a t -> int
  end

module SimpleQueue : SimpleQueueSig =
  struct
    type 'a t = { q: 'a list; revq: 'a list }

    exception Empty

    let empty = { q=[]; revq=[] }
    let is_empty = function {q=[]; revq=[]} -> true | _ -> false
    let length x = (List.length x.q)+(List.length x.revq)

    let enqueue a x = {x with revq=a::x.revq}

    let peek {q=q; revq=revq} =
      match q with
      | [] -> begin
        match revq with
        | [] -> raise Empty
        | _ -> let aq = List.rev revq in
          begin match aq with
            | a::_ -> a
            | _ -> assert false   (* NOT READCHABLE *)
          end
        end
      | a::q -> a

    let dequeue {q=q; revq=revq} =
      match q with
      | [] -> begin
        match revq with
        | [] -> raise Empty
        | _ -> let aq = List.rev revq in
          begin match aq with
            | a::q -> a, {q=q; revq=[]}
            | _ -> assert false   (* NOT READCHABLE *)
          end
        end
      | a::q -> a, {q=q; revq=revq}

    let map ~f {q=q; revq=revq} =
      {q=List.map f q; revq=List.map f revq}

    let fold f q c =
      let g x y = f y x in
      List.fold_left g (List.fold_left g c q.revq) q.q

    let for_all f q = fold (fun x b -> b && (f x)) q true

    let elements q = q.q @ (List.rev q.revq)

  end
