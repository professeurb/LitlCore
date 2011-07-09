(************************************************************************
*
*  litlDeque.ml
*  
*
*  Created by Olivier Brunet on 19 Jun 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
*
************************************************************************)

module List = struct 
  include List
  let cons a b = a :: b
end

module Node = struct 
  type 'a t = Node2 of 'a * 'a | Node3 of 'a * 'a * 'a

  let fold_right f n z = begin 
	  match n with
		  Node2 (a, b) -> f a (f b z)
		| Node3 (a, b, c) -> f a (f b (f c z))
  end

  let fold_left f z n = begin 
	  match n with
		  Node2 (b, a) -> f (f z b) a
		| Node3 (c, b, a) -> f (f (f z c) b) a
  end

  let iter f n = begin 
    match n with
      Node2 (a, b) -> f a ; f b
    | Node3 (a, b, c) -> f a ; f b ; f c
  end
end	

module Digit = struct 
  type 'a t =
    One of 'a
  | Two of 'a * 'a
  | Three of 'a * 'a * 'a
  | Four of 'a * 'a * 'a * 'a

  let fold_right f n z = begin 
	  match n with
	    One a -> f a z
	  | Two (a, b) -> f a (f b z)
	  | Three (a, b, c) -> f a (f b (f c z))
	  | Four (a, b, c, d) -> f a (f b (f c (f d z)))
	end

  let fold_left f z n = begin 
	  match n with
	    One a -> f z a
	  | Two (b, a) -> f (f z b) a
	  | Three (c, b, a) -> f (f (f z c) b) a
	  | Four (d, c, b, a) -> f (f (f (f z d) c) b) a
	end

  let iter f n = begin 
	  match n with
	    One a -> f a
	  | Two (a, b) -> f a ; f b
	  | Three (a, b, c) -> f a ; f b ; f c
	  | Four (a, b, c, d) -> f a ; f b ; f c ; f d
	end

  let from_node n = begin 
    match n with
      Node.Node2 (a, b) -> Two (a, b)
    | Node.Node3 (a, b, c) -> Three (a, b, c)
  end
end

type 'a digit = 'a Digit.t
type 'a node = 'a Node.t
type 'a t = Empty | Single of 'a | Deep of 'a digit * 'a node t * 'a digit

let empty = Empty

let is_empty t = begin 
  match t with
    Empty -> true
  | _ -> false
end

let rec fold_right : 'a 'b . ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b = begin 
  fun f t z -> match t with
    Empty -> z
  | Single x -> f x z
  | Deep (pr, m, sf) ->
	    Digit.fold_right f pr (
		    fold_right (Node.fold_right f) m (Digit.fold_right f sf z)
		)
end	

let rec fold_left : 'a 'b . ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a = begin 
  fun f z t -> match t with
    Empty -> z
  | Single x -> f z x
  | Deep (pr, m, sf) ->
	    Digit.fold_left f (
		    fold_left (Node.fold_left f) (Digit.fold_left f z pr) m
		) sf
end

let rec iter : 'a . ('a -> unit) -> 'a t -> unit = begin 
  fun f t -> match t with
    Empty -> ()
  | Single x -> f x ;
  | Deep (pr, m, sf) ->
      Digit.iter f pr ;
      iter (fun n -> Node.iter f n) m ;
      Digit.iter f sf
end

let rec consl : 'a . 'a -> 'a t -> 'a t = begin 
  fun a ft -> 
	  match ft with
	    Empty -> Single a
	  | Single b -> Deep (Digit.One a, Empty, Digit.One b)
	  | Deep (Digit.Four (b, c, d, e), m, sf) ->
        Deep (Digit.Two (a, b), (consl (Node.Node3 (c, d, e)) m), sf)
	  | Deep (Digit.One b, m, sf) -> Deep (Digit.Two (a, b), m, sf)
	  | Deep (Digit.Two (b, c), m, sf) -> Deep (Digit.Three (a, b, c), m, sf)
	  | Deep (Digit.Three (b, c, d), m, sf) ->
        Deep (Digit.Four (a, b, c, d), m, sf)
end

let rec consr : 'a . 'a t -> 'a -> 'a t = begin 
  fun ft a -> 
	  match ft with
	    Empty -> Single a
	  | Single b -> Deep (Digit.One b, Empty, Digit.One a)
	  | Deep (pr, m, Digit.Four (e, d, c, b)) ->
        Deep (pr, (consr m (Node.Node3 (e, d, c))), Digit.Two (b, a))
	  | Deep (pr, m, Digit.One b) -> Deep (pr, m, Digit.Two (b, a))
	  | Deep (pr, m, Digit.Two (c, b)) -> Deep (pr, m, Digit.Three (c, b, a))
	  | Deep (pr, m, Digit.Three (d, c, b)) ->
        Deep (pr, m, Digit.Four (d, c, b, a))
end

let from_list s = List.fold_right consl s Empty ;;
let to_list s = fold_right List.cons s [] ;;

let rec next : 'a . 'a t -> ('a * 'a t) option = begin 
  fun ft ->
	  match ft with
	    Empty -> None
	  | Single x -> Some (x, Empty)
	  | Deep (Digit.One a, m, sf) ->
		    Some (a, deepl m sf)
	  | Deep (Digit.Two (a, b), m, sf) ->
		    Some (a, Deep ((Digit.One b), m, sf))
	  | Deep (Digit.Three (a, b, c), m, sf) ->
		    Some (a, Deep (Digit.Two (b, c), m, sf))
	  | Deep (Digit.Four (a, b, c, d), m, sf) ->
		    Some (a, Deep (Digit.Three (b, c, d), m, sf))
end
and deepl : 'a . ('a Node.t) t -> 'a Digit.t -> 'a t = begin 
	fun m sf ->  
	  match next m with
        None -> Digit.fold_right consl sf Empty
      | Some (a, m') -> Deep (Digit.from_node a, m', sf)
end

let rec nextr : 'a . 'a t -> ('a t * 'a) option = begin 
  fun ft ->
	  match ft with
	    Empty -> None
	  | Single x -> Some (Empty, x)
	  | Deep (pr, m, Digit.One a) ->
		    Some (deepr pr m, a)
	  | Deep (pr, m, Digit.Two (b, a)) ->
		    Some (Deep (pr, m, (Digit.One b)), a)
	  | Deep (pr, m, Digit.Three (c, b, a)) ->
		    Some (Deep (pr, m,Digit.Two (c, b)), a)
	  | Deep (pr, m, Digit.Four (d, c, b, a)) ->
		    Some (Deep (pr, m, Digit.Three (d, c, b)), a)
end
and deepr : 'a . 'a Digit.t -> ('a Node.t) t -> 'a t = begin 
	fun pr m ->  
	  match nextr m with
        None -> Digit.fold_left consr Empty pr
      | Some (m', a) -> Deep (pr, m', Digit.from_node a)
end

let nodes d1 m d2 = begin 
	match d1, m, d2 with
	Digit.One a, Digit.One b, Digit.One c ->
	    Digit.One (Node.Node3 (a, b, c))
	| Digit.One a, Digit.One b, Digit.Two (c, d) ->
	    Digit.Two (Node.Node2 (a, b), Node.Node2 (c, d))
	| Digit.One a, Digit.One b, Digit.Three (c, d, e) ->
	    Digit.Two (Node.Node3 (a, b, c), Node.Node2 (d, e))
	| Digit.One a, Digit.One b, Digit.Four (c, d, e, f) ->
	    Digit.Two (Node.Node3 (a, b, c), Node.Node3 (d, e, f))
	| Digit.One a, Digit.Two (b, c), Digit.One d ->
	    Digit.Two (Node.Node2 (a, b), Node.Node2 (c, d))
	| Digit.One a, Digit.Two (b, c), Digit.Two (d, e) ->
	    Digit.Two (Node.Node3 (a, b, c), Node.Node2 (d, e))
	| Digit.One a, Digit.Two (b, c), Digit.Three (d, e, f) ->
	    Digit.Two (Node.Node3 (a, b, c), Node.Node3 (d, e, f))
	| Digit.One a, Digit.Two (b, c), Digit.Four (d, e, f, g) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node2 (d, e), Node.Node2 (f, g))
	| Digit.One a, Digit.Three (b, c, d), Digit.One e ->
	    Digit.Two (Node.Node3 (a, b, c), Node.Node2 (d, e))
	| Digit.One a, Digit.Three (b, c, d), Digit.Two (e, f) ->
	    Digit.Two (Node.Node3 (a, b, c), Node.Node3 (d, e, f))
	| Digit.One a, Digit.Three (b, c, d), Digit.Three (e, f, g) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node2 (d, e), Node.Node2 (f, g))
	| Digit.One a, Digit.Three (b, c, d), Digit.Four (e, f, g, h) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node2 (g, h))
	| Digit.One a, Digit.Four (b, c, d, e), Digit.One f ->
	    Digit.Two (Node.Node3 (a, b, c), Node.Node3 (d, e, f))
	| Digit.One a, Digit.Four (b, c, d, e), Digit.Two (f, g) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node2 (d, e), Node.Node2 (f, g))
	| Digit.One a, Digit.Four (b, c, d, e), Digit.Three (f, g, h) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node2 (g, h))
	| Digit.One a, Digit.Four (b, c, d, e), Digit.Four (f, g, h, i) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node3 (g, h, i))
	| Digit.Two (a, b), Digit.One c, Digit.One d ->
	    Digit.Two (Node.Node2 (a, b), Node.Node2 (c, d))
	| Digit.Two (a, b), Digit.One c, Digit.Two (d, e) ->
	    Digit.Two (Node.Node3 (a, b, c), Node.Node2 (d, e))
	| Digit.Two (a, b), Digit.One c, Digit.Three (d, e, f) ->
	    Digit.Two (Node.Node3 (a, b, c), Node.Node3 (d, e, f))
	| Digit.Two (a, b), Digit.One c, Digit.Four (d, e, f, g) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node2 (d, e), Node.Node2 (f, g))
	| Digit.Two (a, b), Digit.Two (c, d), Digit.One e ->
	    Digit.Two (Node.Node3 (a, b, c), Node.Node2 (d, e))
	| Digit.Two (a, b), Digit.Two (c, d), Digit.Two (e, f) ->
	    Digit.Two (Node.Node3 (a, b, c), Node.Node3 (d, e, f))
	| Digit.Two (a, b), Digit.Two (c, d), Digit.Three (e, f, g) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node2 (d, e), Node.Node2 (f, g))
	| Digit.Two (a, b), Digit.Two (c, d), Digit.Four (e, f, g, h) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node2 (g, h))
	| Digit.Two (a, b), Digit.Three (c, d, e), Digit.One f ->
	    Digit.Two (Node.Node3 (a, b, c), Node.Node3 (d, e, f))
	| Digit.Two (a, b), Digit.Three (c, d, e), Digit.Two (f, g) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node2 (d, e), Node.Node2 (f, g))
	| Digit.Two (a, b), Digit.Three (c, d, e), Digit.Three (f, g, h) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node2 (g, h))
	| Digit.Two (a, b), Digit.Three (c, d, e), Digit.Four (f, g, h, i) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node3 (g, h, i))
	| Digit.Two (a, b), Digit.Four (c, d, e, f), Digit.One g ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node2 (d, e), Node.Node2 (f, g))
	| Digit.Two (a, b), Digit.Four (c, d, e, f), Digit.Two (g, h) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node2 (g, h))
	| Digit.Two (a, b), Digit.Four (c, d, e, f), Digit.Three (g, h, i) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node3 (g, h, i))
	| Digit.Two (a, b), Digit.Four (c, d, e, f), Digit.Four (g, h, i, j) ->
	    Digit.Four (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node2 (g, h), Node.Node2 (i, j))
	| Digit.Three (a, b, c), Digit.One d, Digit.One e ->
	    Digit.Two (Node.Node3 (a, b, c), Node.Node2 (d, e))
	| Digit.Three (a, b, c), Digit.One d, Digit.Two (e, f) ->
	    Digit.Two (Node.Node3 (a, b, c), Node.Node3 (d, e, f))
	| Digit.Three (a, b, c), Digit.One d, Digit.Three (e, f, g) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node2 (d, e), Node.Node2 (f, g))
	| Digit.Three (a, b, c), Digit.One d, Digit.Four (e, f, g, h) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node2 (g, h))
	| Digit.Three (a, b, c), Digit.Two (d, e), Digit.One f ->
	    Digit.Two (Node.Node3 (a, b, c), Node.Node3 (d, e, f))
	| Digit.Three (a, b, c), Digit.Two (d, e), Digit.Two (f, g) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node2 (d, e), Node.Node2 (f, g))
	| Digit.Three (a, b, c), Digit.Two (d, e), Digit.Three (f, g, h) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node2 (g, h))
	| Digit.Three (a, b, c), Digit.Two (d, e), Digit.Four (f, g, h, i) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node3 (g, h, i))
	| Digit.Three (a, b, c), Digit.Three (d, e, f), Digit.One g ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node2 (d, e), Node.Node2 (f, g))
	| Digit.Three (a, b, c), Digit.Three (d, e, f), Digit.Two (g, h) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node2 (g, h))
	| Digit.Three (a, b, c), Digit.Three (d, e, f), Digit.Three (g, h, i) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node3 (g, h, i))
	| Digit.Three (a, b, c), Digit.Three (d, e, f), Digit.Four (g, h, i, j) ->
	    Digit.Four (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node2 (g, h), Node.Node2 (i, j))
	| Digit.Three (a, b, c), Digit.Four (d, e, f, g), Digit.One h ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node2 (g, h))
	| Digit.Three (a, b, c), Digit.Four (d, e, f, g), Digit.Two (h, i) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node3 (g, h, i))
	| Digit.Three (a, b, c), Digit.Four (d, e, f, g), Digit.Three (h, i, j) ->
	    Digit.Four (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node2 (g, h), Node.Node2 (i, j))
	| Digit.Three (a, b, c), Digit.Four (d, e, f, g), Digit.Four (h, i, j, k) ->
	    Digit.Four (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node3 (g, h, i), Node.Node2 (j, k))
	| Digit.Four (a, b, c, d), Digit.One e, Digit.One f ->
	    Digit.Two (Node.Node3 (a, b, c), Node.Node3 (d, e, f))
	| Digit.Four (a, b, c, d), Digit.One e, Digit.Two (f, g) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node2 (d, e), Node.Node2 (f, g))
	| Digit.Four (a, b, c, d), Digit.One e, Digit.Three (f, g, h) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node2 (g, h))
	| Digit.Four (a, b, c, d), Digit.One e, Digit.Four (f, g, h, i) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node3 (g, h, i))
	| Digit.Four (a, b, c, d), Digit.Two (e, f), Digit.One g ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node2 (d, e), Node.Node2 (f, g))
	| Digit.Four (a, b, c, d), Digit.Two (e, f), Digit.Two (g, h) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node2 (g, h))
	| Digit.Four (a, b, c, d), Digit.Two (e, f), Digit.Three (g, h, i) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node3 (g, h, i))
	| Digit.Four (a, b, c, d), Digit.Two (e, f), Digit.Four (g, h, i, j) ->
	    Digit.Four (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node2 (g, h), Node.Node2 (i, j))
	| Digit.Four (a, b, c, d), Digit.Three (e, f, g), Digit.One h ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node2 (g, h))
	| Digit.Four (a, b, c, d), Digit.Three (e, f, g), Digit.Two (h, i) ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node3 (g, h, i))
	| Digit.Four (a, b, c, d), Digit.Three (e, f, g), Digit.Three (h, i, j) ->
	    Digit.Four (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node2 (g, h), Node.Node2 (i, j))
	| Digit.Four (a, b, c, d), Digit.Three (e, f, g), Digit.Four (h, i, j, k) ->
	    Digit.Four (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node3 (g, h, i), Node.Node2 (j, k))
	| Digit.Four (a, b, c, d), Digit.Four (e, f, g, h), Digit.One i ->
	    Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node3 (g, h, i))
	| Digit.Four (a, b, c, d), Digit.Four (e, f, g, h), Digit.Two (i, j) ->
	    Digit.Four (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node2 (g, h), Node.Node2 (i, j))
	| Digit.Four (a, b, c, d), Digit.Four (e, f, g, h), Digit.Three (i, j, k) ->
	    Digit.Four (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node3 (g, h, i), Node.Node2 (j, k))
	| Digit.Four (a, b, c, d), Digit.Four (e, f, g, h), Digit.Four (i, j, k, l) ->
	    Digit.Four (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node3 (g, h, i), Node.Node3 (j, k, l))
end

let nodes' d1 d2 = begin 
  match d1, d2 with 
    Digit.One a, Digit.One b ->
      Digit.One (Node.Node2 (a, b))
  | Digit.One a, Digit.Two (b, c) ->
      Digit.One (Node.Node3 (a, b, c))
  | Digit.One a, Digit.Three (b, c, d) ->
      Digit.Two (Node.Node2 (a, b), Node.Node2 (c, d))
  | Digit.One a, Digit.Four (b, c, d, e) ->
      Digit.Two (Node.Node3 (a, b, c), Node.Node2 (d, e))
  | Digit.Two (a, b), Digit.One c ->
      Digit.One (Node.Node3 (a, b, c))
  | Digit.Two (a, b), Digit.Two (c, d) ->
      Digit.Two (Node.Node2 (a, b), Node.Node2 (c, d))
  | Digit.Two (a, b), Digit.Three (c, d, e) ->
      Digit.Two (Node.Node3 (a, b, c), Node.Node2 (d, e))
  | Digit.Two (a, b), Digit.Four (c, d, e, f) ->
      Digit.Two (Node.Node3 (a, b, c), Node.Node3 (d, e, f))
  | Digit.Three (a, b, c), Digit.One d ->
      Digit.Two (Node.Node2 (a, b), Node.Node2 (c, d))
  | Digit.Three (a, b, c), Digit.Two (d, e) ->
      Digit.Two (Node.Node3 (a, b, c), Node.Node2 (d, e))
  | Digit.Three (a, b, c), Digit.Three (d, e, f) ->
      Digit.Two (Node.Node3 (a, b, c), Node.Node3 (d, e, f))
  | Digit.Three (a, b, c), Digit.Four (d, e, f, g) ->
      Digit.Three (Node.Node3 (a, b, c), Node.Node2 (d, e), Node.Node2 (f, g))
  | Digit.Four (a, b, c, d), Digit.One e ->
      Digit.Two (Node.Node3 (a, b, c), Node.Node2 (d, e))
  | Digit.Four (a, b, c, d), Digit.Two (e, f) ->
      Digit.Two (Node.Node3 (a, b, c), Node.Node3 (d, e, f))
  | Digit.Four (a, b, c, d), Digit.Three (e, f, g) ->
      Digit.Three (Node.Node3 (a, b, c), Node.Node2 (d, e), Node.Node2 (f, g))
  | Digit.Four (a, b, c, d), Digit.Four (e, f, g, h) ->
      Digit.Three (Node.Node3 (a, b, c), Node.Node3 (d, e, f), Node.Node2 (g, h))
end

let rec app3 : 'a . 'a t -> 'a Digit.t -> 'a t -> 'a t = 
fun xs ts ys -> begin 
  match xs with
    Empty -> Digit.fold_right consl ts ys
  | Single x -> consl x (Digit.fold_right consl ts ys)
  | Deep (pr1, m1, sf1) -> begin 
      match ys with
        Empty -> Digit.fold_left consr xs ts
      | Single y -> consr (Digit.fold_left consr xs ts) y
      | Deep (pr2, m2, sf2) ->
	       let m' = app3 m1 (nodes sf1 ts pr2) m2 in
	       Deep (pr1, m', sf2)
	end
end

let concat xs ys = begin 
  match xs with
    Empty -> ys
  | Single x -> consl x ys
  | Deep (pr1, m1, sf1) -> begin 
      match ys with
        Empty -> xs
      | Single y -> consr xs y
      | Deep (pr2, m2, sf2) ->
	       let m' = app3 m1 (nodes' sf1 pr2) m2 in
	       Deep (pr1, m', sf2)
	end
end
