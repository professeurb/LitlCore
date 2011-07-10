(************************************************************************
*
*  litlFingerTree.ml
*  
*
*  Created by Olivier Brunet on 19 Jun 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
*
************************************************************************)

module List = struct 
  include List

  let empty = []
  let cons a b = a :: b
end

module M : sig 
  type 'a t
  val identity : 'a t
  val lift : 'a -> 'a t
  val combine : 'a t -> 'a t -> 'a t
end
= struct 
  type 'a t = 'a option
  let identity = None
  let lift a = Some a
  let combine a b = begin 
    match a with
      None -> b
    | Some va -> begin 
      match b with
        Some vb -> Some (max va vb)
      | None -> a
    end
	end
end

module M = struct 
  type 'a t = int
  let identity = 0
  let lift a = 1
  let combine a b = a + b
end

type 'a m = 'a M.t
let f = M.lift
let (@@) = M.combine

module Node = struct 
  type 'a t =
    Node2 of 'a * 'a m * 'a * 'a m
  | Node3 of 'a * 'a m * 'a * 'a m * 'a * 'a m

  let fold_right f n z = begin 
	  match n with
		  Node2 (a, _, b, _) -> f a (f b z)
		| Node3 (a, _, b, _, c, _) -> f a (f b (f c z))
  end

  let fold_left f z n = begin 
	  match n with
		  Node2 (b, _, a, _) -> f (f z b) a
		| Node3 (c, _, b, _, a, _) -> f (f (f z c) b) a
  end

  let iter f n = begin 
    match n with
      Node2 (a, _, b, _) -> f a ; f b
    | Node3 (a, _, b, _, c, _) -> f a ; f b ; f c
  end

  let m n = begin 
    match n with
      Node2 (_, am, _, bm) -> am @@ bm
    | Node3 (_, am, _, bm, _, cm) -> am @@ bm @@ cm
  end
end	

module Digit = struct 
  type 'a t =
    One of 'a * 'a m
  | Two of 'a * 'a m * 'a * 'a m
  | Three of 'a * 'a m * 'a * 'a m * 'a * 'a m
  | Four of 'a * 'a m * 'a * 'a m * 'a * 'a m * 'a * 'a m

  let fold_right f n z = begin 
	  match n with
	    One (a, _) -> f a z
	  | Two (a, _, b, _) -> f a (f b z)
	  | Three (a, _, b, _, c, _) -> f a (f b (f c z))
	  | Four (a, _, b, _, c, _, d, _) -> f a (f b (f c (f d z)))
	end

  let fold_left f z n = begin 
	  match n with
	    One (a, _) -> f z a
	  | Two (b, _, a, _) -> f (f z b) a
	  | Three (c, _, b, _, a, _) -> f (f (f z c) b) a
	  | Four (d, _, c, _, b, _, a, _) -> f (f (f (f z d) c) b) a
	end

  let fold_right_m f n z = begin 
	  match n with
	    One (a, am) -> f a am z
	  | Two (a, am, b, bm) -> f a am (f b bm z)
	  | Three (a, am, b, bm, c, cm) -> f a am (f b bm (f c cm z))
	  | Four (a, am, b, bm, c, cm, d, dm) -> f a am (f b bm (f c cm (f d dm z)))
	end

  let fold_left_m f z n = begin 
	  match n with
	    One (a, am) -> f z a am
	  | Two (b, bm, a, am) -> f (f z b bm) a am
	  | Three (c, cm, b, bm, a, am) -> f (f (f z c cm) b bm) a am
	  | Four (d, dm, c, cm, b, bm, a, am) -> f (f (f (f z d dm) c cm) b bm) a am
	end

  let iter f n = begin 
	  match n with
	    One (a, _) -> f a
	  | Two (a, _, b, _) -> f a ; f b
	  | Three (a, _, b, _, c, _) -> f a ; f b ; f c
	  | Four (a, _, b, _, c, _, d, _) -> f a ; f b ; f c ; f d
	end

  let from_node n = begin 
    match n with
      Node.Node2 (a, am, b, bm) -> Two (a, am, b, bm)
    | Node.Node3 (a, am, b, bm, c, cm) -> Three (a, am, b, bm, c, cm)
  end
end

type 'a digit = 'a Digit.t
type 'a node = 'a Node.t
type 'a t =
  Empty
| Single of 'a * 'a m
| Deep of 'a digit * 'a m * 'a node t * 'a m * 'a digit * 'a m

let empty = Empty

let is_empty t = begin 
  match t with
    Empty -> true
  | _ -> false
end

let rec fold_right : 'a 'b . ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b = begin 
  fun f t z -> match t with
    Empty -> z
  | Single (x, _) -> f x z
  | Deep (pr, _, m, _, sf, _) ->
	    Digit.fold_right f pr (
		    fold_right (Node.fold_right f) m (Digit.fold_right f sf z)
		)
end	

let rec fold_left : 'a 'b . ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a = begin 
  fun f z t -> match t with
    Empty -> z
  | Single (x, _) -> f z x
  | Deep (pr, _, m, _, sf, _) ->
	    Digit.fold_left f (
		    fold_left (Node.fold_left f) (Digit.fold_left f z pr) m
		) sf
end

let rec iter : 'a . ('a -> unit) -> 'a t -> unit = begin 
  fun f t -> match t with
    Empty -> ()
  | Single (x, _) -> f x
  | Deep (pr, _, m, _, sf, _) ->
      Digit.iter f pr ;
      iter (fun n -> Node.iter f n) m ;
      Digit.iter f sf
end

let rec consl : 'a . 'a -> 'a m -> 'a t -> 'a t = begin 
  fun a am ft -> 
	  match ft with
	    Empty -> Single (a, am)
	  | Single (b, bm) ->
		    Deep (Digit.One (a, am), am, Empty, M.identity, Digit.One (b, bm), bm)
	  | Deep (Digit.Four (b, bm, c, cm, d, dm, e, em), _, m, mm, sf, sfm) ->
		    let cdem = cm @@ dm @@ em in 
        Deep (
	        Digit.Two (a, am, b, bm), am @@ bm, 
	        consl (Node.Node3 (c, cm, d, dm, e, em)) cdem m,
	        cdem @@ mm,
	        sf, sfm
	      )
	  | Deep (Digit.One (b, bm), _, m, mm, sf, sfm) ->
		    Deep (Digit.Two (a, am, b, bm), am @@ bm, m, mm, sf, sfm)
	  | Deep (Digit.Two (b, bm, c, cm), prm, m, mm, sf, sfm) ->
		    Deep (
			    Digit.Three (a, am, b, bm, c, cm),
			    am @@ prm, m, mm, sf, sfm
			  )
	  | Deep (Digit.Three (b, bm, c, cm, d, dm), prm, m, mm, sf, sfm) ->
        Deep (
	        Digit.Four (a, am, b, bm, c, cm, d, dm),
	        am @@ prm, m, mm, sf, sfm
	      )
end
let cons_left a ft = begin 
  consl a (f a) ft
end
let cons = cons_left

let rec consr : 'a . 'a t -> 'a -> 'a m -> 'a t = begin 
  fun ft a am -> 
	  match ft with
	    Empty -> Single (a, am)
	  | Single (b, bm) ->
		    Deep (
			    Digit.One (b, bm), bm,
			    Empty, M.identity,
			    Digit.One (a, am), am
			  )
	  | Deep (pr, prm, m, mm, Digit.Four (e, em, d, dm, c, cm, b, bm), _) ->
		    let edcm = em @@ dm @@ cm in
        Deep (
	        pr, prm,
	        consr m (Node.Node3 (e, em, d, dm, c, cm)) edcm, mm @@ edcm,
	        Digit.Two (b, bm, a, am), bm @@ am
	      )
	  | Deep (pr, prm, m, mm, Digit.One (b, bm), _) ->
		    Deep (pr, prm, m, mm, Digit.Two (b, bm, a, am), bm @@ am)
	  | Deep (pr, prm, m, mm, Digit.Two (c, cm, b, bm), sfm) ->
		    Deep (
			    pr, prm, m, mm,
			    Digit.Three (c, cm, b, bm, a, am), sfm @@ am
			  )
	  | Deep (pr, prm, m, mm, Digit.Three (d, dm, c, cm, b, bm), sfm) ->
        Deep (
	        pr, prm, m, mm, 
	        Digit.Four (d, dm, c, cm, b, bm, a, am), sfm @@ am
	      )
end
let cons_right ft a = begin 
  consr ft a (f a)
end

let from_list s = begin 
  List.fold_right cons s Empty
end
let to_list s = begin 
  fold_right List.cons s []
end

let rec nextl : 'a . 'a t -> ('a * 'a t * 'a m) option = begin 
  fun ft ->
	  match ft with
	    Empty -> None
	  | Single (x, _) -> Some (x, Empty, M.identity)
	  | Deep (Digit.One (a, _), _, m, mm, sf, sfm) ->
		    Some (a, deepl m sf sfm, mm @@ sfm)
	  | Deep (Digit.Two (a, _, b, bm), _, m, mm, sf, sfm) ->
		    Some (a, Deep (Digit.One (b, bm), bm, m, mm, sf, sfm), bm @@ mm @@ sfm)
	  | Deep (Digit.Three (a, _, b, bm, c, cm), _, m, mm, sf, sfm) ->
        let bcm = bm @@ cm in
		    Some (a,
			    Deep (
				    Digit.Two (b, bm, c, cm), bcm,
			      m, mm, sf, sfm
			    ), bcm @@ mm @@ sfm
			)
	  | Deep (Digit.Four (a, _, b, bm, c, cm, d, dm), _, m, mm, sf, sfm) ->
		    let bcdm = bm @@ cm @@ dm in
		    Some (
			    a, Deep (
				    Digit.Three (b, bm, c, cm, d, dm), bcdm,
				    m, mm, sf, sfm
				  ), bcdm @@ mm @@ sfm
				)
end
and deepl : 'a . ('a Node.t) t -> 'a Digit.t -> 'a m -> 'a t = begin 
	fun m sf sfm ->  
	  match nextl m with
        None -> Digit.fold_right_m consl sf Empty
      | Some (a, m', mm') ->
	        Deep (Digit.from_node a, Node.m a, m', mm', sf, sfm)
end
let next ft = begin 
	match nextl ft with
	  None -> None
	| Some (a, ft, _) -> Some (a, ft)
end

let rec nextr : 'a . 'a t -> ('a t * 'a m * 'a) option = begin 
  fun ft ->
	  match ft with
	    Empty -> None
	  | Single (x, _) -> Some (Empty, M.identity, x)
	  | Deep (pr, prm, m, mm, Digit.One (a, _), _) ->
		    Some (deepr pr prm m, prm @@ mm, a)
	  | Deep (pr, prm, m, mm, Digit.Two (b, bm, a, am), _) ->
		    Some (Deep (pr, prm, m, mm, Digit.One (b, bm), bm), prm @@ mm @@ bm, a)
	  | Deep (pr, prm, m, mm, Digit.Three (c, cm, b, bm, a, am), _) ->
		    let cbm = cm @@ bm in
		    Some (
			    Deep (
				    pr, prm,
				    m, mm,
				    Digit.Two (c, cm, b, bm), cbm
				  ), prm @@ mm @@ cbm, a
				)
	  | Deep (pr, prm, m, mm, Digit.Four (d, dm, c, cm, b, bm, a, am), _) ->
		    let dcbm = dm @@ cm @@ bm in
		    Some (
			    Deep (
				    pr, prm,
				    m, mm,
				    Digit.Three (d, dm, c, cm, b, bm), dcbm
				  ), prm @@ mm @@ dcbm, a
				)
end
and deepr : 'a . 'a Digit.t -> 'a m -> ('a Node.t) t -> 'a t = begin 
	fun pr prm m ->  
	  match nextr m with
        None -> Digit.fold_left_m consr Empty pr
      | Some (m', mm', a) ->
	        Deep (pr, prm, m', mm', Digit.from_node a, Node.m a)
end
let next_right ft = begin 
  match nextr ft with
    None -> None
  | Some (ft, _, a) -> Some (ft, a)
end

(*
let nl n =
  let rec aux n l =
    if n = 0 then l else aux (n-1) (n :: l)
  in aux n []
;;

let ft = from_list (nl 20) ;;
let Some (ft, v) = next_right ft ;;
let Some (v, ft) = next ft ;;

*)

(* For automatically generating function "nodes"

let rec nodes = function
  [a; b] -> ["Node2 ("^a^", "^b^")"]
| [a; b; c] -> ["Node3 ("^a^", "^b^", "^c^")"]
| [a; b; c; d] -> ["Node2 ("^a^", "^b^")"; "Node2 ("^c^", "^d^")"]
| a :: b :: c :: l -> ("Node3 ("^a^", "^b^", "^c^")") :: (nodes l)
| _ -> assert false
;;

let digits l =
	match nodes l with
	[a] -> "One ("^a^")"
| [a; b] -> "Two ("^a^", "^b^")"
| [a; b; c] -> "Three ("^a^", "^b^", "^c^")"
| [a; b; c; d] -> "Four ("^a^", "^b^", "^c^", "^d^")"
| _ -> assert false ;;

let letters = 
  [| "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l" |] ;;

let make_sequence n =
	let rec aux n l =
		if n = 0 then l else aux (n-1) (letters.(n-1) :: l)
  in
  aux n []

let string_of_digit a l =
	match l with
	  1 -> "One " ^ letters.(a - 1)
  | 2 -> "Two (" ^ letters.(a - 1) ^ ", " ^ letters.(a) ^ ")"
  | 3 -> "Three (" ^ letters.(a - 1) ^ ", " ^
      letters.(a) ^ ", " ^ letters.(a + 1) ^ ")"
  | _ -> "Four (" ^ letters.(a - 1) ^ ", " ^
      letters.(a) ^ ", " ^ letters.(a + 1) ^ ", " ^ letters.(a + 2) ^ ")"
;;

let write_nodes_3 () = begin 
  Printf.(
  let rec aux depth header vars = begin 
    if depth = 4 then begin
      printf "\n%s%s\n" header (digits (make_sequence (vars - 1)))
    end
    else begin
      printf "\n%sbegin \n" header;
      printf "%s  match d%d with\n" header depth ;
      for i = 1 to 4 do
        printf "%s  | %s ->" header (string_of_digit vars i) ;
        aux (depth + 1) (header^"  ") (vars + i)
      done ;
      printf "%send\n" header
    end
  end in
  printf "let nodes_3 d1 d2 d3 =" ;
  aux 1 "" 1 
  )
end

let write_nodes_2 () = begin 
  Printf.(
  let rec aux depth header vars = begin 
    if depth = 3 then begin
      printf "\n%s%s\n" header (digits (make_sequence (vars - 1)))
    end
    else begin
      printf "\n%sbegin \n" header;
      printf "%s  match d%d with\n" header depth ;
      for i = 1 to 4 do
        printf "%s  | %s ->" header (string_of_digit vars i) ;
        aux (depth + 1) (header^"  ") (vars + i)
      done ;
      printf "%send\n" header
    end
  end in
  printf "let nodes_2 d1 d2 =" ;
  aux 1 "" 1 
  )
end
*)

let nodes_3 d1 d2 d3 =
Digit.(
	Node.(
    match d1 with
    | One a ->
    begin 
      match d2 with
      | One b ->
      begin 
        match d3 with
        | One c ->
        One (Node3 (a, b, c))
        | Two (c, d) ->
        Two (Node2 (a, b), Node2 (c, d))
        | Three (c, d, e) ->
        Two (Node3 (a, b, c), Node2 (d, e))
        | Four (c, d, e, f) ->
        Two (Node3 (a, b, c), Node3 (d, e, f))
      end
      | Two (b, c) ->
      begin 
        match d3 with
        | One d ->
        Two (Node2 (a, b), Node2 (c, d))
        | Two (d, e) ->
        Two (Node3 (a, b, c), Node2 (d, e))
        | Three (d, e, f) ->
        Two (Node3 (a, b, c), Node3 (d, e, f))
        | Four (d, e, f, g) ->
        Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
      end
      | Three (b, c, d) ->
      begin 
        match d3 with
        | One e ->
        Two (Node3 (a, b, c), Node2 (d, e))
        | Two (e, f) ->
        Two (Node3 (a, b, c), Node3 (d, e, f))
        | Three (e, f, g) ->
        Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
        | Four (e, f, g, h) ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
      end
      | Four (b, c, d, e) ->
      begin 
        match d3 with
        | One f ->
        Two (Node3 (a, b, c), Node3 (d, e, f))
        | Two (f, g) ->
        Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
        | Three (f, g, h) ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
        | Four (f, g, h, i) ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i))
      end
    end
    | Two (a, b) ->
    begin 
      match d2 with
      | One c ->
      begin 
        match d3 with
        | One d ->
        Two (Node2 (a, b), Node2 (c, d))
        | Two (d, e) ->
        Two (Node3 (a, b, c), Node2 (d, e))
        | Three (d, e, f) ->
        Two (Node3 (a, b, c), Node3 (d, e, f))
        | Four (d, e, f, g) ->
        Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
      end
      | Two (c, d) ->
      begin 
        match d3 with
        | One e ->
        Two (Node3 (a, b, c), Node2 (d, e))
        | Two (e, f) ->
        Two (Node3 (a, b, c), Node3 (d, e, f))
        | Three (e, f, g) ->
        Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
        | Four (e, f, g, h) ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
      end
      | Three (c, d, e) ->
      begin 
        match d3 with
        | One f ->
        Two (Node3 (a, b, c), Node3 (d, e, f))
        | Two (f, g) ->
        Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
        | Three (f, g, h) ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
        | Four (f, g, h, i) ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i))
      end
      | Four (c, d, e, f) ->
      begin 
        match d3 with
        | One g ->
        Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
        | Two (g, h) ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
        | Three (g, h, i) ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i))
        | Four (g, h, i, j) ->
        Four (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h), Node2 (i, j))
      end
    end
    | Three (a, b, c) ->
    begin 
      match d2 with
      | One d ->
      begin 
        match d3 with
        | One e ->
        Two (Node3 (a, b, c), Node2 (d, e))
        | Two (e, f) ->
        Two (Node3 (a, b, c), Node3 (d, e, f))
        | Three (e, f, g) ->
        Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
        | Four (e, f, g, h) ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
      end
      | Two (d, e) ->
      begin 
        match d3 with
        | One f ->
        Two (Node3 (a, b, c), Node3 (d, e, f))
        | Two (f, g) ->
        Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
        | Three (f, g, h) ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
        | Four (f, g, h, i) ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i))
      end
      | Three (d, e, f) ->
      begin 
        match d3 with
        | One g ->
        Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
        | Two (g, h) ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
        | Three (g, h, i) ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i))
        | Four (g, h, i, j) ->
        Four (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h), Node2 (i, j))
      end
      | Four (d, e, f, g) ->
      begin 
        match d3 with
        | One h ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
        | Two (h, i) ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i))
        | Three (h, i, j) ->
        Four (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h), Node2 (i, j))
        | Four (h, i, j, k) ->
        Four (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i), Node2 (j, k))
      end
    end
    | Four (a, b, c, d) ->
    begin 
      match d2 with
      | One e ->
      begin 
        match d3 with
        | One f ->
        Two (Node3 (a, b, c), Node3 (d, e, f))
        | Two (f, g) ->
        Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
        | Three (f, g, h) ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
        | Four (f, g, h, i) ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i))
      end
      | Two (e, f) ->
      begin 
        match d3 with
        | One g ->
        Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
        | Two (g, h) ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
        | Three (g, h, i) ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i))
        | Four (g, h, i, j) ->
        Four (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h), Node2 (i, j))
      end
      | Three (e, f, g) ->
      begin 
        match d3 with
        | One h ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
        | Two (h, i) ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i))
        | Three (h, i, j) ->
        Four (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h), Node2 (i, j))
        | Four (h, i, j, k) ->
        Four (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i), Node2 (j, k))
      end
      | Four (e, f, g, h) ->
      begin 
        match d3 with
        | One i ->
        Three (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i))
        | Two (i, j) ->
        Four (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h), Node2 (i, j))
        | Three (i, j, k) ->
        Four (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i), Node2 (j, k))
        | Four (i, j, k, l) ->
        Four (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i), Node3 (j, k, l))
      end
    end
  )
)

(* let nodes d1 m d2 = begin 
	Node.( Digit.(
		match d1, m, d2 with
	One a, One b, One c ->
	    One (Node3 (a, b, c))
	| One a, One b, Two (c, d) ->
	    Two (Node2 (a, b), Node2 (c, d))
	| One a, One b, Three (c, d, e) ->
	    Two (Node3 (a, b, c), Node2 (d, e))
	| One a, One b, Four (c, d, e, f) ->
	    Two (Node3 (a, b, c), Node3 (d, e, f))
	| One a, Two (b, c), One d ->
	    Two (Node2 (a, b), Node2 (c, d))
	| One a, Two (b, c), Two (d, e) ->
	    Two (Node3 (a, b, c), Node2 (d, e))
	| One a, Two (b, c), Three (d, e, f) ->
	    Two (Node3 (a, b, c), Node3 (d, e, f))
	| One a, Two (b, c), Four (d, e, f, g) ->
	    Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
	| One a, Three (b, c, d), One e ->
	    Two (Node3 (a, b, c), Node2 (d, e))
	| One a, Three (b, c, d), Two (e, f) ->
	    Two (Node3 (a, b, c), Node3 (d, e, f))
	| One a, Three (b, c, d), Three (e, f, g) ->
	    Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
	| One a, Three (b, c, d), Four (e, f, g, h) ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
	| One a, Four (b, c, d, e), One f ->
	    Two (Node3 (a, b, c), Node3 (d, e, f))
	| One a, Four (b, c, d, e), Two (f, g) ->
	    Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
	| One a, Four (b, c, d, e), Three (f, g, h) ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
	| One a, Four (b, c, d, e), Four (f, g, h, i) ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i))
	| Two (a, b), One c, One d ->
	    Two (Node2 (a, b), Node2 (c, d))
	| Two (a, b), One c, Two (d, e) ->
	    Two (Node3 (a, b, c), Node2 (d, e))
	| Two (a, b), One c, Three (d, e, f) ->
	    Two (Node3 (a, b, c), Node3 (d, e, f))
	| Two (a, b), One c, Four (d, e, f, g) ->
	    Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
	| Two (a, b), Two (c, d), One e ->
	    Two (Node3 (a, b, c), Node2 (d, e))
	| Two (a, b), Two (c, d), Two (e, f) ->
	    Two (Node3 (a, b, c), Node3 (d, e, f))
	| Two (a, b), Two (c, d), Three (e, f, g) ->
	    Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
	| Two (a, b), Two (c, d), Four (e, f, g, h) ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
	| Two (a, b), Three (c, d, e), One f ->
	    Two (Node3 (a, b, c), Node3 (d, e, f))
	| Two (a, b), Three (c, d, e), Two (f, g) ->
	    Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
	| Two (a, b), Three (c, d, e), Three (f, g, h) ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
	| Two (a, b), Three (c, d, e), Four (f, g, h, i) ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i))
	| Two (a, b), Four (c, d, e, f), One g ->
	    Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
	| Two (a, b), Four (c, d, e, f), Two (g, h) ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
	| Two (a, b), Four (c, d, e, f), Three (g, h, i) ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i))
	| Two (a, b), Four (c, d, e, f), Four (g, h, i, j) ->
	    Four (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h), Node2 (i, j))
	| Three (a, b, c), One d, One e ->
	    Two (Node3 (a, b, c), Node2 (d, e))
	| Three (a, b, c), One d, Two (e, f) ->
	    Two (Node3 (a, b, c), Node3 (d, e, f))
	| Three (a, b, c), One d, Three (e, f, g) ->
	    Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
	| Three (a, b, c), One d, Four (e, f, g, h) ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
	| Three (a, b, c), Two (d, e), One f ->
	    Two (Node3 (a, b, c), Node3 (d, e, f))
	| Three (a, b, c), Two (d, e), Two (f, g) ->
	    Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
	| Three (a, b, c), Two (d, e), Three (f, g, h) ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
	| Three (a, b, c), Two (d, e), Four (f, g, h, i) ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i))
	| Three (a, b, c), Three (d, e, f), One g ->
	    Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
	| Three (a, b, c), Three (d, e, f), Two (g, h) ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
	| Three (a, b, c), Three (d, e, f), Three (g, h, i) ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i))
	| Three (a, b, c), Three (d, e, f), Four (g, h, i, j) ->
	    Four (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h), Node2 (i, j))
	| Three (a, b, c), Four (d, e, f, g), One h ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
	| Three (a, b, c), Four (d, e, f, g), Two (h, i) ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i))
	| Three (a, b, c), Four (d, e, f, g), Three (h, i, j) ->
	    Four (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h), Node2 (i, j))
	| Three (a, b, c), Four (d, e, f, g), Four (h, i, j, k) ->
	    Four (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i), Node2 (j, k))
	| Four (a, b, c, d), One e, One f ->
	    Two (Node3 (a, b, c), Node3 (d, e, f))
	| Four (a, b, c, d), One e, Two (f, g) ->
	    Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
	| Four (a, b, c, d), One e, Three (f, g, h) ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
	| Four (a, b, c, d), One e, Four (f, g, h, i) ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i))
	| Four (a, b, c, d), Two (e, f), One g ->
	    Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
	| Four (a, b, c, d), Two (e, f), Two (g, h) ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
	| Four (a, b, c, d), Two (e, f), Three (g, h, i) ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i))
	| Four (a, b, c, d), Two (e, f), Four (g, h, i, j) ->
	    Four (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h), Node2 (i, j))
	| Four (a, b, c, d), Three (e, f, g), One h ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
	| Four (a, b, c, d), Three (e, f, g), Two (h, i) ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i))
	| Four (a, b, c, d), Three (e, f, g), Three (h, i, j) ->
	    Four (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h), Node2 (i, j))
	| Four (a, b, c, d), Three (e, f, g), Four (h, i, j, k) ->
	    Four (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i), Node2 (j, k))
	| Four (a, b, c, d), Four (e, f, g, h), One i ->
	    Three (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i))
	| Four (a, b, c, d), Four (e, f, g, h), Two (i, j) ->
	    Four (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h), Node2 (i, j))
	| Four (a, b, c, d), Four (e, f, g, h), Three (i, j, k) ->
	    Four (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i), Node2 (j, k))
	| Four (a, b, c, d), Four (e, f, g, h), Four (i, j, k, l) ->
	    Four (Node3 (a, b, c), Node3 (d, e, f), Node3 (g, h, i), Node3 (j, k, l))
  ) )
end *)

let nodes_2 d1 d2 =
Digit.(
	Node.(
    match d1 with
    | One a ->
    begin 
      match d2 with
      | One b ->
      One (Node2 (a, b))
      | Two (b, c) ->
      One (Node3 (a, b, c))
      | Three (b, c, d) ->
      Two (Node2 (a, b), Node2 (c, d))
      | Four (b, c, d, e) ->
      Two (Node3 (a, b, c), Node2 (d, e))
    end
    | Two (a, b) ->
    begin 
      match d2 with
      | One c ->
      One (Node3 (a, b, c))
      | Two (c, d) ->
      Two (Node2 (a, b), Node2 (c, d))
      | Three (c, d, e) ->
      Two (Node3 (a, b, c), Node2 (d, e))
      | Four (c, d, e, f) ->
      Two (Node3 (a, b, c), Node3 (d, e, f))
    end
    | Three (a, b, c) ->
    begin 
      match d2 with
      | One d ->
      Two (Node2 (a, b), Node2 (c, d))
      | Two (d, e) ->
      Two (Node3 (a, b, c), Node2 (d, e))
      | Three (d, e, f) ->
      Two (Node3 (a, b, c), Node3 (d, e, f))
      | Four (d, e, f, g) ->
      Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
    end
    | Four (a, b, c, d) ->
    begin 
      match d2 with
      | One e ->
      Two (Node3 (a, b, c), Node2 (d, e))
      | Two (e, f) ->
      Two (Node3 (a, b, c), Node3 (d, e, f))
      | Three (e, f, g) ->
      Three (Node3 (a, b, c), Node2 (d, e), Node2 (f, g))
      | Four (e, f, g, h) ->
      Three (Node3 (a, b, c), Node3 (d, e, f), Node2 (g, h))
    end
  )
)

(* let nodes' d1 d2 = begin 
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
end *)

let rec pre_concat : 'a . 'a t -> 'a Digit.t -> 'a t -> 'a t = 
fun xs ts ys -> begin 
  match xs with
    Empty -> Digit.fold_right consl ts ys
  | Single x -> consl x (Digit.fold_right consl ts ys)
  | Deep (pr1, m1, sf1) -> begin 
      match ys with
        Empty -> Digit.fold_left consr xs ts
      | Single y -> consr (Digit.fold_left consr xs ts) y
      | Deep (pr2, m2, sf2) ->
	       let m' = pre_concat m1 (nodes_3 sf1 ts pr2) m2 in
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
	       let m' = pre_concat m1 (nodes_2 sf1 pr2) m2 in
	       Deep (pr1, m', sf2)
	end
end
