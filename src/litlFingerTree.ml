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

  let m n = begin 
    match n with
      One (_, am) -> am
    | Two (_, am, _, bm) -> am @@ bm
    | Three (_, am, _, bm, _, cm) -> am @@ bm @@ cm
    | Four (_, am, _, bm, _, cm, _, dm) -> am @@ bm @@ cm @@ dm
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

let ft = from_list (nl 10) ;;
let Some (ft, v) = next_right ft ;;
let Some (v, ft) = next ft ;;

let ft1 = from_list (nl 10) ;;
let ft2 = from_list (nl 7) ;;
let ft = concat ft1 ft2 ;;
*)

(* For automatically generating function "nodes"

let rec nodes = function
  [a; b] -> ["Node2 ("^a^", "^a^"m, "^b^", "^b^"m)"]
| [a; b; c] -> ["Node3 ("^a^", "^a^"m, "^b^", "^b^"m, "^c^", "^c^"m)"]
| [a; b; c; d] -> ["Node2 ("^a^", "^a^"m, "^b^", "^b^"m)"; "Node2 ("^c^", "^c^"m, "^d^", "^d^"m)"]
| a :: b :: c :: l -> ("Node3 ("^a^", "^a^"m, "^b^", "^b^"m, "^c^", "^c^"m)") :: (nodes l)
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

For the FingerTree version, we then apply some regular expressions :
"\| One ([^\(])" => "| One ($1, $1m)"
"\| Two \(([^\(]), (.)\)" => "| Two ($1, $1m, $2, $2m)"
"\| Three \(([^\(]), (.), (.)\)" => "| Three ($1, $1m, $2, $2m, $3, $3m)"
"\| Four \(([^\(]), (.), (.), (.)\)"
    => "| Four ($1, $1m, $2, $2m, $3, $3m, $4, $4m)"
"Node2 \((.), (.)\)" => "Node2 ($1, $1m, $2, $2m), $1m @@ $2m"
"Node3 \((.), (.), (.)\)"
    => "Node3 ($1, $1m, $2, $2m, $3, $3m), $1m @@ $2m @@ $3m"
*)

let nodes_3 d1 d2 d3 =
Digit.(
	Node.(
    match d1 with
    | One (a, am) ->
    begin 
      match d2 with
      | One (b, bm) ->
      begin 
        match d3 with
        | One (c, cm) ->
        One (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm)
        | Two (c, cm, d, dm) ->
        Two (Node2 (a, am, b, bm), am @@ bm, Node2 (c, cm, d, dm), cm @@ dm)
        | Three (c, cm, d, dm, e, em) ->
        Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em)
        | Four (c, cm, d, dm, e, em, f, fm) ->
        Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm)
      end
      | Two (b, bm, c, cm) ->
      begin 
        match d3 with
        | One (d, dm) ->
        Two (Node2 (a, am, b, bm), am @@ bm, Node2 (c, cm, d, dm), cm @@ dm)
        | Two (d, dm, e, em) ->
        Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em)
        | Three (d, dm, e, em, f, fm) ->
        Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm)
        | Four (d, dm, e, em, f, fm, g, gm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em, Node2 (f, fm, g, gm), fm @@ gm)
      end
      | Three (b, bm, c, cm, d, dm) ->
      begin 
        match d3 with
        | One (e, em) ->
        Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em)
        | Two (e, em, f, fm) ->
        Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm)
        | Three (e, em, f, fm, g, gm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em, Node2 (f, fm, g, gm), fm @@ gm)
        | Four (e, em, f, fm, g, gm, h, hm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node2 (g, gm, h, hm), gm @@ hm)
      end
      | Four (b, bm, c, cm, d, dm, e, em) ->
      begin 
        match d3 with
        | One (f, fm) ->
        Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm)
        | Two (f, fm, g, gm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em, Node2 (f, fm, g, gm), fm @@ gm)
        | Three (f, fm, g, gm, h, hm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node2 (g, gm, h, hm), gm @@ hm)
        | Four (f, fm, g, gm, h, hm, i, im) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node3 (g, gm, h, hm, i, im), gm @@ hm @@ im)
      end
    end
    | Two (a, am, b, bm) ->
    begin 
      match d2 with
      | One (c, cm) ->
      begin 
        match d3 with
        | One (d, dm) ->
        Two (Node2 (a, am, b, bm), am @@ bm, Node2 (c, cm, d, dm), cm @@ dm)
        | Two (d, dm, e, em) ->
        Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em)
        | Three (d, dm, e, em, f, fm) ->
        Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm)
        | Four (d, dm, e, em, f, fm, g, gm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em, Node2 (f, fm, g, gm), fm @@ gm)
      end
      | Two (c, cm, d, dm) ->
      begin 
        match d3 with
        | One (e, em) ->
        Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em)
        | Two (e, em, f, fm) ->
        Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm)
        | Three (e, em, f, fm, g, gm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em, Node2 (f, fm, g, gm), fm @@ gm)
        | Four (e, em, f, fm, g, gm, h, hm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node2 (g, gm, h, hm), gm @@ hm)
      end
      | Three (c, cm, d, dm, e, em) ->
      begin 
        match d3 with
        | One (f, fm) ->
        Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm)
        | Two (f, fm, g, gm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em, Node2 (f, fm, g, gm), fm @@ gm)
        | Three (f, fm, g, gm, h, hm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node2 (g, gm, h, hm), gm @@ hm)
        | Four (f, fm, g, gm, h, hm, i, im) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node3 (g, gm, h, hm, i, im), gm @@ hm @@ im)
      end
      | Four (c, cm, d, dm, e, em, f, fm) ->
      begin 
        match d3 with
        | One (g, gm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em, Node2 (f, fm, g, gm), fm @@ gm)
        | Two (g, gm, h, hm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node2 (g, gm, h, hm), gm @@ hm)
        | Three (g, gm, h, hm, i, im) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node3 (g, gm, h, hm, i, im), gm @@ hm @@ im)
        | Four (g, gm, h, hm, i, im, j, jm) ->
        Four (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node2 (g, gm, h, hm), gm @@ hm, Node2 (i, im, j, jm), im @@ jm)
      end
    end
    | Three (a, am, b, bm, c, cm) ->
    begin 
      match d2 with
      | One (d, dm) ->
      begin 
        match d3 with
        | One (e, em) ->
        Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em)
        | Two (e, em, f, fm) ->
        Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm)
        | Three (e, em, f, fm, g, gm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em, Node2 (f, fm, g, gm), fm @@ gm)
        | Four (e, em, f, fm, g, gm, h, hm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node2 (g, gm, h, hm), gm @@ hm)
      end
      | Two (d, dm, e, em) ->
      begin 
        match d3 with
        | One (f, fm) ->
        Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm)
        | Two (f, fm, g, gm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em, Node2 (f, fm, g, gm), fm @@ gm)
        | Three (f, fm, g, gm, h, hm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node2 (g, gm, h, hm), gm @@ hm)
        | Four (f, fm, g, gm, h, hm, i, im) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node3 (g, gm, h, hm, i, im), gm @@ hm @@ im)
      end
      | Three (d, dm, e, em, f, fm) ->
      begin 
        match d3 with
        | One (g, gm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em, Node2 (f, fm, g, gm), fm @@ gm)
        | Two (g, gm, h, hm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node2 (g, gm, h, hm), gm @@ hm)
        | Three (g, gm, h, hm, i, im) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node3 (g, gm, h, hm, i, im), gm @@ hm @@ im)
        | Four (g, gm, h, hm, i, im, j, jm) ->
        Four (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node2 (g, gm, h, hm), gm @@ hm, Node2 (i, im, j, jm), im @@ jm)
      end
      | Four (d, dm, e, em, f, fm, g, gm) ->
      begin 
        match d3 with
        | One (h, hm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node2 (g, gm, h, hm), gm @@ hm)
        | Two (h, hm, i, im) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node3 (g, gm, h, hm, i, im), gm @@ hm @@ im)
        | Three (h, hm, i, im, j, jm) ->
        Four (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node2 (g, gm, h, hm), gm @@ hm, Node2 (i, im, j, jm), im @@ jm)
        | Four (h, hm, i, im, j, jm, k, km) ->
        Four (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node3 (g, gm, h, hm, i, im), gm @@ hm @@ im, Node2 (j, jm, k, km), jm @@ km)
      end
    end
    | Four (a, am, b, bm, c, cm, d, dm) ->
    begin 
      match d2 with
      | One (e, em) ->
      begin 
        match d3 with
        | One (f, fm) ->
        Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm)
        | Two (f, fm, g, gm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em, Node2 (f, fm, g, gm), fm @@ gm)
        | Three (f, fm, g, gm, h, hm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node2 (g, gm, h, hm), gm @@ hm)
        | Four (f, fm, g, gm, h, hm, i, im) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node3 (g, gm, h, hm, i, im), gm @@ hm @@ im)
      end
      | Two (e, em, f, fm) ->
      begin 
        match d3 with
        | One (g, gm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em, Node2 (f, fm, g, gm), fm @@ gm)
        | Two (g, gm, h, hm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node2 (g, gm, h, hm), gm @@ hm)
        | Three (g, gm, h, hm, i, im) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node3 (g, gm, h, hm, i, im), gm @@ hm @@ im)
        | Four (g, gm, h, hm, i, im, j, jm) ->
        Four (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node2 (g, gm, h, hm), gm @@ hm, Node2 (i, im, j, jm), im @@ jm)
      end
      | Three (e, em, f, fm, g, gm) ->
      begin 
        match d3 with
        | One (h, hm) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node2 (g, gm, h, hm), gm @@ hm)
        | Two (h, hm, i, im) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node3 (g, gm, h, hm, i, im), gm @@ hm @@ im)
        | Three (h, hm, i, im, j, jm) ->
        Four (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node2 (g, gm, h, hm), gm @@ hm, Node2 (i, im, j, jm), im @@ jm)
        | Four (h, hm, i, im, j, jm, k, km) ->
        Four (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node3 (g, gm, h, hm, i, im), gm @@ hm @@ im, Node2 (j, jm, k, km), jm @@ km)
      end
      | Four (e, em, f, fm, g, gm, h, hm) ->
      begin 
        match d3 with
        | One (i, im) ->
        Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node3 (g, gm, h, hm, i, im), gm @@ hm @@ im)
        | Two (i, im, j, jm) ->
        Four (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node2 (g, gm, h, hm), gm @@ hm, Node2 (i, im, j, jm), im @@ jm)
        | Three (i, im, j, jm, k, km) ->
        Four (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node3 (g, gm, h, hm, i, im), gm @@ hm @@ im, Node2 (j, jm, k, km), jm @@ km)
        | Four (i, im, j, jm, k, km, l, lm) ->
        Four (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node3 (g, gm, h, hm, i, im), gm @@ hm @@ im, Node3 (j, jm, k, km, l, lm), jm @@ km @@ lm)
      end
    end
  )
)

let nodes_2 d1 d2 =
Digit.(
	Node.(
    match d1 with
    | One (a, am) ->
    begin 
      match d2 with
      | One (b, bm) ->
      One (Node2 (a, am, b, bm), am @@ bm)
      | Two (b, bm, c, cm) ->
      One (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm)
      | Three (b, bm, c, cm, d, dm) ->
      Two (Node2 (a, am, b, bm), am @@ bm, Node2 (c, cm, d, dm), cm @@ dm)
      | Four (b, bm, c, cm, d, dm, e, em) ->
      Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em)
    end
    | Two (a, am, b, bm) ->
    begin 
      match d2 with
      | One (c, cm) ->
      One (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm)
      | Two (c, cm, d, dm) ->
      Two (Node2 (a, am, b, bm), am @@ bm, Node2 (c, cm, d, dm), cm @@ dm)
      | Three (c, cm, d, dm, e, em) ->
      Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em)
      | Four (c, cm, d, dm, e, em, f, fm) ->
      Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm)
    end
    | Three (a, am, b, bm, c, cm) ->
    begin 
      match d2 with
      | One (d, dm) ->
      Two (Node2 (a, am, b, bm), am @@ bm, Node2 (c, cm, d, dm), cm @@ dm)
      | Two (d, dm, e, em) ->
      Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em)
      | Three (d, dm, e, em, f, fm) ->
      Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm)
      | Four (d, dm, e, em, f, fm, g, gm) ->
      Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em, Node2 (f, fm, g, gm), fm @@ gm)
    end
    | Four (a, am, b, bm, c, cm, d, dm) ->
    begin 
      match d2 with
      | One (e, em) ->
      Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em)
      | Two (e, em, f, fm) ->
      Two (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm)
      | Three (e, em, f, fm, g, gm) ->
      Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node2 (d, dm, e, em), dm @@ em, Node2 (f, fm, g, gm), fm @@ gm)
      | Four (e, em, f, fm, g, gm, h, hm) ->
      Three (Node3 (a, am, b, bm, c, cm), am @@ bm @@ cm, Node3 (d, dm, e, em, f, fm), dm @@ em @@ fm, Node2 (g, gm, h, hm), gm @@ hm)
    end
  )
)

let rec pre_concat : 'a . 'a t -> 'a Digit.t -> 'a t -> 'a t = 
fun xs ts ys -> begin 
  match xs with
    Empty -> Digit.fold_right_m consl ts ys
  | Single (x, xm) -> consl x xm (Digit.fold_right_m consl ts ys)
  | Deep (pr1, pr1m, m1, m1m, sf1, sf1m) -> begin 
      match ys with
        Empty -> Digit.fold_left_m consr xs ts
      | Single (y, ym) -> consr (Digit.fold_left_m consr xs ts) y ym
      | Deep (pr2, pr2m, m2, m2m, sf2, sf2m) ->
	       let m' = pre_concat m1 (nodes_3 sf1 ts pr2) m2 in
	       Deep (
		       pr1, pr1m,
		       m', m1m @@ sf1m @@ (Digit.m ts) @@ pr2m, 
		       sf2, sf2m
		     )
	end
end

let concat xs ys = begin 
  match xs with
    Empty -> ys
  | Single (x, xm) -> consl x xm ys
  | Deep (pr1, pr1m, m1, m1m, sf1, sf1m) -> begin 
      match ys with
        Empty -> xs
      | Single (y, ym) -> consr xs y ym
      | Deep (pr2, pr2m, m2, m2m, sf2, sf2m) ->
	       let m' = pre_concat m1 (nodes_2 sf1 pr2) m2 in
	       Deep (pr1, pr1m, m', m1m @@ sf1m @@ pr2m @@ m2m, sf2, sf2m)
	end
end
