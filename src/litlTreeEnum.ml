(************************************************************************
*
*  litlTreeEnum.ml
*  
*
*  Created by Olivier Brunet on 23 May 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
*
************************************************************************)

module BT = LitlBinaryTree

(* An 'a zipper as actually some kind of partial tree zipper *)
type 'a zipper =
		End
	| More of 'a * 'a BT.t * 'a zipper

type 'a t = 'a BT.t * 'a zipper

let make t = begin 
	t, End
end

let rec next t = begin 
	match t with
		(BT.Empty, End) -> None
	| (BT.Empty, More (v, bt, z)) -> Some (v, (bt, z))
	| (BT.Node (BT.Empty, v, right, _), z) ->
			Some (v, (right, z))
	| (BT.Node (left, v, right, _), z) ->
			next (left, More (v, right, z))
end

let iter f (t, z) = begin 
	let rec aux z = begin 
		match z with
			End -> ()
		| More (v, t, z') -> begin 
				f v ;
				BT.iter f t ;
				aux z'
		end
	end in
	BT.iter f t ;
	aux z
end

let fold f (t, z) res = begin 
	let rec aux z r = begin 
		match z with
			End -> r
		| More (v, t, z') ->
				aux z' (BT.fold f t (f v r))
	end
	in
	aux z (BT.fold f t res)
end

let skip_until f (t, z) = begin 
  let rec aux_2 t z = begin 
    match t with
      BT.Node (l, v, r, _) ->
        if f v
        then aux_2 l (More (v, r, z))
        else aux_2 r z
    | BT.Empty -> begin 
        match z with
          End -> None
        | More (v, t', z') -> Some (v, (t', z'))
    end
  end in
  let rec aux_1 t z = begin 
    match z with
      More (v, t', z') -> 
        if f v then aux_2 t z else aux_1 t' z'
    | End -> aux_2 t z
  end in
  aux_1 t z
end
