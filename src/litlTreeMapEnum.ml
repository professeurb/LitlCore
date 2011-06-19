(************************************************************************
*
*  litlTreeMapEnum.ml
*  
*
*  Created by Olivier Brunet on 25 May 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
*
************************************************************************)

module BTM = LitlBinaryTreeMap

type ('a, 'b) zipper = |
	End
| More of
		('a * 'b) *
		('a, 'b) BTM.t * ('a, 'b) zipper

type ('a, 'b) t = ('a, 'b) BTM.t * ('a, 'b) zipper

let make t = begin 
	t, End
end

let rec next t = begin 
	match t with
		(BTM.Empty, End) -> None
	| (BTM.Empty, More (v, bt, z)) -> Some (v, (bt, z))
	| (BTM.Node (BTM.Empty, v, k, r, _), z) -> Some ((v, k), (r, z))
	| (BTM.Node (l, v, k, r, _), z) -> next (l, More ((v, k), r, z))
end

let iter f (bt, z) = begin 
	let rec aux z = begin 
		match pldt with
			End -> ()
		| More (v, bt', z') -> begin 
				f v ;
				BTM.iter_elts f bt' ;
				aux z'
		end
	end in
	BTM.iter_elts f bt ;
	aux z
end

let fold f (t, z) res = begin 
	let rec aux z r = begin 
		match z with
			End -> r
		| More (v, bt', z') ->
				aux z' (BTM.fold_elts f bt' (f v r))
	end in
	aux z (BTM.fold_elts f t res)
end

let skip_until f (bt, z) = begin 
  let rec aux_2 bt z = begin 
    match bt with
      BTM.Node (l, k, v, r, _) ->
        if f (k, v)
        then aux_2 l (More ((k, v), r, z))
        else aux_2 r z
    | BTM.Empty -> begin 
        match z with
          End -> None
        | More (kv, bt', z') -> Some (kv, (bt', z'))
    end
  end in
  let rec aux_1 bt z = begin 
    match z with
    More (kv, bt', z') -> 
      if f kv then aux_2 bt z else aux_1 bt' z'
  | End -> aux_2 bt z
  end in
  aux_1 bt z
end
