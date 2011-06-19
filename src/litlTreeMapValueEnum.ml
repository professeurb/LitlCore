(************************************************************************
*
*  litlTreeMapValueEnum.ml
*  
*
*  Created by Olivier Brunet on 27 May 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
*
************************************************************************)

type ('a, 'b) zipper =
	End
| More of 'b * ('a, 'b) BTM.t * ('a, 'b) zipper

type ('a, 'b) enumer = ('a, 'b) BTM.t * ('a, 'b) zipper
type ('a, 'r) applier = { f : 'b. ('b, 'a) enumer -> 'r }
type 'a t = { apply : 'r. ('a, 'r) applier -> 'r }

let make t = begin 
	{ apply = fun applier -> applier.f (t, End) }
end

let rec nexter t = begin 
	match t with
		(BTM.Empty, End) -> None
	| (BTM.Empty, More (v, bt, z)) ->
		  Some (v, { apply = fun applier -> applier.f (bt, z)})
	| (BTM.Node (BTM.Empty, _, v, r, _), z) ->
			Some (v, { apply = fun applier -> applier.f (r, z)})
	| (BTM.Node (l, _, v, r, _), z) ->
			nexter (l, More (v, r, z))
end
let next t = begin 
  t.apply { f = nexter }
end 

let iterer f (t, z) = begin 
	let rec aux z = begin 
		match pldt with
			End -> ()
		| More (v, t', z') -> begin 
				f v ;
				BTM.iter_values f t' ;
				aux z'
		end
	end
	in
	BTM.iter_values f t ;
	aux z
end
let iter f t = begin 
  t.apply { f = fun g -> iterer f g }
end

let folder f (t, z) res = begin 
	let rec aux z r = begin 
		match pldt with
			End -> r
		| More (v, t', z') ->
				aux z' (BTM.fold_values f t' (f v r))
	end
	in
	aux z (BTM.fold_values f t res)
end
let fold f t r = begin 
  t.apply { f = fun g -> folder f g r }
end

let skiper f (bt, pldt) = begin 
  let rec aux_2 bt z = begin 
    match bt with
      BTM.Node (l, _, v, r, _) ->
        if f v
        then aux_2 l (More (v, r, z))
        else aux_2 r z
    | BTM.Empty -> begin 
        match z with
          End -> None
        | More (v, bt, z') -> Some (v,
						{ apply = fun applier ->
								applier.f (bt, z')
						}
					)
    end
  end in
  let rec aux_1 bt z = begin 
    match z with
    More (v, bt', z') -> 
      if f v then aux_2 bt z else aux_1 bt' z'
  | End -> aux_2 bt z
  end in
  aux_1 bt z
end
let skip_until f t = begin 
  t.apply { f = fun t' -> skiper f t' }
end
