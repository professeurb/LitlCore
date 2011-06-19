(************************************************************************
*
*  litlTreeMapKeyEnum.ml
*  
*
*  Created by Olivier Brunet on 25 May 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
*
************************************************************************)

module BTM = LitlBinaryTreeMap

type ('a, 'b) zipper =
  End
| More of 'a * ('a, 'b) BTM.t * ('a, 'b) zipper

type ('a, 'b) enumer = ('a, 'b) BTM.t * ('a, 'b) zipper
type ('a, 'r) applier = { f : 'b. ('a, 'b) enumer -> 'r }
type 'a t = { apply : 'r. ('a, 'r) applier -> 'r }

let make t = begin { 
	  apply = fun applier -> applier.f (t, End)
	}
end

let rec nexter t = begin 
	match t with
		(BTM.Empty, End) -> None
	| (BTM.Empty, More (v, bt, z)) -> Some (v,
			{	apply = fun applier -> applier.f (bt, z)
			}
		)
	| (BTM.Node
			(BTM.Empty, v, _, r, _), z) ->
			Some (v,
				{ apply = fun applier -> applier.f (r, z)
				}
			)
	| (BTM.Node (l, v, _, r, _), z) ->
			nexter (l, More (v, r, z))
end
let next t = begin 
  t.apply { f = nexter}
end

let folder f (bt, z) res = begin 
	let rec aux z r = begin 
		match z with
			End -> r
		| More (v, bt', z') ->
				aux z' (BTM.fold_keys f bt' (f v r))
	end
	in
	aux z (BTM.fold_keys f bt res)
end
let fold f t r = begin 
  t.apply { f = fun g -> folder f g r }
end

let iterer f (bt, z) = begin 
	let rec aux z = begin 
		match z with
			End -> ()
		| More (v, bt', z') -> begin 
				f v ;
				BTM.iter_keys f bt' ;
				aux z'
		end
	end in
	BTM.iter_keys f bt ;
	aux z
end
let iter f t = begin 
  t.apply { f = fun g -> iterer f g }
end

let skiper f (bt, z) = begin 
  let rec aux_2 bt pldt = begin 
    match bt with
      BTM.Node (left, v, _, right, _) ->
        if f v
        then aux_2 left (More (v, right, pldt))
        else aux_2 right pldt
    | BTM.Empty -> begin 
        match pldt with
          End -> None
        | More (v, bt, pldt) -> Some (v,
						{ apply = fun applier ->
								applier.f (bt, pldt)
						}
					)
    end
  end in
  let rec aux_1 bt z = begin 
    match pldt with
    More (v, bt', z') -> 
      if f v then aux_2 bt z else aux_1 bt' z'
  | End -> aux_2 bt z
  end in
  aux_1 bt z
end
let skip_until f t = begin 
  t.apply { f = fun t' -> skiper f t' }
end
