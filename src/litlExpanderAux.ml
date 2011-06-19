(************************************************************************
*
*  litlExpanderAux.ml
*  
*
*  Created by Olivier Brunet on 27 May 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
*
************************************************************************)

type ('a, 'b, 'c) expander = {
	func : 'a -> 'c -> 'b LitlEnum.t * 'c ;
	curr : 'b LitlEnum.t ;
	pool : 'a LitlEnum.t ;
	state : 'c
}
type ('a, 'r) applier = { f : 'b 'c. ('b, 'a, 'c) expander -> 'r }
type 'a t = { apply : 'r. ('a, 'r) applier -> 'r }

let make func pool state = begin 
	{ apply = fun applier -> applier.f
		{	func = func ;
			curr = LitlEnum.empty ;
			pool = pool ;
			state = state
		}
	}
end

let next_expand expand = begin 
	let rec next_aux func pool state = begin 
		match LitlEnum.next pool with
			None -> None
		| Some (pre_enum, pool') -> begin 
				let (enum, state') = func pre_enum state in
				match LitlEnum.next enum with
					None -> next_aux func pool' state'
				| Some (value, enum') -> Some (
					value, 
					{ apply = fun applier -> applier.f {
							func = func ;
							curr = enum' ;
							pool = pool' ;
							state = state'
						}
					}
				)
		end
	end in
	match LitlEnum.next expand.curr with
		Some (value, enum') -> Some (
			value,
			{ apply = fun applier -> applier.f {
					func = expand.func ;
					curr = enum' ;
					pool = expand.pool ;
					state = expand.state
				}
			}
		)
	| None -> next_aux expand.func expand.pool expand.state
end
let next t = begin 
  t.apply { f = next_expand }
end

let fold_expand f t r = begin 
	let (res, _) = LitlEnum.fold (
		fun pre_enum (r, state) ->
			let (enum, state') = t.func pre_enum state in
			(LitlEnum.fold f enum r, state')
		) t.pool (LitlEnum.fold f t.curr r, t.state) in
	res
end
let fold f t r = begin 
  t.apply { f = fun g -> fold_expand f g r }
end

let iter_expand f t = begin 
	LitlEnum.iter f t.curr ;
	let _ = LitlEnum.fold (
		fun pre_enum state ->
			let (enum, state') = t.func pre_enum state in
			LitlEnum.iter f enum ; state'
		) t.pool t.state
	in ()
end
let iter f t = begin 
  t.apply { f = fun g -> iter_expand f g }
end
