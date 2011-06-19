(************************************************************************
*
*  litlExpander.ml
*  
*
*  Created by Olivier Brunet on 27 May 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
*
************************************************************************)

type ('a, 'b) expander = {
	func : 'a -> 'b LitlEnum.t ;
	curr : 'b LitlEnum.t ;
	pool : 'a LitlEnum.t
}
type ('a, 'r) applier = { f : 'b. ('b, 'a) expander -> 'r }
type 'a t = { apply : 'r. ('a, 'r) applier -> 'r }

let make func pool = begin 
	{ apply = fun applier -> applier.f
		{	func = func ;
			curr = LitlEnum.empty ;
			pool = pool
		}
	}
end

let next_expand expand = begin 
	let rec next_aux func pool = begin 
		match LitlEnum.next pool with
			None -> None
		| Some (pre_enum, pool') -> begin 
				let enum = func pre_enum in
				match LitlEnum.next enum with
					None -> next_aux func pool'
				| Some (value, enum') -> Some (
						value,
						{ apply = fun applier -> applier.f {
							func = func ;
							curr = enum' ;
							pool = pool'
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
				pool = expand.pool
			}
		}
	)
	| None -> next_aux expand.func expand.pool
end
let next t = begin 
  t.apply { f = next_expand }
end

let fold_expand f t r = begin 
	LitlEnum.fold
		(fun pre_enum r -> LitlEnum.fold f (t.func pre_enum) r)
		t.pool (LitlEnum.fold f t.curr r)
end
let fold f t r = begin 
  t.apply { f = fun g -> fold_expand f g r }
end

let iter_expand f t = begin 
	LitlEnum.iter f t.curr ;
	LitlEnum.iter (fun e -> LitlEnum.iter f (t.func e)) t.pool
end
let iter f t = begin 
  t.apply { f = fun g -> iter_expand f g }
end
