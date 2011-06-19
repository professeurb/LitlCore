(************************************************************************
*
*  litlMapperAux.ml
*  
*
*  Created by Olivier Brunet on 27 May 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
*
************************************************************************)


type ('a, 'b, 'c) mapper = {
	func : 'a -> 'c -> ('b * 'c) option ;
	enum : 'a LitlEnum.t ;
	state : 'c
}
type ('a, 'r) applier = { f : 'b 'c. ('b, 'a, 'c) mapper -> 'r }
type 'a t = { apply : 'r. ('a, 'r) applier -> 'r }

exception Eject

let make f e s = begin 
	{ apply = fun applier -> applier.f {
			func = f ;
			enum = e ;
			state = s
		}
	}
end

let next_map m = begin 
	match LitlEnum.next m.enum with
		None -> None
	| Some (v, e') -> begin 
		match (m.func v m.state) with
			None -> None
		| Some (v', s') -> Some (
			v', 
			{	apply = fun applier -> applier.f {
					func = m.func;
					enum = e';
					state = s'
				}
			}
		)
	end
end
let next t = begin 
  t.apply { f = next_map }
end

let fold_mapper f m r = begin 
	let res = ref r in
	let ff elt state = begin 
		match m.func elt state with
			None -> raise Eject
		| Some (elt', state') -> begin 
				res := f elt' !res;
				state'
		end
	end in
	try
		let _ = LitlEnum.fold ff m.enum m.state
		in !res
	with Eject -> !res
end
let fold f t r = begin 
  t.apply { f = fun m -> fold_mapper f m r }
end

let iter_mapper f m = begin 
	try
		let _ = LitlEnum.fold (
			fun elt s ->
				match m.func elt s with
					None -> raise Eject
				| Some (elt', s') -> begin 
						f elt' ;
						s'
				end
			) m.enum m.state in
			()
	with Eject -> ()
end
let iter f t = begin 
  t.apply { f = fun g -> iter_mapper f g }	
end
