(************************************************************************
*
*  litlMapperOpt.ml
*  
*
*  Created by Olivier Brunet on 27 May 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
*
************************************************************************)


type ('a, 'b) mapper = {
	func : 'a -> 'b option ;
	enum : 'a LitlEnum.t
}
type ('a, 'r) applier = { f : 'b. ('b, 'a) mapper -> 'r }
type 'a t = { apply : 'r. ('a, 'r) applier -> 'r }

let make f e = begin 
	{ apply = fun applier -> applier.f
		{	func = f ;
			enum = e
		}
	}
end

let next_mapper m = begin 
	let rec aux e = begin 
		match LitlEnum.next e with
			None -> None
		| Some (v, e') -> begin 
			  match m.func v with
					None -> aux e'
				| Some v' -> Some (
						v', {
							apply = fun applier -> applier.f {
								func = m.func ;
								enum = e'
							}
						}
					)
		end
	end in
	aux m.enum
end
let next t = begin 
  t.apply { f = next_mapper }
end

let fold_mapper f m r = begin 
	LitlEnum.fold (
		fun elt r -> begin 
			match m.func elt with
				None -> r
			| Some elt' -> f elt' r
		end
	) m.enum r
end
let fold f t r = begin 
  t.apply { f = fun m -> fold_mapper f m r}
end

let iter_mapper f m = begin 
	LitlEnum.iter (
		fun elt -> begin 
			match m.func elt with
				None -> ()
			| Some elt' -> f elt'
		end
	) m.enum
end
let iter f t = begin 
  t.apply { f = fun m -> iter_mapper f m}
end
