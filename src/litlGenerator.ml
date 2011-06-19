(************************************************************************
*
*  litlGenerator.ml
*  
*
*  Created by Olivier Brunet on 23 May 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
*
************************************************************************)

(* This module is mainly inspired from http://caml.inria.fr/pub/ml-archives/caml-list/2004/01/52732867110697f55650778d883ae5e9.en.html
*)

type ('a, 'b) generator = {
	func : 'a -> ('a * 'b) option ;
	state : 'a
}
type ('a, 'r) applier = { f : 'b. ('b, 'a) generator -> 'r }
type 'a t = { apply : 'r. ('a, 'r) applier -> 'r }

let make func state = begin 
	{	apply = fun applier -> applier.f
		{	func = func ;
			state = state
		}
	}
end

let next t = begin 
  let next_gen g = begin 
		match g.func g.state with
			None -> None
		| Some (state', value) -> Some (
			value, 
			{	apply = fun applier -> applier.f {
					func = g.func ;
					state = state'
				}
			}
		)
	end in
	t.apply { f = next_gen }
end

let fold f t a = begin 
  let fold_gen gen result = begin 
  	let rec aux state result = begin 
  		match gen.func state with
  		None -> result
  	| Some (state', value) -> aux state' (f value result)
  	end in
  	aux gen.state result
  end in
 	t.apply { f = fun g -> fold_gen g a }
end

let iter f t = begin 
  let iter_gen gen = begin 
  	let rec aux s = begin 
  		match gen.func s with
  		None -> ()
  	| Some (s', v) -> f v ; aux s'
  	end in
  	aux gen.state
  end in
  t.apply { f = fun g -> iter_gen g }
end
