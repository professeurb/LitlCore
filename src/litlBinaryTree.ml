(************************************************************************
*
*  BinaryTree.ml
*  
*
*  Created by Olivier Brunet on 13 Apr 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
*
************************************************************************)

type ('a, 'b) t = Empty | Node of ('a, 'b) t * 'a * ('a, 'b) t * 'b

let rec fold f t res = begin 
	match t with
	|	Empty -> res
	| Node (l, v, r, _) -> fold f r (f v (fold f l res))
end

let rec iter f t = begin 
	match t with
	|	Empty -> ()
	| Node (l, v, r, _) -> begin 
			iter f l ;
			f v ;
			iter f r
	end
end

let choose t = begin 
	match t with
	|	Empty -> raise Not_found
	| Node (_, v, _, _) -> v
end

let leftmost t = begin 
	let rec aux l v = begin 
	  match l with
	  | Empty -> v
	  | Node (l', v', _, _) -> aux l' v'
	end in
	match t with
	| Empty -> raise Not_found
	| Node (l, v, _, _) -> aux l v
end

let rightmost t = begin
	let rec aux v r = begin 
	  match r with
	    Empty -> v
	  | Node (_, v', r', _) -> aux v' r'
	end in
	match t with
	  Empty -> raise Not_found
	| Node (_, v, r, _) -> aux v r
end
