(************************************************************************
*
*  BinaryTreeMap.ml
*  
*
*  Created by Olivier Brunet on 30 Apr 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
*
************************************************************************)
(* 'a is the key, 'b is the value, 
  'c is some tree-structuring information. *)
type ('a, 'b, 'c) t =
	Empty
| Node of ('a, 'b, 'c) t * 'a * 'b * ('a, 'b, 'c) t * 'c

let rec fold f t res = begin 
	match t with
		Empty -> res
	| Node (l, k, v, r, _) -> fold f r (f k v (fold f l res))
end

let rec fold_keys f t res = begin 
	match t with
		Empty -> res
	| Node (l, k, v, r, _) -> fold_keys f r (f k (fold_keys f l res))
end

let rec fold_values f t res = begin 
	match t with
		Empty -> res
	| Node (l, k, v, r, _) -> fold_values f r (f v (fold_values f l res))
end

let rec fold_elts f t res = begin 
	match t with
		Empty -> res
	| Node (l, k, v, r, _) -> fold_elts f r (f (k, v) (fold_elts f l res))
end

let rec iter f t = begin 
	match t with
		Empty -> ()
	| Node (l, k, v, r, _) -> begin 
			iter f l ;
			f k v ;
			iter f r
	end
end

let rec iter_keys f t = begin 
	match t with
		Empty -> ()
	| Node (l, k, v, r, _) -> begin 
			iter_keys f l ;
			f k ;
			iter_keys f r
	end
end

let rec iter_values f t = begin 
	match t with
		Empty -> ()
	| Node (l, k, v, r, _) -> begin 
			iter_values f l ;
			f v ;
			iter_values f r
	end
end

let rec iter_elts f t = begin 
	match t with
		Empty -> ()
	| Node (l, k, v, r, _) -> begin 
			iter_elts f l ;
			f (k, v) ;
			iter_elts f r
	end
end

let choose t = begin 
	match t with
		Empty -> raise Not_found
	| Node (_, k, v, _, _) -> (k, v)
end

let choose_key t = begin 
	match t with
		Empty -> raise Not_found
	| Node (_, k, _, _, _) -> k
end

let choose_value t = begin 
	match t with
		Empty -> raise Not_found
	| Node (_, _, v, _, _) -> v
end

let leftmost t = begin 
	let rec aux l k v = begin 
		match l with
		  Empty -> (k, v)
		| Node (l', k', v', _, _) -> aux l' k' v'
  end in
  match t with
    Empty -> raise Not_found
  | Node (l, k, v, _, _) -> aux l k v
end

let leftmost_key t = begin 
	let rec aux l k = begin 
		match l with
		  Empty -> k
		| Node (l', k', _, _, _) -> aux l' k'
  end in
  match t with
    Empty -> raise Not_found
  | Node (l, k, _, _, _) -> aux l k
end

let leftmost_value t = begin 
	let rec aux l v = begin 
		match l with
		  Empty -> v
		| Node (l', _, v', _, _) -> aux l' v'
  end in
  match t with
    Empty -> raise Not_found
  | Node (l, _, v, _, _) -> aux l v
end

let rightmost t = begin 
	let rec aux k v r = begin 
		match r with
		  Empty -> (k, v)
		| Node (_, k', v', r', _) -> aux k' v' r'
  end in
  match t with
    Empty -> raise Not_found
  | Node (_, k, v, r, _) -> aux k v r
end

let rightmost_key t = begin 
	let rec aux k r = begin 
		match r with
		  Empty -> k
		| Node (_, k', _, r', _) -> aux k' r'
  end in
  match t with
    Empty -> raise Not_found
  | Node (_, k, _, r, _) -> aux k r
end

let rightmost_value t = begin 
	let rec aux v r = begin 
		match r with
		  Empty -> v
		| Node (_, _, v', r', _) -> aux v' r'
  end in
  match t with
    Empty -> raise Not_found
  | Node (_, _, v, r, _) -> aux v r
end
