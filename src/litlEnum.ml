(************************************************************************
*
*  Enumerator.ml
*  
*
*  Created by Olivier Brunet on 3 Jan 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
*
************************************************************************)

(* See http://caml.inria.fr/pub/ml-archives/caml-list/2004/01/52732867110697f55650778d883ae5e9.en.html
*)

module BT = LitlBinaryTree
module BTM = LitlBinaryTreeMap

class type ['a] enumerator = object ('enum)
	method next : ('a * 'enum) option
	method fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b
	method iter : ('a -> unit) -> unit
	method skip_until : ('a -> bool) -> ('a * 'enum) option
	method memoized : bool
end

module TreeEnumerator : sig 
	type 'a t
	
	val make : ('a, 'b) BT.t -> 'a t
	val next : 'a t -> ('a * 'a t) option
	val iter : ('a -> unit) -> 'a t -> unit
	val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
	val skip_until : ('a -> bool) -> 'a t -> ('a * 'a t) option
end
= struct 
	(* An 'a zipper as actually some kind of partial tree zipper *)
	type ('a, 'b) zipper = End | More of 'a * ('a, 'b) BT.t * ('a, 'b) zipper

	type 'a t = C : ('a, 'b) BT.t * ('a, 'b) zipper -> 'a t

	let make t = begin 
		C (t, End)
	end

	let rec next (C (t, z)) = begin 
		match (t, z) with
			(BT.Empty, End) -> None
		| (BT.Empty, More (v, bt, z)) -> Some (v, C (bt, z))
		| (BT.Node (BT.Empty, v, right, _), z) ->
				Some (v, C (right, z))
		| (BT.Node (left, v, right, _), z) ->
				next (C (left, More (v, right, z)))
	end

	let iter f (C (t, z)) = begin 
		let rec aux z = begin 
			match z with
				End -> ()
			| More (v, t, z') -> begin 
					f v ;
					BT.iter f t ;
					aux z'
			end
		end in
		BT.iter f t ;
		aux z
	end

	let fold f (C (t, z)) res = begin 
		let rec aux z r = begin 
			match z with
				End -> r
			| More (v, t, z') ->
					aux z' (BT.fold f t (f v r))
		end
		in
		aux z (BT.fold f t res)
	end

	let skip_until f (C (t, z)) = begin 
	  let rec aux_2 t z = begin 
	    match t with
	      BT.Node (l, v, r, _) ->
	        if f v
	        then aux_2 l (More (v, r, z))
	        else aux_2 r z
	    | BT.Empty -> begin 
	        match z with
	          End -> None
	        | More (v, t', z') -> Some (v, C (t', z'))
	    end
	  end in
	  let rec aux_1 t z = begin 
	    match z with
	      More (v, t', z') -> 
	        if f v then aux_2 t z else aux_1 t' z'
	    | End -> aux_2 t z
	  end in
	  aux_1 t z
	end
end

module TreeMapEnumerator : sig 
	type ('a, 'b) t
	
	val make : ('a, 'b, 'c) BTM.t -> ('a, 'b) t
	val next : ('a, 'b) t -> (('a * 'b) * ('a, 'b) t) option
	val iter : ('a * 'b -> unit) -> ('a, 'b) t -> unit
	val fold : ('a * 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
	val skip_until :
		('a * 'b -> bool) -> ('a, 'b) t -> (('a * 'b) * ('a, 'b) t) option
end
= struct 
	type ('a, 'b, 'c) zipper = End | More of
			('a * 'b) *
			('a, 'b, 'c) BTM.t * ('a, 'b, 'c) zipper

	type ('a, 'b) t = C : ('a, 'b, 'c) BTM.t * ('a, 'b, 'c) zipper -> ('a, 'b) t

	let make t = begin 
		C (t, End)
	end

	let rec next (C (t, z)) = begin 
		match (t, z) with
			(BTM.Empty, End) -> None
		| (BTM.Empty, More (v, bt, z)) -> Some (v, C (bt, z))
		| (BTM.Node (BTM.Empty, v, k, r, _), z) -> Some ((v, k), C (r, z))
		| (BTM.Node (l, v, k, r, _), z) -> next (C (l, More ((v, k), r, z)))
	end

	let iter f (C (bt, z)) = begin 
		let rec aux z = begin 
			match z with
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

	let fold f (C (t, z)) res = begin 
		let rec aux z r = begin 
			match z with
				End -> r
			| More (v, bt', z') ->
					aux z' (BTM.fold_elts f bt' (f v r))
		end in
		aux z (BTM.fold_elts f t res)
	end

	let skip_until f (C (bt, z)) = begin 
	  let rec aux_2 bt z = begin 
	    match bt with
	      BTM.Node (l, k, v, r, _) ->
	        if f (k, v)
	        then aux_2 l (More ((k, v), r, z))
	        else aux_2 r z
	    | BTM.Empty -> begin 
	        match z with
	          End -> None
	        | More (kv, bt', z') -> Some (kv, C (bt', z'))
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
end

module TreeMapKeyEnumerator : sig 
	type 'a t
	
	val make : ('a, 'b, 'c) BTM.t -> 'a t
	val next : 'a t -> ('a * 'a t) option
	val iter : ('a -> unit) -> 'a t -> unit
	val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
	val skip_until :
		('a -> bool) -> 'a t -> ('a * 'a t) option
end
= struct 
	type ('a, 'b, 'c) zipper = End | More of
			'a *
			('a, 'b, 'c) BTM.t * ('a, 'b, 'c) zipper

	type 'a t = C : ('a, 'b, 'c) BTM.t * ('a, 'b, 'c) zipper -> 'a t

	let make t = begin 
		C (t, End)
	end

	let rec next (C (t, z)) = begin 
		match (t, z) with
			(BTM.Empty, End) -> None
		| (BTM.Empty, More (k, bt, z)) -> Some (k, C (bt, z))
		| (BTM.Node (BTM.Empty, k, _, r, _), z) -> Some (k, C (r, z))
		| (BTM.Node (l, k, _, r, _), z) -> next (C (l, More (k, r, z)))
	end

	let iter f (C (bt, z)) = begin 
		let rec aux z = begin 
			match z with
				End -> ()
			| More (k, bt', z') -> begin 
					f k ;
					BTM.iter_keys f bt' ;
					aux z'
			end
		end in
		BTM.iter_keys f bt ;
		aux z
	end

	let fold f (C (t, z)) res = begin 
		let rec aux z r = begin 
			match z with
				End -> r
			| More (k, bt', z') ->
					aux z' (BTM.fold_keys f bt' (f k r))
		end in
		aux z (BTM.fold_keys f t res)
	end

	let skip_until f (C (bt, z)) = begin 
	  let rec aux_2 bt z = begin 
	    match bt with
	      BTM.Node (l, k, _, r, _) ->
	        if f k
	        then aux_2 l (More (k, r, z))
	        else aux_2 r z
	    | BTM.Empty -> begin 
	        match z with
	          End -> None
	        | More (k, bt', z') -> Some (k, C (bt', z'))
	    end
	  end in
	  let rec aux_1 bt z = begin 
	    match z with
	    More (k, bt', z') -> 
	      if f k then aux_2 bt z else aux_1 bt' z'
	  | End -> aux_2 bt z
	  end in
	  aux_1 bt z
	end
end

module TreeMapValueEnumerator : sig 
	type 'a t
	
	val make : ('a, 'b, 'c) BTM.t -> 'b t
	val next : 'a t -> ('a * 'a t) option
	val iter : ('a -> unit) -> 'a t -> unit
	val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
	val skip_until :
		('a -> bool) -> 'a t -> ('a * 'a t) option
end
= struct 
	type ('a, 'b, 'c) zipper =  End
	| More of 'b * ('a, 'b, 'c) BTM.t * ('a, 'b, 'c) zipper

	type 'b t = C : ('a, 'b, 'c) BTM.t * ('a, 'b, 'c) zipper -> 'b t

	let make t = begin 
		C (t, End)
	end

	let rec next (C (t, z)) = begin 
		match (t, z) with
			(BTM.Empty, End) -> None
		| (BTM.Empty, More (v, bt, z)) -> Some (v, C (bt, z))
		| (BTM.Node (BTM.Empty, _, v, r, _), z) -> Some (v, C (r, z))
		| (BTM.Node (l, _, v, r, _), z) -> next (C (l, More (v, r, z)))
	end

	let iter f (C (bt, z)) = begin 
		let rec aux z = begin 
			match z with
				End -> ()
			| More (v, bt', z') -> begin 
					f v ;
					BTM.iter_values f bt' ;
					aux z'
			end
		end in
		BTM.iter_values f bt ;
		aux z
	end

	let fold f (C (t, z)) res = begin 
		let rec aux z r = begin 
			match z with
				End -> r
			| More (v, bt', z') ->
					aux z' (BTM.fold_values f bt' (f v r))
		end in
		aux z (BTM.fold_values f t res)
	end

	let skip_until f (C (bt, z)) = begin 
	  let rec aux_2 bt z = begin 
	    match bt with
	      BTM.Node (l, _, v, r, _) ->
	        if f v
	        then aux_2 l (More (v, r, z))
	        else aux_2 r z
	    | BTM.Empty -> begin 
	        match z with
	          End -> None
	        | More (k, bt', z') -> Some (k, C (bt', z'))
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
end


module Generator : sig 
	type 'a t
	val make : ('a -> ('a * 'b) option) -> 'a -> 'b t
	val next : 'a t -> ('a * 'a t) option
	val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
	val iter : ('a -> unit) -> 'a t -> unit
end
= struct 
	type 'a t = C : ('a -> ('a * 'b) option) * 'a -> 'b t 

	let make f s = begin 
		C (f, s)
	end

	let next (C (f, s)) = begin 
	  match f s with
	    None -> None
	  | Some (s', r) -> Some (r, C (f, s'))
	end

  let fold f (C (g, s)) a = begin 
    let rec aux s a = begin 
      match g s with
        None -> a
      | Some (s', r) -> aux s' (f r a)
    end in
    aux s a
  end

  let iter f (C (g, s)) = begin 
    let rec aux s = begin 
      match g s with
        None -> ()
      | Some (s', r) -> f r ; aux s'
    end in
    aux s
  end
end

module rec Enum : sig 
	type 'a t
	
	val empty : 'a t
	
	val from_list : 'a list -> 'a t
	val from_generator : ('a -> ('a * 'b) option) -> 'a -> 'b t
	val from_unit_generator : (unit -> 'a option) -> 'a t
	val from_object : 'a enumerator -> 'a t
	val from_binary_tree : ('a, 'b) BT.t -> 'a t
	val from_binary_tree_map : ('a, 'b, 'c) BTM.t -> ('a * 'b) t
	val from_binary_tree_map_keys : ('a, 'b, 'c) BTM.t -> 'a t
	val from_binary_tree_map_values : ('a, 'b, 'c) BTM.t -> 'b t
	val from_stream : 'a Stream.t -> 'a t
	val from_once : (unit -> ('a * 'a t) option) -> 'a t
	val cons : 'a -> 'a t -> 'a t

	val next : 'a t -> ('a * 'a t) option
	val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
	val iter : ('a -> unit) -> 'a t -> unit
	val skip_until : ('a -> bool) -> 'a t -> ('a * 'a t) option
	
	val map : ('a -> 'b) -> 'a t -> 'b t
	val map_with_aux : ('a -> 'c -> ('b * 'c) option) -> 'a t -> 'c -> 'b t
	val map_opt : ('a -> 'b option) -> 'a t -> 'b t
	val map_opt_with_aux :
		('a -> 'b -> ('c option * 'b) option) -> 'a t -> 'b -> 'c t
	val filter : ('a -> bool) -> 'a t -> 'a t
	val concat : 'a t -> 'a t -> 'a t
	val expand : ('a -> 'b t) -> 'a t -> 'b t
	val expand_with_aux : ('a -> 'c -> ('b t * 'c)) -> 'a t -> 'c -> 'b t
	val memo : 'a t -> 'a t
end
= struct 
	type 'a t = |
		Empty
	| List of 'a list
	| BT of 'a TreeEnumerator.t
	(* | BTM : ('a, 'b) TreeMapEnumerator.t -> ('a * 'b) t *)
	| BTMKey of 'a TreeMapKeyEnumerator.t
	| BTMValue of 'a TreeMapValueEnumerator.t
	| Once of (unit -> ('a * 'a t) option)
	| Gen of 'a Generator.t 
	| Obj of 'a enumerator
	| Memo of 'a Memo.t
	| Map of 'a Mapper.t
	| Map_with_aux of 'a MapperAux.t
	| MapOpt of 'a MapperOpt.t
	| MapOptAux of 'a MapperOptAux.t
	| Filter of ('a -> bool) * 'a t
	| Concat of 'a t * 'a t LitlDeque.t
	| Expand of 'a Expander.t
	| Expand_with_aux of 'a ExpanderAux.t
		
	let empty = Empty

	let from_list l = List l

	let from_generator f state = Gen (Generator.make f state)

	let from_object e = Obj e

	let from_binary_tree t = BT (TreeEnumerator.make t)

	let from_binary_tree_map m = begin 
		let obj = object 
			val em = TreeMapEnumerator.make m

			method next = begin 
				match TreeMapEnumerator.next em with
					None -> None
				| Some (v, em') -> Some (v, {< em = em' >})
			end

			method iter f = begin 
				TreeMapEnumerator.iter f em
			end

			method fold : 'c. (('a * 'b) -> 'c -> 'c) -> 'c -> 'c = fun f a -> begin 
				TreeMapEnumerator.fold f em a
			end

			method skip_until f = begin 
			  match TreeMapEnumerator.skip_until f em with
			    None -> None
			  | Some (v, em') -> Some (v, {< em = em' >})
			end

			method memoized = begin 
				true
		  end
		end in
		Obj (obj : ('a * 'b) enumerator)
	end
	(* let from_binary_tree_map t = BTM (TreeMapEnumerator.make t) *)

	let from_binary_tree_map_keys m = begin 
		BTMKey (TreeMapKeyEnumerator.make m)
	end

	let from_binary_tree_map_values m = begin 
		BTMValue (TreeMapValueEnumerator.make m)
	end

	let from_once o = Once o

	let cons elt enum = begin 
	  match enum with
	    List l -> List (elt :: l)
	  | Concat (List l, enum_list) -> Concat (List (elt :: l), enum_list)
	  | Concat (enum', enum_list) ->
		    Concat (List [elt], LitlDeque.cons enum' enum_list)
	  | _ -> Concat (List [elt], LitlDeque.cons enum LitlDeque.empty)
  end

	let rec next = function 
			Empty -> None
		|	List [] -> None
		| List [v] -> Some (v, Empty)
		| List (v :: l) -> Some (v, List l)
		| BT t -> begin 
				match (TreeEnumerator.next t) with
					None -> None
				| Some (v, t') -> Some (v, BT t')
		end
		(* | BTM t -> begin 
		    match (TreeMapEnumerator.next t) with
		      None -> None
		    | Some (kv, t') -> Some (kv, BTM t')
		end *)
		| BTMKey t -> begin 
				match (TreeMapKeyEnumerator.next t) with
					None -> None
				| Some (k, t') -> Some (k, BTMKey t')
		end
		| BTMValue t -> begin 
				match (TreeMapValueEnumerator.next t) with
					None -> None
				| Some (v, t') -> Some (v, BTMValue t')
		end
		| Once o -> o ()
		| Gen g -> begin 
				match (Generator.next g) with
					None -> None
				| Some (v, g') -> Some (v, Gen g')
		end
		| Obj o -> begin
				match o#next with
					None -> None
				| Some (v, o') -> Some (v, Obj o')
		end
		| Map m -> begin 
				match (Mapper.next m) with
					None -> None
				| Some (v, m') -> Some (v, Map m')
		end
		| Map_with_aux m -> begin 
				match (MapperAux.next m) with
					None -> None
				| Some (v, m') -> Some (v, Map_with_aux m')
		end
		| MapOpt m -> begin 
				match (MapperOpt.next m) with
					None -> None
				| Some (v, m') -> Some (v, MapOpt m')
		end
		| MapOptAux m -> begin 
				match (MapperOptAux.next m) with
					None -> None
				| Some (v, m') -> Some (v, MapOptAux m')
		end
		| Filter (p, e) -> begin 
				let rec aux p e = begin 
					match (next e) with
						None -> None
					| Some (v, e') -> 
							if (p v) then Some (v, Filter (p, e')) else aux p e'
				end
				in aux p e
		end
		| Concat (e, el) -> begin 
				match (next e) with
					None -> begin 
						match LitlDeque.next el with
							None -> None
						| Some (e', el') -> next (Concat (e', el'))
					end
				| Some (v, e') -> Some (v, Concat (e', el))
		end
		| Expand e -> begin 
				match (Expander.next e) with
					None -> None
				| Some (v, e') -> Some (v, Expand e')
		end
		| Expand_with_aux e -> begin 
				match (ExpanderAux.next e) with
					None -> None
				| Some (v, e') -> Some (v, Expand_with_aux e')
		end
		| Memo m -> begin 
				match (Memo.next m) with
					None -> None
				| Some (v, m') -> Some (v, Memo m')
		end
	
	let rec fold f e r = begin 
		match e with
			Empty -> r
		| List l -> List.fold_left (fun r' e' -> f e' r') r l
		| Gen g -> Generator.fold f g r
		| BT t -> TreeEnumerator.fold f t r
		(* | BTM t -> TreeMapEnumerator.fold f t r *)
		| BTMKey t -> TreeMapKeyEnumerator.fold f t r
		| BTMValue t -> TreeMapValueEnumerator.fold f t r
		| Once o -> begin 
				match o () with
					None -> r
				| Some (v, e') -> fold f e' (f v r)
		end
		| Obj o -> o#fold f r
		| Map m -> Mapper.fold f m r
		| Map_with_aux m -> MapperAux.fold f m r
		| MapOpt m -> MapperOpt.fold f m r
		| MapOptAux m -> MapperOptAux.fold f m r
		| Filter (p, e) ->
				fold (fun elt r -> if (p elt) then f elt r else r) e r
		| Concat (e, el) ->
				LitlDeque.fold_left (fun r' e' -> fold f e' r') (fold f e r) el
		| Expand e -> Expander.fold f e r
		| Expand_with_aux e -> ExpanderAux.fold f e r
		| Memo m -> Memo.fold f m r
	end	
	
	let rec iter f e = begin 
		match e with
			Empty -> ()
		|	List l -> List.iter f l
		| BT t -> TreeEnumerator.iter f t
		(* | BTM t -> TreeMapEnumerator.iter f t *)
		| BTMKey t -> TreeMapKeyEnumerator.iter f t
		| BTMValue t -> TreeMapValueEnumerator.iter f t
		| Gen g -> Generator.iter f g
		| Once o -> begin 
				match o () with
					None -> ()
				| Some (v, e) -> begin 
						f v ;
						iter f e
				end
		end
		| Obj o -> o#iter f
		| Map m -> Mapper.iter f m
		| Map_with_aux m -> MapperAux.iter f m
		| MapOpt m -> MapperOpt.iter f m
		| MapOptAux m -> MapperOptAux.iter f m
		| Filter (p, e) ->
				iter (fun elt -> if p elt then f elt) e
		| Concat (e, el) -> begin 
				iter f e ;
				LitlDeque.iter (fun e' -> iter f e') el
		end
		| Expand e -> Expander.iter f e
		| Expand_with_aux e -> ExpanderAux.iter f e
		| Memo m -> Memo.iter f m
	end
	
	let rec skip_until p e = begin 
		match e with
			Empty -> None
		|	List [] -> None
		| BT t -> begin 
				match (TreeEnumerator.skip_until p t) with
					None -> None
				| Some (v, t') -> Some (v, BT t')
		end
		| BTMKey t -> begin 
				match (TreeMapKeyEnumerator.skip_until p t) with
					None -> None
				| Some (v, t') -> Some (v, BTMKey t')
		end
		| BTMValue t -> begin 
				match (TreeMapValueEnumerator.skip_until p t) with
					None -> None
				| Some (v, t') -> Some (v, BTMValue t')
		end
		| Obj o -> begin
				match o#skip_until p with
					None -> None
				| Some (v, o') -> Some (v, Obj o')
		end
		| _ -> begin 
		  let rec aux e = begin 
		    match next e with
		      None -> None
		    | Some (v, e') -> if p v then Some (v, e') else aux e'
		  end in
		  aux e
		end
	end

	let map f e = Map (Mapper.make f e)

	let map_with_aux f e s = Map_with_aux (MapperAux.make f e s)

	let map_opt f e = MapOpt (MapperOpt.make f e)

	let map_opt_with_aux f e s = MapOptAux (MapperOptAux.make f e s)
	
	let filter p e = begin 
		match e with
			Empty -> Empty
		| Filter (p', e') -> Filter ((fun elt -> (p' elt) && (p elt)), e')
		| _ -> Filter (p, e)
	end

	let concat e1 e2 = begin 
		match e1 with
			Concat (e1', el1) -> begin 
				match e2 with
					Concat (e2', el2) ->
						Concat (e1', LitlDeque.concat el1 (LitlDeque.cons e2' el2))
				| _ -> Concat (e1', LitlDeque.cons_right el1 e2)
			end
		| _ -> begin 
			match e2 with
				Concat (e2', el2) -> Concat (e1, LitlDeque.cons e2' el2)
			| _ -> Concat (e1, LitlDeque.cons e2 LitlDeque.empty)
		end
	end

	let expand f p = Expand (Expander.make f p)

	let expand_with_aux f p s = Expand_with_aux (ExpanderAux.make f p s)
	
	let memo e = begin 
		match e with
			Empty -> e
		| List _ -> e
		| BT _ -> e
		| Memo _ -> e
		| Obj o -> if o#memoized then e else Memo (Memo.make e)
		|	_ -> Memo (Memo.make e)
	end

  let from_unit_generator f = begin 
    let rec g_enum = Once (begin 
      fun () -> match f () with
         None -> None
       | Some elt -> Some (elt, g_enum)
	  end
	  ) in
	  memo g_enum
	end
	
  let from_stream stream = begin 
    from_unit_generator (
	    fun () -> try Some (Stream.next stream) with Stream.Failure -> None
	  )
  end
end

and Mapper : sig 
	type 'a t
	val make : ('a -> 'b) -> 'a Enum.t -> 'b t
	val next : 'a t -> ('a * 'a t) option
	val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
	val iter : ('a -> unit) -> 'a t -> unit
end
= struct 
	type 'b t = C : ('a -> 'b) * 'a Enum.t -> 'b t
	
	let make f e = C (f, e)

	let next (C (f, e)) = begin 
	  match Enum.next e with
	    None -> None
	  | Some (r, e') -> Some (f r, C (f, e'))
	end
	
	let fold f (C (m, e)) = begin 
	  Enum.fold (fun a b -> f (m a) b) e
  end

	let iter f (C (m, e)) = begin 
	  Enum.iter (fun a -> f (m a)) e
  end
end

and MapperAux : sig 
	type 'a t
	val make :
		('a -> 'c -> ('b * 'c) option) -> 'a Enum.t -> 'c -> 'b t
	val next : 'a t -> ('a * 'a t) option
	val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
	val iter : ('a -> unit) -> 'a t -> unit
end
= struct 
	type ('a, 'b, 'c) mapper = {
		func : 'a -> 'c -> ('b * 'c) option ;
		enum : 'a Enum.t ;
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
		match Enum.next m.enum with
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
	let nexter = { f = next_map }
	let next t = t.apply nexter
	
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
			let _ = Enum.fold ff m.enum m.state
			in !res
		with Eject -> !res
	end
	let fold f t r = t.apply { f = fun m -> fold_mapper f m r }

	let iter_mapper f m = begin 
		try
			let _ = Enum.fold (
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
	let iter f t = t.apply { f = fun g -> iter_mapper f g }	
end

and MapperOpt : sig 
	type 'a t
	val make : ('a -> 'b option) -> 'a Enum.t -> 'b t
	val next : 'a t -> ('a * 'a t) option
	val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
	val iter : ('a -> unit) -> 'a t -> unit
end
= struct 
	type ('a, 'b) mapper = {
		func : 'a -> 'b option ;
		enum : 'a Enum.t
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
			match Enum.next e with
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
		(* match Enum.next m.enum with
			None -> None
		| Some (v, e') -> begin 
				match m.func v with
					None -> ...
					Some v' -> Some (
						v', 
						{ apply = fun applier -> applier.f {
								func = m.func ;
								enum = e'
							}
						}	
					)
		end
	end *)
	let nexter = { f = next_mapper }
	let next t = t.apply nexter
	
	let fold_mapper f m r = begin 
		Enum.fold (
			fun elt r -> begin 
				match m.func elt with
					None -> r
				| Some elt' -> f elt' r
			end
		) m.enum r
	end
	let fold f t r = t.apply { f = fun m -> fold_mapper f m r}

	let iter_mapper f m = begin 
		Enum.iter (
			fun elt -> begin 
				match m.func elt with
					None -> ()
				| Some elt' -> f elt'
			end
		) m.enum
	end
	let iter f t = t.apply { f = fun m -> iter_mapper f m}
end

and MapperOptAux : sig 
	type 'a t
	val make :
		('a -> 'c -> ('b option * 'c) option) -> 'a Enum.t -> 'c -> 'b t
	val next : 'a t -> ('a * 'a t) option
	val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
	val iter : ('a -> unit) -> 'a t -> unit
end
= struct 
	type ('a, 'b, 'c) mapper = {
		func : 'a -> 'c -> ('b option * 'c) option ;
		enum : 'a Enum.t ;
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
		let rec aux e s = begin 
			match Enum.next e with
				None -> None
			| Some (v, e') -> begin 
				match (m.func v s) with
					None -> None
				| Some (None, s') -> aux e' s'
				| Some (Some v', s') -> Some (
					v', { 
						apply = fun applier -> applier.f {
							func = m.func;
							enum = e';
							state = s'
						}
					}
				)
			end
		end in
		aux m.enum m.state
	end
	let nexter = { f = next_map }
	let next t = t.apply nexter
	
	let fold_mapper f m r = begin 
		let res = ref r in
		let ff elt state = begin 
			match m.func elt state with
				None -> raise Eject
			| Some (None, state') -> state'
			| Some (Some elt', state') -> begin 
					res := f elt' !res;
					state'
			end
		end in
		try
			let _ = Enum.fold ff m.enum m.state
			in !res
		with Eject -> !res
	end
	let fold f t r = t.apply { f = fun m -> fold_mapper f m r }

	let iter_mapper f m = begin 
		try
			let _ = Enum.fold (
				fun elt s ->
					match m.func elt s with
						None -> raise Eject
					| Some (None, s') -> s'
					| Some (Some elt', s') -> begin 
							f elt' ;
							s'
					end
				) m.enum m.state in
				()
		with Eject -> ()
	end
	let iter f t = t.apply { f = fun g -> iter_mapper f g }	
end

and Expander : sig 
  type 'a t 
  val make : ('a -> 'b Enum.t) -> 'a Enum.t -> 'b t
  val next : 'a t -> ('a * 'a t) option
  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : ('a -> unit) -> 'a t -> unit
end
= struct 
	type ('a, 'b) expander = {
		func : 'a -> 'b Enum.t ;
		curr : 'b Enum.t ;
		pool : 'a Enum.t
	}
	type ('a, 'r) applier = { f : 'b. ('b, 'a) expander -> 'r }
	type 'a t = { apply : 'r. ('a, 'r) applier -> 'r }
	
	let make func pool = begin 
		{ apply = fun applier -> applier.f
			{	func = func ;
				curr = Enum.empty ;
				pool = pool
			}
		}
	end

	let next_expand expand = begin 
		let rec next_aux func pool = begin 
			match Enum.next pool with
				None -> None
			| Some (pre_enum, pool') -> begin 
					let enum = func pre_enum in
					match Enum.next enum with
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
		match Enum.next expand.curr with
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
	let nexter = { f = next_expand }
	let next t = t.apply nexter
	
	let fold_expand f t r = begin 
		Enum.fold
			(fun pre_enum r -> Enum.fold f (t.func pre_enum) r)
			t.pool (Enum.fold f t.curr r)
	end
	let fold f t r = t.apply { f = fun g -> fold_expand f g r }

	let iter_expand f t = begin 
		Enum.iter f t.curr ;
		Enum.iter (fun pre_enum -> Enum.iter f (t.func pre_enum)) t.pool
	end
	let iter f t = t.apply { f = fun g -> iter_expand f g }
end

and ExpanderAux : sig 
  type 'a t 
  val make : ('a -> 'c -> 'b Enum.t * 'c ) -> 'a Enum.t -> 'c -> 'b t
  val next : 'a t -> ('a * 'a t) option
  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : ('a -> unit) -> 'a t -> unit
end
= struct 
	type ('a, 'b, 'c) expander = {
		func : 'a -> 'c -> 'b Enum.t * 'c ;
		curr : 'b Enum.t ;
		pool : 'a Enum.t ;
		state : 'c
	}
	type ('a, 'r) applier = { f : 'b 'c. ('b, 'a, 'c) expander -> 'r }
	type 'a t = { apply : 'r. ('a, 'r) applier -> 'r }
	
	let make func pool state = begin 
		{ apply = fun applier -> applier.f
			{	func = func ;
				curr = Enum.empty ;
				pool = pool ;
				state = state
			}
		}
	end

	let next_expand expand = begin 
		let rec next_aux func pool state = begin 
			match Enum.next pool with
				None -> None
			| Some (pre_enum, pool') -> begin 
					let (enum, state') = func pre_enum state in
					match Enum.next enum with
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
		match Enum.next expand.curr with
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
	let nexter = { f = next_expand }
	let next t = t.apply nexter
	
	let fold_expand f t r = begin 
		let (res, _) = Enum.fold (
			fun pre_enum (r, state) ->
				let (enum, state') = t.func pre_enum state in
				(Enum.fold f enum r, state')
			) t.pool (Enum.fold f t.curr r, t.state) in
		res
	end
	let fold f t r = t.apply { f = fun g -> fold_expand f g r }

	let iter_expand f t = begin 
		Enum.iter f t.curr ;
		let _ = Enum.fold (
			fun pre_enum state ->
				let (enum, state') = t.func pre_enum state in
				Enum.iter f enum ; state'
			) t.pool t.state
		in ()
	end
	let iter f t = t.apply { f = fun g -> iter_expand f g }
end

and Memo : sig 
	type 'a t 
	val make : 'a Enum.t -> 'a t
	val next : 'a t -> ('a * 'a t) option
	val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
	val iter : ('a -> unit) -> 'a t -> unit
end
= struct 
	type 'a t = ('a cell) ref
	and 'a cell = C_Empty | C_Enum of 'a Enum.t | C_Value of 'a * 'a t
	
	let make e = ref (C_Enum e)
	
	let next m = begin 
		match !m with
			C_Empty -> None
		| C_Value (v, m') -> Some (v, m')
		| C_Enum e -> begin 
				match Enum.next e with
					None -> begin 
						m := C_Empty ;
						None
					end
				| Some (v, e') -> begin 
						let m' = ref (C_Enum e') in
						begin 
							m := C_Value (v, m') ;
							Some (v, m')
						end
				end
		end
	end
	
	let rec fold f m r = begin 
		match next m with
			None -> r
		| Some (v, m') -> fold f m' (f v r)
	end
	
	let rec iter f m = begin 
		match next m with
			None -> ()
		| Some (v, m') -> begin 
			f v ;
			iter f m'
		end
	end
end

include Enum

type 'a enum = 'a t

let range a b = begin 
	if b > a
	then
		from_generator (fun n -> if n > b then None else Some (n+1, n)) a
	else
		from_generator (fun n -> if n < b then None else Some (n-1, n)) a
end

let counter = begin 
	from_generator (fun n -> Some (n+1, n)) 0
end

let trim n e = begin 
	map_with_aux (
		fun elt count -> 
			if count < n
			then Some (elt, count + 1)
			else None
	) e 0
end ;;

(* let prime = from_generator (
	fun e -> match next e with
		None -> None (* Should not happen *)
	| Some (e', n) -> Some (filter (fun x -> x mod n <> 0) e', n)
	) (from_generator (fun n -> Some (n+1, n)) 2) ;; *)

	(* let prime = generator (
		fun e -> match e#next with
			None -> None (* Should not happen *)
		| Some (n, e') -> Some (n, filter (fun x -> x mod n <> 0) e')
		) (generator (fun n -> Some (n, n+1)) 2) ;; *)
		
let to_list e = begin 
	fold (fun a l -> a :: l) e []
end ;;

(* 
let ( => ) a b = expand b a ;;
let ( =? ) a b = filter b a ;;
let ( =! ) a b = map b a ;;
let ( -- ) a b = range a b ;; 


to_list (( 1 -- 10 ) => (range 1)) ;;

let pythagorean n = 
	(1 -- n) => (fun x -> (1 -- n) =! (fun y -> (x, y)) )
		=> (fun (x, y) -> map (fun z -> (x, y, z)) (1 -- n))
		=? (fun (x, y, z) -> x*x + y*y = z*z)

let chrono f = begin 
	let t1 = Sys.time () in
	let r = f () in
	let t2 = Sys.time () in
	(t2 -. t1, r)
end ;;

module S = Set.Make (struct type t = int let compare = compare end)

let prime = 
	let rec prime_aux set n =
		if S.for_all (fun p -> n mod p <> 0) set
		then n
		else prime_aux set (n+1)
	in
from_generator (fun (s, n) -> let n' = prime_aux s n in Some ((S.add n' s, n' + 1), n')) (S.empty, 2)

let prime' = memo prime ;;

chrono (fun () -> to_list (trim 10000 prime)) ;;
(* 13.96 14.23 14.17 *)

chrono (fun () -> to_list (trim 10000 prime')) ;;

*)

(* version 2 : 11.74 11.73 11.71 *)

(* version 3 : with Map *)

(* version 4 : objets *)
(* 7.676 *)
(* chrono (fun () -> to_list (from_generator (
	fun e -> match next e with
		None -> None
	| Some (e', n) -> Some (filter (fun x -> x mod n <> 0) e', n)
	) (range 2 104729))) ;;
(* On peut améliorer next//filter *)

*)

(*
let crapou n e = begin 
  let rec aux e = begin 
    match next e with
      None -> true
    | Some (p, e') -> if p*p > n then true else if n mod p = 0 then false else aux e'
  end in
  aux e
end ;;

let ints_2 = from_generator (fun n -> Some (n + 1, n)) 2 ;;

let rec p_prime () = memo (cons 2 (from_once (fun () -> aux 3)))
and aux n = begin
  if crapou n (p_prime ()) 
  then Some (n, from_once (fun () -> aux (n+1)))
  else aux (n + 1)
end
and prime = p_prime () ;;

let rec prime = memo (ints_2 =? (fun n -> crapou n prime))
*)