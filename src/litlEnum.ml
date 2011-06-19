(************************************************************************
*
*  litlEnum.ml
*  
*
*  Created by Olivier Brunet on 27 May 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
*
************************************************************************)

class type ['a] enumerator = object ('enum)
	method next : ('a * 'enum) option
	method fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b
	method iter : ('a -> unit) -> unit
	method skip_until : ('a -> bool) -> ('a * 'enum) option
	method memoized : bool
end

type 'a t =
	Empty
| List of 'a list
| BT of 'a LitlTreeEnum.t
| BTMKey of 'a LitlTreeMapKeyEnum.t
| BTMValue of 'a LitlTreeMapValueEnum.t
| Once of (unit -> ('a * 'a t) option)
| Gen of 'a LitlGenerator.t
| Obj of 'a enumerator
| Memo of 'a LitlMemo.t
| Map of 'a LitlMapper.t
| Map_with_aux of 'a LitlMapperAux.t
| MapOpt of 'a LitlMapperOpt.t
| MapOptAux of 'a LitlMapperOptAux.t
| Filter of ('a -> bool) * 'a t
| Concat of 'a t * 'a t list
| Expand of 'a LitlExpander.t
| Expand_with_aux of 'a LitlExpanderAux.t
	
let empty = Empty

let from_list l = List l

let from_generator f state = Gen (LitlGenerator.make f state)

let from_enumerator e = Obj e

let from_binary_tree t = BT (LitlTreeEnum.make t)

let from_binary_tree_map m = begin 
	let obj = object 
		val em = LitlTreeMapEnum.make m

		method next = begin 
			match LitlTreeMapEnum.next em with
				None -> None
			| Some (v, em') -> Some (v, {< em = em' >})
		end

		method iter f = begin 
			LitlTreeMapEnum.iter f em
		end

		method fold : 'c. (('a * 'b) -> 'c -> 'c) -> 'c -> 'c = fun f a -> begin 
			LitlTreeMapEnum.fold f em a
		end
		
		method skip_until f = begin 
		  match LitlTreeMapEnum.skip_until f em with
		    None -> None
		  | Some (v, em') -> Some (v, {< em = em' >})
		end
		
		method memoized = begin 
			true
	  end
	end in
	Obj (obj : ('a * 'b) enumerator)
end

let from_binary_tree_map_keys m = begin 
	BTMKey (LitlTreeMapKeyEnum.make m)
end

let from_binary_tree_map_values m = begin 
	BTMValue (LitlTreeMapValueEnum.make m)
end

let from_once o = begin 
  Once o
end 

let cons elt enum = begin 
  match enum with
    List l -> List (elt :: l)
  | Concat (List l, enum_list) -> Concat (List (elt :: l), enum_list)
  | Concat (enum', enum_list) -> Concat (List [elt], enum' :: enum_list)
  | _ -> Concat (List [elt], [enum])
end

let rec next e = begin 
	match e with
		Empty -> None
	|	List [] -> None
	| List [v] -> Some (v, Empty)
	| List (v :: l) -> Some (v, List l)
	| BT t -> begin 
			match (LitlTreeEnum.next t) with
				None -> None
			| Some (v, t') -> Some (v, BT t')
	end
	| BTMKey t -> begin 
			match (LitlTreeMapKeyEnum.next t) with
				None -> None
			| Some (v, t') -> Some (v, BTMKey t')
	end
	| BTMValue t -> begin 
			match (LitlTreeMapValueEnum.next t) with
				None -> None
			| Some (v, t') -> Some (v, BTMValue t')
	end
	| Once o -> o ()
	| Gen g -> begin 
			match (LitlGenerator.next g) with
				None -> None
			| Some (v, g') -> Some (v, Gen g')
	end
	| Obj o -> begin
			match o#next with
				None -> None
			| Some (v, o') -> Some (v, Obj o')
	end
	| Map m -> begin 
			match (LitlMapper.next m) with
				None -> None
			| Some (v, m') -> Some (v, Map m')
	end
	| Map_with_aux m -> begin 
			match (LitlMapperAux.next m) with
				None -> None
			| Some (v, m') -> Some (v, Map_with_aux m')
	end
	| MapOpt m -> begin 
			match (LitlMapperOpt.next m) with
				None -> None
			| Some (v, m') -> Some (v, MapOpt m')
	end
	| MapOptAux m -> begin 
			match (LitlMapperOptAux.next m) with
				None -> None
			| Some (v, m') -> Some (v, MapOptAux m')
	end
	| Filter (p, e) -> begin 
			let rec aux p e = begin 
				match (next e) with
					None -> None
				| Some (v, e') -> begin 
						if (p v) then Some (v, Filter (p, e')) else aux p e'
				end
			end
			in aux p e
	end
	| Concat (e, el) -> begin 
			match (next e) with
				None -> begin 
					match el with
						[] -> None
					| (e' :: el') -> next (Concat (e', el'))
				end
			| Some (v, e') -> Some (v, Concat (e', el))
	end
	| Expand e -> begin 
			match (LitlExpander.next e) with
				None -> None
			| Some (v, e') -> Some (v, Expand e')
	end
	| Expand_with_aux e -> begin 
			match (LitlExpanderAux.next e) with
				None -> None
			| Some (v, e') -> Some (v, Expand_with_aux e')
	end
	| Memo m -> begin 
			match (LitlMemo.next m) with
				None -> None
			| Some (v, m') -> Some (v, Memo m')
	end
end

let rec fold f e r = begin 
	match e with
		Empty -> r
	| List l -> List.fold_left (fun r' e' -> f e' r') r l
	| Gen g -> LitlGenerator.fold f g r
	| BT t -> LitlTreeEnum.fold f t r
	| BTMKey t -> LitlTreeMapKeyEnum.fold f t r
	| BTMValue t -> LitlTreeMapValueEnum.fold f t r
	| Once o -> begin 
			match o () with
				None -> r
			| Some (v, e') -> fold f e' (f v r)
	end
	| Obj o -> o#fold f r
	| Map m -> LitlMapper.fold f m r
	| Map_with_aux m -> LitlMapperAux.fold f m r
	| MapOpt m -> LitlMapperOpt.fold f m r
	| MapOptAux m -> LitlMapperOptAux.fold f m r
	| Filter (p, e) ->
			fold (fun elt r -> if (p elt) then f elt r else r) e r
	| Concat (e, el) ->
			List.fold_left (fun r' e' -> fold f e' r') (fold f e r) el
	| Expand e -> LitlExpander.fold f e r
	| Expand_with_aux e -> LitlExpanderAux.fold f e r
	| Memo m -> LitlMemo.fold f m r
end	

let rec iter f e = begin 
	match e with
		Empty -> ()
	|	List l -> List.iter f l
	| BT t -> LitlTreeEnum.iter f t
	| BTMKey t -> LitlTreeMapKeyEnum.iter f t
	| BTMValue t -> LitlTreeMapValueEnum.iter f t
	| Gen g -> LitlGenerator.iter f g
	| Once o -> begin 
			match o () with
				None -> ()
			| Some (v, e) -> begin 
					f v ;
					iter f e
			end
	end
	| Obj o -> o#iter f
	| Map m -> LitlMapper.iter f m
	| Map_with_aux m -> LitlMapperAux.iter f m
	| MapOpt m -> LitlMapperOpt.iter f m
	| MapOptAux m -> LitlMapperOptAux.iter f m
	| Filter (p, e) ->
			iter (fun elt -> if p elt then f elt) e
	| Concat (e, el) -> begin 
			iter f e ;
			List.iter (fun e' -> iter f e') el
	end
	| Expand e -> LitlExpander.iter f e
	| Expand_with_aux e -> LitlExpanderAux.iter f e
	| Memo m -> LitlMemo.iter f m
end

let rec skip_until p e = begin 
	match e with
		Empty -> None
	|	List [] -> None
	| BT t -> begin 
			match (LitlTreeEnum.skip_until p t) with
				None -> None
			| Some (v, t') -> Some (v, BT t')
	end
	| BTMKey t -> begin 
			match (LitlTreeMapKeyEnum.skip_until p t) with
				None -> None
			| Some (v, t') -> Some (v, BTMKey t')
	end
	| BTMValue t -> begin 
			match (LitlTreeMapValueEnum.skip_until p t) with
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

let map f e = begin 
  Map (LitlMapper.make f (Obj.magic e))
end

let map_with_aux f e s = begin 
  Map_with_aux (LitlMapperAux.make f (Obj.magic e) s)
end 

let map_opt f e = begin 
  MapOpt (LitlMapperOpt.make f (Obj.magic e))
end

let map_opt_with_aux f e s = begin 
  MapOptAux (LitlMapperOptAux.make f (Obj.magic e) s)
end

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
				Concat (e2', el2) -> Concat (e1', (el1 @ (e2' :: el2)))
			| _ -> Concat (e1', el1 @ [e2])
		end
	| _ -> begin 
		match e2 with
			Concat (e2', el2) -> Concat (e1, e2' :: el2)
		| _ -> Concat (e1, [e2])
	end
end

let expand f p = begin 
  Expand (LitlExpander.make (Obj.magic f) (Obj.magic p))
end

let expand_with_aux f p s = begin 
  Expand_with_aux (LitlExpanderAux.make (Obj.magic f) (Obj.magic p) s)
end

let memo e = begin 
	match e with
		Empty -> e
	| List _ -> e
	| BT _ -> e
	| Memo _ -> e
	| Obj o -> if o#memoized then e else Memo (LitlMemo.make (Obj.magic e))
	|	_ -> Memo (LitlMemo.make (Obj.magic e))
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
