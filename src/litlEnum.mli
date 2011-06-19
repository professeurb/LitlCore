(************************************************************************
*  litlEnum.mli
*  
*
*  Created by Olivier Brunet on 27 May 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
************************************************************************)

class type ['a] enumerator = object ('enum)
	method next : ('a * 'enum) option
	method fold : 'b. ('a -> 'b -> 'b) -> 'b -> 'b
	method iter : ('a -> unit) -> unit
	method skip_until : ('a -> bool) -> ('a * 'enum) option
	method memoized : bool
end

type 'a t

val empty : 'a t

val from_list : 'a list -> 'a t
val from_generator : ('a -> ('a * 'b) option) -> 'a -> 'b t
val from_unit_generator : (unit -> 'a option) -> 'a t
val from_enumerator : 'a enumerator -> 'a t
val from_binary_tree : 'a LitlBinaryTree.t -> 'a t
val from_binary_tree_map : ('a, 'b) LitlBinaryTreeMap.t -> ('a * 'b) t
val from_binary_tree_map_keys : ('a, 'b) LitlBinaryTreeMap.t -> 'a t
val from_binary_tree_map_values : ('a, 'b) LitlBinaryTreeMap.t -> 'b t
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
