(************************************************************************
*  BinaryTreeMap.mli
*  
*
*  Created by Olivier Brunet on  30 Apr 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
************************************************************************)

type ('a, 'b, 'c) t =
	Empty
| Node of ('a, 'b, 'c) t * 'a * 'b * ('a, 'b, 'c) t * 'c

val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b, 'd) t -> 'c -> 'c
val fold_keys : ('a -> 'b -> 'b) -> ('a, 'c, 'd) t -> 'b -> 'b
val fold_values : ('a -> 'b -> 'b) -> ('c, 'a, 'd) t -> 'b -> 'b
val fold_elts : ('a * 'b -> 'c -> 'c) -> ('a, 'b, 'd) t -> 'c -> 'c

val iter : ('a -> 'b -> unit) -> ('a, 'b, 'c) t -> unit
val iter_keys : ('a -> unit) -> ('a, 'b, 'c) t -> unit
val iter_values : ('a -> unit) -> ('b, 'a, 'c) t -> unit
val iter_elts : ('a * 'b -> unit) -> ('a, 'b, 'c) t -> unit

val choose : ('a, 'b, 'c) t -> 'a * 'b
val choose_key : ('a, 'b, 'c) t -> 'a
val choose_value : ('a, 'b, 'c) t -> 'b

val leftmost : ('a, 'b, 'c) t -> 'a * 'b
val leftmost_key : ('a, 'b, 'c) t -> 'a
val leftmost_value : ('a, 'b, 'c) t -> 'b

val rightmost : ('a, 'b, 'c) t -> 'a * 'b
val rightmost_key : ('a, 'b, 'c) t -> 'a
val rightmost_value : ('a, 'b, 'c) t -> 'b
