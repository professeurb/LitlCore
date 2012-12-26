(************************************************************************
*  BinaryTree.mli
*  
*
*  Created by Olivier Brunet on 13 Apr 2010.
*  Copyright (c) 2010 B.W.C. Computing. All rights reserved.
************************************************************************)

(* Basically, 'a is used to store data, 
  'b is used to add internal information such as
  a height or a color *)
type ('a, 'b) t = Empty | Node of ('a, 'b) t * 'a * ('a, 'b) t * 'b

val fold : ('a -> 'b -> 'b) -> ('a, 'c) t -> 'b -> 'b
val iter : ('a -> unit) -> ('a, 'b) t -> unit

val choose : ('a, 'b) t -> 'a
val leftmost : ('a, 'b) t -> 'a
val rightmost : ('a, 'b) t -> 'a
