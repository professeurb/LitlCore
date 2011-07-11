(************************************************************************
*  litlFingerTree.mli
*  
*
*  Created by Olivier Brunet on 19 Jun 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
************************************************************************)

module type MONOID = sig 
  type 'a t
  val identity : 'a t
  val lift : 'a -> 'a t
  val combine : 'a t -> 'a t -> 'a t
end

module type FINGER_TREE = sig 
  type 'a m
  type 'a t
  
  val empty : 'a t
  val is_empty : 'a t -> bool
  
  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : ('a -> unit) -> 'a t -> unit
  
  val cons : 'a -> 'a t -> 'a t
  val cons_left : 'a -> 'a t -> 'a t
  val cons_right : 'a t -> 'a -> 'a t
  
  val from_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  
  val next : 'a t -> ('a * 'a t) option
  val next_right : 'a t -> ('a t * 'a) option
  
  val concat : 'a t -> 'a t -> 'a t
  
  val split_tree : ('a m -> bool) -> 'a m -> 'a t -> 'a t * 'a * 'a t
  val split : ('a m -> bool) -> 'a t -> 'a t * 'a t
end

module Make (M : MONOID) : FINGER_TREE with type 'a m = 'a M.t
