(************************************************************************
*  litlDequeue.mli
*  
*
*  Created by Olivier Brunet on 19 Jun 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
************************************************************************)

type 'a t

val empty : 'a t
val is_empty : 'a t -> bool

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val iter : ('a -> unit) -> 'a t -> unit
val consl : 'a -> 'a t -> 'a t
val consr : 'a t -> 'a -> 'a t
val from_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val next : 'a t -> ('a * 'a t) option
val nextr : 'a t -> ('a * 'a t) option
val concat : 'a t -> 'a t -> 'a t
