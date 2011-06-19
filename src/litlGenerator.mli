(************************************************************************
*  litlGenerator.mli
*  
*
*  Created by Olivier Brunet on 23 May 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
************************************************************************)

type 'a t

val make : ('a -> ('a * 'b) option) -> 'a -> 'b t

val next : 'a t -> ('a * 'a t) option
val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val iter : ('a -> unit) -> 'a t -> unit
