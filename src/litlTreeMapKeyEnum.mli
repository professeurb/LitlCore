(************************************************************************
*  litlTreeMapKeyEnum.mli
*  
*
*  Created by Olivier Brunet on 25 May 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
************************************************************************)

type 'a t 

val make : ('a, 'b) LitlBinaryTreeMap.t -> 'a t

val next : 'a t -> ('a * 'a t) option
val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val iter : ('a -> unit) -> 'a t -> unit

val skip_until : ('a -> bool) -> 'a t -> ('a * 'a t) option
