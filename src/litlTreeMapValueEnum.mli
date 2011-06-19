(************************************************************************
*  litlTreeMapValueEnum.mli
*  
*
*  Created by Olivier Brunet on 27 May 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
************************************************************************)

type 'a t

val make : ('a, 'b) LitlBinaryTreeMap.t -> 'b t

val next : 'a t -> ('a * 'a t) option

val iter : ('a -> unit) -> 'a t -> unit
val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val skip_until : ('a -> bool) -> 'a t -> ('a * 'a t) option
