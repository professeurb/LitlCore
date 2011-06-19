(************************************************************************
*  litlTreeMapEnum.mli
*  
*
*  Created by Olivier Brunet on 25 May 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
************************************************************************)

type ('a, 'b) t

val make : ('a, 'b) LitlBinaryTreeMap.t -> ('a, 'b) t
val next : ('a, 'b) t -> (('a * 'b) * ('a, 'b) t) option
val iter : ('a * 'b -> unit) -> ('a, 'b) t -> unit
val fold : ('a * 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
val skip_until :
	('a * 'b -> bool) -> ('a, 'b) t -> (('a * 'b) * ('a, 'b) t) option
