(************************************************************************
*
*  litlMemo.ml
*  
*
*  Created by Olivier Brunet on 27 May 2011.
*  Copyright (c) 2011 B.W.C. Computing. All rights reserved.
*
************************************************************************)


type 'a t = ('a cell) ref
and 'a cell = Empty | Enum of 'a LitlEnum.t | Value of 'a * 'a t

let make e = ref (Enum e)

let next m = begin 
	match !m with
		Empty -> None
	| Value (v, m') -> Some (v, m')
	| Enum e -> begin 
			match LitlEnum.next e with
				None -> begin 
					m := Empty ;
					None
				end
			| Some (v, e') -> begin 
					let m' = ref (Enum e') in
					begin 
						m := Value (v, m') ;
						Some (v, m')
					end
			end
	end
end

let rec fold f m r = begin 
	match next m with
		None -> r
	| Some (v, m') -> fold f m' (f v r)
end

let rec iter f m = begin 
	match next m with
		None -> ()
	| Some (v, m') -> begin 
		f v ;
		iter f m'
	end
end
