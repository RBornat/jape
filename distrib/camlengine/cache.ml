(* $Id$
   A dom -> ran cache of eval, with initial table size 
 *)

module type T =
  sig
	type dom and ran
	val lookup : dom -> ran
	val reset  : unit -> unit
  end
  
functor F (AAA: struct type dom and ran 
					   val eval : dom->ran
					   val size : int
				end) : T with type dom = AAA.dom and type ran = AAA.ran
= 
  struct
	open Hashtbl
	let store = create size
	let lookup k = 
	  try find store k with Not_found -> let v = AAA.f k in add store k v; v
	let reset () = clear store
  end

