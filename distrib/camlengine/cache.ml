(* $Id$
   A dom -> ran cache of eval, with initial table size 
 *)

module type T =
  sig
	type dom and ran
	val lookup : dom -> ran
	val reset  : unit -> unit
  end
  
module F (AAA: sig type dom and ran 
				   val eval : dom->ran
				   val size : int
			   end) : T with type dom = AAA.dom and type ran = AAA.ran
= 
  struct
    type dom = AAA.dom and ran = AAA.ran
	open Hashtbl
	let store = create AAA.size
	let lookup k = 
	  try find store k with Not_found -> let v = AAA.eval k in add store k v; v
	let reset () = clear store
  end

