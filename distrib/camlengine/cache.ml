(*
	$Id$

    Copyright (C) 2003-4 Richard Bornat & Bernard Sufrin
     
        richard@bornat.me.uk
        sufrin@comlab.ox.ac.uk

    This file is part of the jape proof engine, which is part of jape.

    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).

*)

(* A dom -> ran cache of eval, with initial table size *)

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

