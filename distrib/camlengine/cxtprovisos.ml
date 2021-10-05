(*
    Copyright (C) 2003-19 Richard Bornat & Bernard Sufrin
     
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

open Cxttype

(* for now, provisosigs are ints *)
let getprovisos = fun (Context {provisos = provisos}) -> provisos
(* this function used by rewrite, which is manipulating the provisosig intelligently *)
let setprovisos =
fun (Context cxt) ps ->
Context {cxt with provisos = ps}
let getprovisosig =
fun (Context {provisosig = provisosig}) -> provisosig
let nextprovisosig = ref 0
(* at 1000 contexts/sec, this will last 2^30/1000 = 1M seconds.  Long enough (we are
* certainly not doing 1K contexts/sec, and when we do, we will undoubtedly have 
* 64-bit desktop machines).
* RB 14/viii/97
*)
let incprovisosig =
fun (Context cxt) ->
let bang () =
  raise (Miscellaneous.Catastrophe_ ["STOP, STOP, STOP!!!! too many contexts!!!!"])
in
(try incr nextprovisosig with _ -> bang ());
if !nextprovisosig <= 0 then bang () else ();
Context {cxt with provisosig = !nextprovisosig}
