(*
    $Id$
    Copyright (C) 2003-14 Richard Bornat & Bernard Sufrin
     
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

   
  
   let versionstring = "$Name$" 
   let rec get_version s =
     let cs = Sml.chars_of_string s in
     let cs = Listfuns.dropwhile (function ' ' -> false | _ -> true) cs in
     let cs = Listfuns.dropwhile (fun c -> 'a'<=c && c<='z' || 'A'<=c&&c<='z') (List.tl cs) in
     let cs = Listfuns.takewhile (function ' ' -> false | '$' -> false | _ -> true) cs in
     if Sml.null cs then get_version "$Date$" else
                         Sml.string_of_chars (List.map (function '_' -> '.' | c -> c) cs)
   let _Version = get_version versionstring
   let _Title = "Jape proof engine "






