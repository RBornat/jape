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

open Panelkind
open Name
 
val panelbuttoninsertstring : panelbuttoninsert -> string
type menudata =
    Mseparator
  | Mentry       of (name * string option * string)
  | Mcheckbox    of (name * name * (string * string) * string option)
  | Mradiobutton of (name * (name * string) list * string option)
                (* variable  label  cmd            default cmd *)

type paneldata =
    Pentry       of (name * string)
  | Pbutton      of (name * panelbuttoninsert list)
  (* these are not used any more: I don't know how they could be treated in the GUI;
                                  they were never used, so far as I know.
                                  RB 30/xi/2002
     | Pcheckbox    of (name * name * (string * string) * string option)
     | Pradiobutton of (name * (name * string) list * string option)
                   (* variable  label  cmd            default cmd *)
   *)

val addmenu       : bool -> name -> unit
val addmenudata   : name -> menudata list -> unit
val clearmenudata : name -> unit
val getmenus      : unit -> (bool * name) list
val getmenudata   : name -> (bool * menudata list) option

val addpanel       : panelkind -> name -> unit
val addpaneldata   : name -> paneldata list -> unit
val clearpaneldata : name -> unit
val getpanels      : unit -> (name * panelkind) list
val getpanelkind   : name -> panelkind option
val getpaneldata   : name -> paneldata list option

val clearmenusandpanels : unit -> unit

exception Menuconfusion_ of string list

val menudebug : bool ref

val menuiter : (bool * name -> unit) -> unit
val paneliter : (name * panelkind -> unit) -> unit
val menuitemiter :
  name -> (name * string option * string -> unit) ->
	(name * string -> unit) -> ((name * string) list -> unit) ->
	(unit -> unit) -> unit
val panelitemiter :
  name -> (name * string -> unit) -> (name * panelbuttoninsert list -> unit) 
    (* -> (name * string -> unit) -> ((name * string) list -> unit) *) -> unit

