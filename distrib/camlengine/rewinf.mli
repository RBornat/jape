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

open Termtype

type rewinf

val mkrewinf : term list * vid list * int list * int option -> rewinf
val nullrewinf : rewinf
val rawinf_of_rew : rewinf -> term list * vid list * int list * int option
val rewinf_addbadres : rewinf -> int list -> rewinf
val rewinf_adduVIDs : rewinf -> vid list -> rewinf
val rewinf_addvars : rewinf -> term list -> rewinf
val rewinf_badres : rewinf -> int list
val rewinf_merge : rewinf * rewinf -> rewinf
val rewinf_psig : rewinf -> int option
val rewinf_setbadres : rewinf -> int list -> rewinf
val rewinf_setpsig : rewinf -> int option -> rewinf
val rewinf_setuVIDs : rewinf -> vid list -> rewinf
val rewinf_setvars : rewinf -> term list -> rewinf
val rewinf_uVIDs : rewinf -> vid list
val rewinf_vars : rewinf -> term list

val string_of_rewinf : rewinf -> string


