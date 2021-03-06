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

include Screendraw.T

val hidecut          : bool ref
val hidehyp          : bool ref
val hidetransitivity : bool ref
val hidereflexivity  : bool ref
val hidewhy          : bool ref

val outermostbox   : bool ref
val innerboxes     : bool ref
val showrelations  : bool ref
val showboxfreshvs : bool ref

val priorAntes : bool ref

val outerassumptionword   : string ref
val outerassumptionplural : string ref
val innerassumptionword   : string ref
val innerassumptionplural : string ref

val boxlinedressright : bool ref

val boxseldebug  : bool ref
val boxfolddebug : bool ref

val formulaefolded        : bool ref
val assumptionlinesfolded : bool ref
val reasonstruncated      : bool ref