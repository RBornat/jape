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

open Seqtype
open Symboltype
open Mappingfuns
open Termtype
open Idclass

val describeSeqs : (idclass * string * idclass) list -> unit
val getsemanticturnstile : string -> string option
val setsemanticturnstile : string -> string -> unit
val parseSeq : unit -> seq
val canstartSeq : symbol -> bool
val resetsyntaxandturnstiles : unit -> unit

val string_of_seq : seq -> string
val invisbracketedstring_of_seq : bool -> seq -> string
val debugstring_of_seq : seq -> string
val elementstring_of_seq : seq -> string
val catelim_string_of_seq : seq -> string list -> string list
val catelim_invisbracketedstring_of_seq : bool -> seq -> string list -> string list
val catelim_debugstring_of_seq : seq -> string list -> string list
val catelim_elementstring_of_seq : seq -> string list -> string list
val alwaysshowturnstile : bool ref

val sequent_of_string : string -> seq
val seqexplode : seq -> string * term * term
val seqvars :
(term -> 'a list) -> ('a list -> 'a list -> 'a list) -> seq -> 'a list
val seqVIDs : seq -> vid list
val eqseqs : seq * seq -> bool
val seqmatch :
bool -> seq -> seq -> (term, term) mapping ->
(term, term) mapping option
val seqmatchvars :
bool -> (term -> bool) -> seq -> seq -> (term, term) mapping ->
(term, term) mapping option
val remapseq : (term, term) mapping -> seq -> seq
val mkSeq : string * element list * element list -> seq
val maxseqresnum : seq -> int
val syntacticturnstiles : unit -> string list

val pushSyntax     : string -> unit
val popSyntax      : unit -> unit
val popAllSyntaxes : unit -> unit

