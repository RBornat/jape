(*
    $Id$

    Copyright (C) 2003-8 Richard Bornat & Bernard Sufrin
     
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

(* a little lex/parse mechanism, to support languages made up of the following stuff:
 *     brackets
 *     atoms
 *     strings (atoms with internal spaces, delimited by " ")
 *     comments (sml-style)
 * and nothing else. They don't have to be S-exprs, but that's an example of such a language. 
 *
 * It uses character streams, so it can handle strings and stuff. 
 * UTF-8 encoded Unicode chars won't confuse it. 
 *)

let showchar c = implode ["'"; String.escaped (String.make 1 c); "'"])

let isblank c = member c [' '; '\n'; '\r'; '\t']

let check s c = 
	match Stream.peek s with
		| Some(c') -> 
				if c=c' then junk s 
				        else raise (ParseError_ [showchar c; " expected, "; showchar c'; " found"])
		| None ->
				raise (ParseError_ [showchar c; " expected, Eof found"])

let lexescape s =
	match Stream.npeek 3 s with
		| '\\'::_ -> junk s; '\\'
		| '\"'::_ -> junk s; '\"'
		| '\''::_ -> junk s; '\''
		| 'n' ::_ -> junk s; '\n'
	  | 'r' ::_ -> junk s; '\r'
	  | 't' ::_ -> junk s; '\t'
	  | 'b' ::_ -> junk s; '\b'
	  | ' ' ::_ -> junk s; ' '
	  | [c;h0;h1] when c='x' && ishexdigit h0 && ishexdigit h1 -> 
				njunk 3 s; Char.chr ((hexval h0)*16+hexval h1))
	  | [d0;d1;d2] when isdecdigit d0 && isdecdigit d1 && isdecdigit d2 -> 
				njunk 3 s; Char.chr ((decval d0)*10+decval d1)+decval d2
	  | c::cs when c='x' || isdecdigit c  -> 
				raise (ParseError_ ["invalid escape sequence \\"; utf8_implode (c::cs)])
	  | c::_                       -> 
				raise (ParseError_ ["invalid escape sequence \\"; utf8_of_ucode c])
		| []                         ->
				raise (ParseError_ ["invalid escape sequence \\ (nothing following the backslash!)"])

let lex s = (* read and return one symbol *)
	let rec lexatom cs =
		let finish() = implodef (List.rev cs) in
		match Stream.peek s with
			| Some c -> if isblank c then finish() else (junk s; lexatom (c::cs))
			| None   -> finish()
  in
	let rec lexstring cs =
		match Stream.peek s with
			| Some '\"' -> List.rev cs
			| Some '\\' -> (junk s; lexstring (lexescape s::cs)) 
			| Some c    -> lexstring (c::cs)
			| None      -> raise ParseError_ (["Eof inside string"])
	in
	try
		match Stream.next s with
		| Some c when c=Char.code '(' -> 
				if Stream.peek s=Some (Char.code '*') then (junk s; skipcomment s; lex s) else LParen
		| Some c when c=Char.code ')' -> RParen
		| Some c when c=Char.code '"' -> (let sy = String (lexstring []) in check s '"'; sy)
		| Some c                      -> if isblank c then lex s else Atom (lexatom s [c])
	with Stream.Failure -> Eof

