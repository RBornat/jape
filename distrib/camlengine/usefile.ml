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

open Sml

let bracketedliststring = Listfuns.bracketedliststring
let consolereport = Miscellaneous.consolereport

(* this is the Unix version ... Linux and MacOS X ok; 
   Windoze needs '\\' so we stay in the world of unix filenames 
   and normalize filenames according to OS just before opening
*)

let normalizePath filename = 
if Sys.os_type="Win32" 
then 
   let sloshify = function "/"->"\\" | c -> c
   in Sml.implode (List.map sloshify (UTF.utf8_explode filename))
else filename

let pathStem path =
  (* Remove the stern of a path 
     Path separator is immaterial so we can mix unix/windows-style paths
     in source files.
  *)
  let rec removestern =
  function
    | [] -> ["./"]
    | "/"  :: xs -> "/"  :: xs
    | "\\" :: xs -> "\\" :: xs  
    | x :: xs -> removestern xs
  in
    Sml.implode (List.rev (removestern (List.rev (UTF.utf8_explode path))))
 
let open_input_file  filename = open_in  (normalizePath filename)

let open_output_file filename = open_out (normalizePath filename)

(* It's not good enough ... we ought to parse the strings. RB 3/i/2002 *)

let usestack = ref (if Sys.os_type="Win32" then [] else ["./"])

let isabsolute path =
if Sys.os_type="Win32" then (
  try
	String.index path ':'  < String.index path '\\'
  with
	Not_found -> false)
else (try path.[0]='/' with _ -> false)

let makerelative s =
  if isabsolute s then s else
  match !usestack with
  | []     -> s
  | top::_ -> top ^ s

let rec startusing path =
  usestack := pathStem path :: !usestack

exception Matchinstopusing (* spurious *)

let rec stopusing () =
  match !usestack with
    [path] -> ()
  | path :: paths -> usestack := paths
  | _ -> raise Matchinstopusing

