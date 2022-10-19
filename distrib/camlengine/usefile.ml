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

open Sml
open UTF

let bracketed_string_of_list = Listfuns.bracketed_string_of_list
let consolereport = Miscellaneous.consolereport

(* this is the Unix version ... Linux and MacOS X ok; 
   Windoze needs '\\' so we stay in the world of unix filenames 
   and normalize filenames according to OS just before opening
*)

let normalizePath filename = 
  if Sys.os_type="Win32" 
  then 
     (* 0x2f = '/'
        0x5c = '\\'
      *)
     let sloshify = function 0x2f->0x5c | c -> c in 
     utf8_implode (List.map sloshify (utf8_explode filename))
  else filename

let pathStem path =
  (* Remove the stern of a path 
     Path separator is immaterial so we can mix unix/windows-style paths
     in source files.
  *)
   (* 0x2f = '/'
      0x5c = '\\'
    *)
  let rec removestern =
  function
    | []          -> [Char.code '.'; Char.code '/']
    | 0x2f  :: xs -> 0x2f  :: xs
    | 0x5c :: xs  -> 0x5c :: xs  
    | x :: xs     -> removestern xs
  in
    utf8_implode (List.rev (removestern (List.rev (utf8_explode path))))

let list_of_Unixpath path =
  let rec ds pres path =
    match path with 
    | ""  -> pres
    | _   -> let dir = Filename.dirname path in
             if dir = path then path :: pres 
                           else ds (Filename.basename path :: pres) dir
  in
  ds [] path

let warned = ref ""
 
(* I changed this function to check if it is running on MacOS, and if so whether it is trying 
   to read from one of the protected directories Desktop, Documents, Downloads and Library.
   
   Then I discovered that MacOS will let me read from one file at a time. So now it reads 
   the entire input file into a buffer, closes the file, and gives you a UTF.ucode stream 
   of its contents. Works in an a.out that I compiled ... but definitely doesn't work in Jape.
   
   So I left the check in place, but it now delivers a (UTF.ucode stream) option, because
   that seems to make more sense elsewhere.
 *)
let open_input_file filename = 
  let default () = Some (UTF.of_utfchannel (open_in (normalizePath filename))) in
     if Miscellaneous.onMacOS () then
       (let ss = list_of_Unixpath filename in
        (match ss with
         | "/" :: "Users" :: name :: place :: path 
              when List.mem place ["Desktop"; "Documents"; "Downloads"; "Library"] &&
                   List.mem "examples" path &&
                   !warned <> place
              -> (let message = "It looks as if you have put your examples directory somewhere \
                                 in your " ^ place ^ " folder. This doesn't work: because Jape is \
                                 not notarised by Apple, it won't be able to read the examples files.\n \
                                 \n\
                                 The only thing to do is to move the examples folder somewhere outside \
                                 the protected folders Desktop, Documents, Downloads and Library.\n \
                                 \n\
                                 Please press Cancel, move the folder, and Open the moved folder. \
                                 If you press Proceed it really really really won't work."
                  in
                  match Alert.askCancel Alert.Warning message [("Proceed",0)] (-1) 1 with
                  | 0 -> warned := place; default ()
                  | _ -> None
                 )
         | _  -> default ()
        )
       )
     else default ()  

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
  | [path]        -> ()
  | path :: paths -> usestack := paths
  | _             -> raise Matchinstopusing

