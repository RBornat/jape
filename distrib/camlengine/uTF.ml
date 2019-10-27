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

open Listfuns
open Miscellaneous
open Sml
open Stringfuns

type ucode = int

let uEOF = -1

let hexstring_of_char c = enCharQuote ("x" ^ fixedwidth_hexstring_of_int 2 (Char.code c))
let hex4 n = "0x" ^ fixedwidth_hexstring_of_int 4 n
let hex8 n = "0x" ^ fixedwidth_hexstring_of_int 8 n

exception MalformedUTF_ of string list

let check_808f c =
  match c with
    '\x80'..'\x8f' -> Char.code c land 0x3f
  | _              -> raise (MalformedUTF_ [hexstring_of_char c])

let check_809f c =
  match c with
    '\x80'..'\x9f' -> Char.code c land 0x3f
  | _              -> raise (MalformedUTF_ [hexstring_of_char c])

let check_80bf c =
  match c with
    '\x80'..'\xbf' -> Char.code c land 0x3f
  | _              -> raise (MalformedUTF_ [hexstring_of_char c])

let check_90bf c =
  match c with
    '\x90'..'\xbf' -> Char.code c land 0x3f
  | _              -> raise (MalformedUTF_ [hexstring_of_char c])

let check_a0bf c =
  match c with
    '\xa0'..'\xbf' -> Char.code c land 0x3f
  | _              -> raise (MalformedUTF_ [hexstring_of_char c])
  
let utf8_next next =
  try let n0 = Char.code (next 0) in
      try
        let twobyte f = 
          let n1 = f (next 1) in
          ((n0 land 0x1f) lsl 6) lor n1 in
        let threebyte f = 
          let n1 = f (next 1) in
          try let n2 = check_80bf (next 2) in
              ((((n0 land 0x0f) lsl 6) lor n1) lsl 6) lor n2 
          with Stream.Failure   -> raise (MalformedUTF_ [hexstring_of_char (Char.chr n1); " EOF"])
             | MalformedUTF_ ss -> raise (MalformedUTF_ (hexstring_of_char (Char.chr n1) :: " " :: ss))
        in
        let fourbyte f = 
          let n1 = f (next 1) in
          try let n2 = check_80bf (next 2) in
              try let n3 = check_80bf (next 3) in      
                  ((((((n0 land 0x07) lsl 6) lor n1) lsl 6) lor n2) lsl 6) lor n3 
              with MalformedUTF_ ss -> raise (MalformedUTF_ (hexstring_of_char (Char.chr n2) :: " " :: ss))
          with Stream.Failure   -> raise (MalformedUTF_ [hexstring_of_char (Char.chr n1); " EOF"])
             | MalformedUTF_ ss -> raise (MalformedUTF_ (hexstring_of_char (Char.chr n1) :: " " :: ss))
        in
        match n0 with
          | n when 0x00<=n && n<=0x7f ->
              n0
          | n when 0xc2<=n && n<=0xdf ->
              twobyte check_80bf
          | 0xe0 ->
              threebyte check_a0bf
          | n when (0xe1<=n && n<=0xec) || (0xee<=n && n<=0xef) ->
              threebyte check_80bf
          | 0xed ->
              threebyte check_809f
          | 0xf0 ->
              fourbyte check_90bf
          | n when 0xf1<=n && n<=0xf3 ->
              fourbyte check_80bf
          | 0xf4 ->
              fourbyte check_808f
          | _ -> 
              raise (MalformedUTF_ [])
      with
        Stream.Failure   -> raise (MalformedUTF_ [hexstring_of_char (Char.chr n0); " EOF"])
      | MalformedUTF_ ss -> raise (MalformedUTF_ (hexstring_of_char (Char.chr n0) :: " " :: ss))
  with 
    MalformedUTF_ ss -> raise (MalformedUTF_ ("UTF8 " :: ss))

let next16_be s = 
  let n0 = Char.code (Stream.next s) in
  try let n1 = Char.code (Stream.next s) in
      (n0 lsl 8) lor n1
  with Stream.Failure -> raise (MalformedUTF_ [hexstring_of_char (Char.chr n0); " EOF"])
  
let next16_le s = 
  let n1 = Char.code (Stream.next s) in
  try let n0 = Char.code (Stream.next s) in
      (n0 lsl 8) lor n1
  with Stream.Failure -> raise (MalformedUTF_ [hexstring_of_char (Char.chr n1); " EOF"])

let utf16_next next16 s =
  try match next16 s with
        m when 0x0000<=m && m<=0xd7ff -> 
          m
      | m1 when 0xd800<=m1 && m1<=0xdbff ->
          (try
             match next16 s with 
                m2 when 0xdc00<=m2 && m2<=0xdfff -> (
                  match (((m1 land 0x3ff) lsl 10) lor (m2 land 0x3ff)) + 0x10000 with
                    m when (0x0000<=m && m<=0xd7ff) || (0xe000<=m && m<=0x10ffff) ->
                      m
                  | m ->
                      raise (MalformedUTF_ [hex4 m1; " "; hex4 m2; " => "; hex8 m; " -- no such code point"]))
              | m2 ->
                  raise (MalformedUTF_ [hex4 m1; " "; hex4 m2])
           with 
             Stream.Failure -> raise (MalformedUTF_ [hex4 m1; " EOF"]))
       | m1 -> raise (MalformedUTF_ [hex4 m1])
  with 
    MalformedUTF_ ss -> raise (MalformedUTF_ ("UTF16 " :: ss))

let next32_be s = 
  let n0 = next16_be s in
  try let n1 = next16_be s in
      (n0 lsl 16) lor n1
  with Stream.Failure   -> raise (MalformedUTF_ [hex4 n0; " EOF"])
     | MalformedUTF_ ss -> raise (MalformedUTF_ (hex4 n0 :: " " :: ss))
  
let next32_le s = 
  let n1 = next16_le s in
  try let n0 = next16_le s in
      (n0 lsl 16) lor n1
  with Stream.Failure   -> raise (MalformedUTF_ [hex4 n1; " EOF"])
     | MalformedUTF_ ss -> raise (MalformedUTF_ (hex4 n1 :: " " :: ss))

let utf32_next next32 s =
  try match next32 s with
        m when (0x0000<=m && m<=0xd7ff) || (0xe000<=m && 0xe000<=0x10ffff) -> 
          m
      | m                                                                  -> 
          raise (MalformedUTF_ ["UTF32 "; hex8 m; " -- no such code point"])
  with 
    MalformedUTF_ ss -> raise (MalformedUTF_ ("UTF32 " :: ss))

(************************************)

let utf8_get s i = 
  if i=String.length s then uEOF else
  utf8_next (fun j -> try s.[i+j] with _ -> raise Stream.Failure)
  
let utf8_next s = utf8_next (fun _ -> Stream.next s)

let utf16_next bigendian = 
  utf16_next (if bigendian then next16_be else next16_le)
let utf32_next bigendian = 
  utf32_next (if bigendian then next32_be else next32_le)

(*****************************)

let of_utfstream reader s =
  Stream.from (fun _ -> try Some(reader s) with Stream.Failure -> None)

let utf_stdin = of_utfstream utf8_next (Stream.of_channel stdin)

let rec njunk n s =
  if n>0 then (Stream.junk s; njunk (n-1) s)
  
let of_utfchannel ic =
  let s = Stream.of_channel ic in
  (* look for BOM; without a BOM default is UTF8 *)
  let reader = 
    match Stream.npeek 4 s with 
      ['\x00'; '\x00'; '\xfe'; '\xff'] -> njunk 4 s; utf32_next true
    | ['\xff'; '\xfe'; '\x00'; '\x00'] -> njunk 4 s; utf32_next false
    | cs ->
        match take 3 cs with 
          ['\xef'; '\xbb'; '\xbf'] -> njunk 3 s; utf8_next
        | _ -> 
            match take 2 cs with
              ['\xfe'; '\xff'] -> njunk 2 s; utf16_next true
            | ['\xff'; '\xfe'] -> njunk 2 s; utf16_next false
            | _                -> utf8_next
  in
  of_utfstream reader s

let stream_of_utfNinchannel size bigendian ic =
  let s = Stream.of_channel ic in
    match size with
      8 -> (
        (match Stream.npeek 3 s with
           ['\xef'; '\xbb'; '\xbf'] -> njunk 3 s
         | _                        -> ());
        of_utfstream utf8_next s)
    | 16 -> (
        (match bigendian, Stream.npeek 2 s with
           true , ['\xfe'; '\xff'] -> njunk 2 s
         | false, ['\xff'; '\xfe'] -> njunk 2 s
         | _                       -> ());
        of_utfstream (utf16_next bigendian) s)
    | 32 -> (
        (match bigendian, Stream.npeek 4 s with
           true , ['\x00'; '\x00'; '\xfe'; '\xff'] -> njunk 4 s
         | false, ['\xff'; '\xfe'; '\x00'; '\x00'] -> njunk 4 s
         | _                       -> ());
        of_utfstream (utf32_next bigendian) s)
    | _ -> raise (Miscellaneous.Catastrophe_ ["utf8_of_utfNchannel "; string_of_int size])

let stream_of_utf8string s = 
  of_utfstream utf8_next (Stream.of_string s)
  
let open_out_utf8 s =
  let ic = open_out s in
  output_string ic Miscellaneous.utf8BOM;
  ic

(****************************)

let utf8_widths = Array.make 0x100 0

(* we trust the encoding at this point *)
let _ = for i=0x00 to 0x7f do utf8_widths.(i)<-1 done;
        for i=0xc2 to 0xdf do utf8_widths.(i)<-2 done;
        for i=0xe0 to 0xef do utf8_widths.(i)<-3 done;
        for i=0xf0 to 0xf4 do utf8_widths.(i)<-4 done
        
let utf8width_from_header c =
  utf8_widths.(Char.code c)
  
let utf8_explode s =
  let lim = String.length s in
  let rec f i =
    if i=lim then [] else
      let n = utf8width_from_header s.[i] in
      utf8_get s i :: f (i+n)
  in
  f 0 

let utf8_sub s i =
  if i = String.length s then uEOF else utf8_get s i

let utf8_presub s i =
  if i=0 then uEOF else 
    let rec f j =
      if j<0 then 
        raise (MalformedUTF_ 
                 ["UTF8 string "; bracketed_string_of_list hexstring_of_char "; " (chars_of_string (String.sub s 0 i))])
      else (
        let n = utf8width_from_header s.[j] in
        if n>0 then utf8_get s j
        else f (j-1))    
      in
      f (i-1)

(**********************************************)
  
let utf8_of_ucode m cs =
  if m = uEOF then cs else
  if 0x00<=m && m<=0x07f then
       Char.chr m :: cs
  else
  if 0x80<=m && m<=0x7ff then (
       let y = m lsr 6 in
       let x = m land 0x3f in
       Char.chr (y lor 0xc0) :: Char.chr (x lor 0x80) :: cs)
  else
  if (0x800<=m && m<=0xdfff) || (0xe0000<=m && 0xe0000<=0xffff) then (
       let x = m land 0x3f in
       let y = (m lsr 6) land 0x3f in
       let z = m lsr 12 in
       Char.chr (z lor 0xe0) :: Char.chr (y lor 0x80) :: Char.chr (x lor 0x80) :: cs)
   else
   if 0x10000<=m && m<=0x10ffff then (
       let x = m land 0x3f in
       let y = (m lsr 6) land 0x3f in
       let z = (m lsr 12) land 0x3f in
       let u = m lsr 18 in
       Char.chr (u lor 0xf0) :: Char.chr (z lor 0x80) ::
       Char.chr (y lor 0x80) :: Char.chr (x lor 0x80) :: cs)
   else
       raise (MalformedUTF_ ["int "; hex8 m; " -- no such code point"])

let utf8_implode ns =
   string_of_chars (List.fold_right utf8_of_ucode ns [])

let utf8_of_ucode m = string_of_chars (utf8_of_ucode m [])

let utf8width_from_ucode m =
  if 0x00<=m && m<=0x07f then 1 else 
  if 0x80<=m && m<=0x7ff then 2 else
  if (0x800<=m && m<=0xdfff) || (0xe0000<=m && 0xe0000<=0xffff) then 3 else
  if 0x10000<=m && m<=0x10ffff then 4 else
    raise (MalformedUTF_ ["utf8_width_from_ucode "; hex8 m; " -- no such code point"])
    
let utf8LSQUOTE = utf8_of_ucode 0x2018
let utf8RSQUOTE = utf8_of_ucode 0x2019

(**********************************************)

(* all this because char constants aren't proper ints in patterns. Hard to read or what? *)

(* 0x20 = ' '
   0x22 = '"'
 *)
let rec words =
  function
    "" -> []
  | s  -> let rec wds =
            function
              [] -> [[]]
            | [0x20] -> [[]]
            | 0x20 :: 0x20 :: cs -> wds (0x20 :: cs)
            | 0x20 :: cs -> [] :: wds cs
            | 0x22 :: cs -> let ws = qds cs in (0x22 :: List.hd ws) :: List.tl ws
            | c :: cs -> let ws = wds cs in (c :: List.hd ws) :: List.tl ws
          and qds =
            function
              [] -> [[]]
            | [0x22] -> [[0x22]]
            | 0x22 :: 0x20 :: cs -> qds (0x22 :: cs)
            | 0x22 :: cs -> [0x22] :: wds cs
            | c :: cs -> let ws = qds cs in (c :: List.hd ws) :: List.tl ws
          in
          List.map utf8_implode (wds (utf8_explode s))

let respace ws = String.concat " " ws

(**********************************************)

let charpred s =
  let v = Hashtbl.create (String.length s * 2) in
  String.iter (fun c -> Hashtbl.add v (Char.code c) true) s;
  (fun c -> try Hashtbl.find v c with Not_found -> false),
  (fun (c, b) -> Hashtbl.add v c b)

let (islcletter, _) = charpred "abcdefghijklmnopqrstuvwxyz"
let (isucletter, _) = charpred "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
let rec isletter c = islcletter c || isucletter c

(**********************************************)

let (isdigit, _) = charpred "0123456789"


