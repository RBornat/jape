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

exception Malformed_

(* it appears that OCaml allows you to write ranges of characters in a match, 
   but not ranges of integers. Very odd, and a right pain.
 *)
 
let check_808f c =
  match c with
    '\x80'..'\x8f' -> Char.code c land 0x3f
  | _              -> raise Malformed_

let check_809f c =
  match c with
    '\x80'..'\x9f' -> Char.code c land 0x3f
  | _              -> raise Malformed_

let check_80bf c =
  match c with
    '\x80'..'\xbf' -> Char.code c land 0x3f
  | _              -> raise Malformed_

let check_90bf c =
  match c with
    '\x90'..'\xbf' -> Char.code c land 0x3f
  | _              -> raise Malformed_

let check_a0bf c =
  match c with
    '\xa0'..'\xbf' -> Char.code c land 0x3f
  | _              -> raise Malformed_
  
let get_utf8 s =
  let n1 = Char.code (Stream.next s) in
  try
    let twobyte f = 
      let n2 = f (Stream.next s) in
      ((n1 land 0x1f) lsl 6) lor n2 in
    let threebyte f = 
      let n2 = f (Stream.next s) in
      let n3 = check_80bf (Stream.next s) in
      ((((n1 land 0x0f) lsl 6) lor n2) lsl 6) lor n3 in
    let fourbyte f = 
      let n2 = f (Stream.next s) in
      let n3 = check_80bf (Stream.next s) in
      let n4 = check_80bf (Stream.next s) in      
      ((((((n1 land 0x07) lsl 6) lor n2) lsl 6) lor n3) lsl 6) lor n4 in
    match n1 with
      | n when 0x00<=n && n<=0x7f ->
          n1
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
          raise Malformed_
  with
    Stream.Failure -> raise Malformed_

let get16_be s = 
  let n1 = Char.code (Stream.next s) in
  try let n2 = Char.code (Stream.next s) in
      (n1 lsl 8) lor n2
  with Stream.Failure -> raise Malformed_
  
let get16_le s = 
  let n2 = Char.code (Stream.next s) in
  try let n1 = Char.code (Stream.next s) in
      (n1 lsl 8) lor n2
  with Stream.Failure -> raise Malformed_

let get_utf16 next16 s =
  match next16 s with
    m when 0x0000<=m && m<=0xd7ff -> 
      m
  | m1 when 0xd800<=m1 && m1<=0xdbff ->
      (try
         match next16 s with 
            m2 when 0xdc00<=m2 && m2<=0xdfff -> (
              match (((m1 land 0x3ff) lsl 10) lor (m2 land 0x3ff)) + 0x10000 with
                m when (0x0000<=m && m<=0xd7ff) || (0xe000<=m && m<=0x10ffff) ->
                  m
              | _ ->
                  raise Malformed_)
          | _ ->
              raise Malformed_
       with 
         Stream.Failure -> raise Malformed_)
   | _ -> raise Malformed_

let get32_be s = 
  let n1 = get16_be s in
  try let n2 = get16_be s in
      (n1 lsl 16) lor n2
  with Stream.Failure -> raise Malformed_
  
let get32_le s = 
  let n2 = get16_le s in
  try let n1 = get16_le s in
      (n1 lsl 16) lor n2
  with Stream.Failure -> raise Malformed_

let get_utf32 next32 s =
  match next32 s with
    m when (0x0000<=m && m<=0xd7ff) || (0xe000<=m && 0xe000<=0x10ffff) -> m
  | _                                                                  -> raise Malformed_

(************************************)

let utf8_of_int m =
  if 0x00<=m && m<=0x07f then
       [Char.chr m]
  else
  if 0x80<=m && m<=0x7ff then (
       let y = m lsr 6 in
       let x = m land 0x3f in
       [Char.chr (y lor 0xc0); Char.chr (x lor 0x80)])
  else
  if (0x800<=m && m<=0xdfff) || (0xe0000<=m && 0xe0000<=0xffff) then (
       let x = m land 0x3f in
       let y = (m lsr 6) land 0x3f in
       let z = m lsr 12 in
       [Char.chr (z lor 0xe0); Char.chr (y lor 0x80); Char.chr (x lor 0x80)])
   else
   if 0x10000<=m && m<=0x10ffff then (
       let x = m land 0x3f in
       let y = (m lsr 6) land 0x3f in
       let z = (m lsr 12) land 0x3f in
       let u = m lsr 18 in
       [Char.chr (u lor 0xf0); Char.chr (z lor 0x80);
        Char.chr (y lor 0x80); Char.chr (x lor 0x80)])
   else
       raise Malformed_
      
let split16 m cs = 
  Char.chr(m lsr 8) :: Char.chr(m land 0xff) :: cs
    
let utf16_of_int m =
  if (0x0000<=m && m<=0xd7ff) || (0xe000<=m && 0xe000<=0xffff) then 
       split16 m []
  else
  if 0x10000<=m && m<=0x10ffff then (
       let x = m land 0x3ff in
       let y = (m - 0x10000) lsr 10 in
       split16 (0xd800 lor y) (split16 (0xdc00 lor x) []))
   else 
       raise Malformed_

let utf32_of_int m =
  if (0x0000<=m && m<=0xd7ff) || (0xe000<=m && 0xe000<=0x10ffff) then 
    split16 (m lsr 16) (split16 (m land 0xffff) [])
  else raise Malformed_

(*******************************)

let next_utf8 = get_utf8
let next_utf16 bigendian = 
  get_utf16 (if bigendian then get16_be else get16_le)
let next_utf32 bigendian = 
  get_utf32 (if bigendian then get32_be else get32_le)

(* this is slow, but I don't think it's going to be used much *)
let utf16_of_int bigendian =
  if bigendian then utf16_of_int else (List.rev <.> utf16_of_int)
let utf32_of_int bigendian =
  if bigendian then utf32_of_int else (List.rev <.> utf32_of_int)

(*****************************)

type ustream = { mutable utf8 : char list }

let utf8stream s unext = 
  let ustr = { utf8 = [] } in
  Stream.from 
    (fun _ ->
       try Some (match ustr.utf8 with
                   [] -> let i = unext s in
                         (match utf8_of_int i with
                            [c]      -> c
                          | (c::cs') -> ustr.utf8<-cs'; c
                          | _        -> raise (Miscellaneous.Catastrophe_
                                                ["utf8_of_int "; Stringfuns.hexstring_of_int i; " = []"]))
                  | (c::cs') -> ustr.utf8<-cs'; c
                 ) with Stream.Failure -> None) 

let rec njunk n s =
  if n>0 then (Stream.junk s; njunk (n-1) s)
  
let utf8_of_utfchannel ic =
  let s = Stream.of_channel ic in
  (* look for BOM; without a BOM default is UTF8 *)
  match Stream.npeek 4 s with 
    ['\x00'; '\x00'; '\xfe'; '\xff'] -> njunk 4 s; utf8stream s (next_utf32 true)
  | ['\xff'; '\xfe'; '\x00'; '\x00'] -> njunk 4 s; utf8stream s (next_utf32 false)
  | cs ->
      match Listfuns.take 3 cs with 
        ['\xef'; '\xbb'; '\xbf'] -> njunk 3 s; s
      | _ -> 
          match Listfuns.take 2 cs with
            ['\xfe'; '\xff'] -> njunk 2 s; utf8stream s (next_utf16 true)
          | ['\xff'; '\xfe'] -> njunk 2 s; utf8stream s (next_utf16 false)
          | _                -> s

let utf8_of_utfNchannel size bigendian ic =
  let s = Stream.of_channel ic in
    match size with
      8 -> (
        match Stream.npeek 3 s with
          ['\xef'; '\xbb'; '\xbf'] -> njunk 3 s; s
        | _                        -> s)
    | 16 -> (
        let s' = utf8stream s (next_utf16 bigendian) in
        match bigendian, Stream.npeek 2 s with
          true , ['\xfe'; '\xff'] -> njunk 2 s; s'
        | false, ['\xff'; '\xfe'] -> njunk 2 s; s'
        | _                       -> s')
    | 32 -> (
        let s' = utf8stream s (next_utf32 bigendian) in
        match bigendian, Stream.npeek 4 s with
          true , ['\x00'; '\x00'; '\xfe'; '\xff'] -> njunk 4 s; s'
        | false, ['\xff'; '\xfe'; '\x00'; '\x00'] -> njunk 4 s; s'
        | _                       -> s')
    | _ -> raise (Miscellaneous.Catastrophe_ ["utf8_of_utfNchannel "; string_of_int size])

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
  
let utf8_junk s =
  match Stream.peek s with
    Some c -> 
      let n = utf8width_from_header c in
      if n>0 then njunk n s
             else raise Malformed_
  | None   -> ()
  
let utf8_peek s = 
  match Stream.peek s with
    Some c -> 
      let n = utf8width_from_header c in
      if n>0 then Some (Sml.string_of_chars (Stream.npeek n s))
             else raise Malformed_
  | None   -> None

let utf8_explode s =
  let lim = String.length s in
  let rec f i =
    if i=lim then [] else (
      let n = utf8width_from_header s.[i] in
      if n>0 then (
        let str = String.create n in
        for j = 0 to n-1 do str.[j]<-s.[i+j] done;
        str :: f (i+n))
      else raise Malformed_)
  in
  f 0 

let utf8_sub s i =
  if i = String.length s then "" else (
    let n = utf8width_from_header s.[i] in
    if n>0 then (
      let str = String.create n in
      for j = 0 to n-1 do str.[j]<-s.[i+j] done;
      str)
    else raise Malformed_)

let utf8_presub s i =
  if i=0 then "" else 
    try let rec f i =
          let n = utf8width_from_header s.[i] in
          if n>0 then (
            let str = String.create n in
            for j = 0 to n-1 do str.[j]<-s.[i+j] done;
            str)
          else f (i-1)
        in
          f (i-1)
    with _ -> raise Malformed_
