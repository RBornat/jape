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

let (env : (string * string) list ref) = ref []
let rec reset () = env := []
let rec split =
  function "" -> "", ""
  | s -> 
    let eq = Char.code '=' in
    let p = ref 0 in
    while !p <> String.length s - 1 && Char.code (String.get s !p) <> eq do incr p done;
    match !p with
      0 -> s, s
    | n -> String.sub s 0 n, String.sub s (n+1) (String.length s - n - 1)
let rec declenv s = env:=List.map split s; !env
let rec fetchenv () =
  match !env with
    [] -> declenv (Array.to_list (Unix.environment ()))
  | env -> env
let rec currentenv () = List.map (fun (n, v) -> (n ^ "=") ^ v) (fetchenv ())
let rec setenv nv = env := nv :: fetchenv ()
let rec getenv d s =
  let rec _F =
    function
      [] -> d
    | (n, v) :: nvs -> if n = s then v else _F nvs
  in
  _F (fetchenv ())
let rec nodups l =
  nj_fold
    (fun (x, xs) -> if List.exists (fun x' -> x = x') xs then xs else x :: xs)
    l []
let rec variables () = nodups (List.map fst (fetchenv ()))

