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

open Sys

let _ = Printf.eprintf "Operating System %s\n" os_type

exception Interrupt (* in case you need it *)

type process_id = 
|       Unixoid         of int 
|       Windowsoid      of (in_channel*out_channel)


let ignorePipeSignals() =
if os_type="Win32" then 
   (* I daresay we'll have to think of something *)
   ()
else
   Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let normalizePath filename = 
if os_type="Win32" 
then 
   let sloshify = function '/'->'\\' | c -> c
   in Sml.string_of_chars(List.map sloshify (Sml.chars_of_string filename))
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
    Sml.implode (List.rev (removestern (List.rev (Sml.explode path))))
 
let open_input_file  filename = open_in  (normalizePath filename)

let open_output_file filename = open_out (normalizePath filename)


let rec onInterrupt f g =
if os_type="Win32" then 
  let _ = g () in ()
else 
  let now = signal (sigint) (Signal_handle f) in
  try g (); set_signal sigint now
  with exn -> set_signal sigint now; raise exn
    
let execute_in_env cmd args env =

if os_type<>"Win32" then 
   let (in_read, in_write) = Unix.pipe() in
   let (out_read, out_write) = Unix.pipe() in
   let inchan = Unix.in_channel_of_descr in_read in
   let outchan = Unix.out_channel_of_descr out_write in
   let pid = Unix.create_process_env cmd (Array.of_list args) (Array.of_list env)
			   out_read in_write Unix.stderr 
   in
   Unix.close out_read;
   Unix.close in_write;
   (Unixoid pid, inchan, outchan)
else
   let (inchan, outchan) = 
       Unix.open_process cmd 
   in (Windowsoid(inchan, outchan), inchan, outchan)

let execute cmd args =
  execute_in_env cmd args (Array.to_list (Unix.environment()))
  
(* let create_process cmd args new_stdin new_stdout new_stderr =

let create_process_env cmd args env new_stdin new_stdout new_stderr = *)

let reap = function
| Unixoid    pid   -> (try Unix.kill pid Sys.sigkill with _ -> ())
| Windowsoid chans -> (try let _ = Unix.close_process chans in () with _ -> ())

