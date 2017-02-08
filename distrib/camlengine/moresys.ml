(* 
    Copyright (C) 2003-17 Richard Bornat & Bernard Sufrin
     
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

(* because the Unix library on Windows doesn't implement kill, we still need the
   distinction between Windows and the rest. RB 22/xi/2008.
   PS I think Bernard wrote all this stuff.
 *)
 
open Sys

let _ = output_string stderr "Operating System "
let _ = output_string stderr Sys.os_type
let _ = output_char stderr '\n'

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

let rec onInterrupt f g =
if os_type="Win32" then 
  let _ = g () in ()
else 
  let now = signal sigint (Signal_handle f) in
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

