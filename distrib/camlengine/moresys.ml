open Sys

exception Interrupt (* in case you need it *)

let rec onInterrupt f g =
  (* does this catch two ctrl-Cs? or do I need to do the set_signal again, once it's caught? RB *)
  let now = signal sigint (Signal_handle (fun i -> f ())) in
  try g (); set_signal sigint now
  with exn -> set_signal sigint now; raise exn
    
let execute_in_env cmd args env =
   let (in_read, in_write) = Unix.pipe() in
   let (out_read, out_write) = Unix.pipe() in
   let inchan = Unix.in_channel_of_descr in_read in
   let outchan = Unix.out_channel_of_descr out_write in
   let pid = Unix.create_process_env cmd (Array.of_list args) (Array.of_list env)
			   out_read in_write Unix.stderr 
   in
   Unix.close out_read;
   Unix.close in_write;
   (pid, inchan, outchan)

let execute cmd args =
  execute_in_env cmd args (Array.to_list (Unix.environment()))
  
(* let create_process cmd args new_stdin new_stdout new_stderr =

let create_process_env cmd args env new_stdin new_stdout new_stderr = *)
