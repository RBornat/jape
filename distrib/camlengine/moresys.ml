open Sys

exception Interrupt (* in case you need it *)

let rec onInterrupt f g =
  (* does this catch two ctrl-Cs? or do I need to do the set_signal again, once it's caught? RB *)
  let now = signal sigint (Signal_handle (fun i -> f ())) in
  try g (); set_signal sigint now
  with exn -> set_signal sigint now; raise exn
    
exception Execute_ of string

let execute cmd args =
  try 
	prerr_string "execute 1\n";
	let (in_read, in_write) = Unix.pipe() in
	prerr_string "execute 2\n";
	let (out_read, out_write) = Unix.pipe() in
	prerr_string "execute 3\n";
	let inchan = Unix.in_channel_of_descr in_read in
	prerr_string "execute 4\n";
	let outchan = Unix.out_channel_of_descr out_write in
	prerr_string "execute 5\n";
	let was = signal sigpipe Signal_ignore in
	set_signal sigpipe
		(Signal_handle (fun _ -> prerr_string "caught sigpipe signal\n"; 
		                         Pervasives.flush stderr; 
		                         raise (Execute_ "broken pipe"))); 
	prerr_string "execute 5a\n";
	flush stderr;
	let pid = (try Unix.create_process cmd (Array.of_list args) out_read in_write Unix.stderr 
	           with exn -> prerr_string "execute oh dear\n"; raise Interrupt) in
	prerr_string "execute 6\n";
	prerr_string "japeserver name "; prerr_string cmd; prerr_string "; "; 
	prerr_int pid; prerr_string "\n";
	Unix.close out_read;
	Unix.close in_write;
	(* set_signal sigpipe was; *)
	(inchan, outchan)
  with Sys_error "Broken pipe" -> prerr_string "caught it\n"; raise (Execute_ "broken pipe")

(* let create_process cmd args new_stdin new_stdout new_stderr =

let create_process_env cmd args env new_stdin new_stdout new_stderr = *)
