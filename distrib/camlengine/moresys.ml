exception Interrupt (* in case you need it *)

let rec onInterrupt f g =
  (* does this catch two ctrl-Cs? or do I need to do the set_signal again, once it's caught? RB *)
  let now = Sys.signal Sys.sigint (Sys.Signal_handle (fun i -> f ())) in
  try g (); Sys.set_signal Sys.sigint now
  with exn -> Sys.set_signal Sys.sigint now; raise exn
    
