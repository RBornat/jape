(* $Id$ *)

let reasons : string list ref = ref []

let rec getReason () = let r = !reasons in (reasons := []; r)
let rec setReason ss = reasons := ss
  
let rec prefixtoReason ss = reasons := ss @ !reasons
let rec appendtoReason ss = reasons := !reasons @ ss

