(* $Id$ *)

type proofstage = Proved | Disproved | InProgress

let rec proofstage2word =
  function
    Proved     -> "PROOF"
  | Disproved  -> "DISPROOF"
  | InProgress -> "CURRENTPROOF"
