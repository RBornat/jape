(* $Id$ *)

module type Proofstage =
  sig
    type proofstage = Proved | Disproved | InProgress
    val proofstage2word : proofstage -> string
  end 
(* $Id$ *)

module Proofstage : Proofstage =
  struct
    type proofstage = Proved | Disproved | InProgress
    let rec proofstage2word =
      function
        Proved -> "PROOF"
      | Disproved -> "DISPROOF"
      | InProgress -> "CURRENTPROOF"
  end
