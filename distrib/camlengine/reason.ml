(* $Id$ *)

module type T =
  sig
    val setReason : string list -> unit
    val prefixtoReason : string list -> unit
    val appendtoReason : string list -> unit
    val getReason : unit -> string list
  end
(* $Id$ *)

module M : T =
  struct
    let reasons : string list ref = ref []
    let rec setReason ss = reasons := ss
    let rec getReason () = let r = !reasons in (reasons := []; r)
    (* read and clear *)
      
    let rec prefixtoReason ss = reasons := ss @ !reasons
    let rec appendtoReason ss = reasons := !reasons @ ss
  end

