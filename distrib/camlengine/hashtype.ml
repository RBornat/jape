(* $Id$ *)

module type T =
  sig
    type type__
    val eq : type__ * type__ -> bool
    val copy : type__ -> type__
    val hash : type__ * int -> int
  end
