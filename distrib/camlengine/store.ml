(* $Id$ *)

module type T =
  sig
    (* infixr 9 at; *)

    type dom and ran and store
    val new__ : int -> store
    val update : store * dom * ran -> unit
    val delete : store * dom -> unit
    val at : store * dom -> ran option
    val fold : ((dom * ran) * 'a -> 'a) -> 'a -> store -> 'a
    val statistics : store -> int list
  end

