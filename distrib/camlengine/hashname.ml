(* $Id$ *)

module
  hashname
  (AAA : sig module name : Name module hashstring : Hashtype end)
  :
  hashtype =
  struct
    open AAA
    open name
    type type__ = name
    let eq : type__ * type__ -> bool = fun (x, y) -> x = y
    let copy x : type__ = x
    let rec hash (s, n) = hashstring.hash (namestring s, n)
  end
