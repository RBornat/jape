(* $Id$ *)

module M : Hashtype.T with type type__ = Name.M.name =
  struct
    open Name.M
    type type__ = Name.M.name
    let eq : type__ * type__ -> bool = fun (x, y) -> x = y
    let copy x : type__ = x
    let rec hash (s, n) = Hashstring.M.hash (namestring s, n)
  end
