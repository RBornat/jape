(* $Id$ *)

module M
  (AAA : sig module Name : Name.T 
             module Hashstring : Hashtype.T 
                    with type type__ = string
         end)
  : Hashtype.T =
  struct
    open AAA
    open Name
    type type__ = name
    let eq : type__ * type__ -> bool = fun (x, y) -> x = y
    let copy x : type__ = x
    let rec hash (s, n) = Hashstring.hash (namestring s, n)
  end
