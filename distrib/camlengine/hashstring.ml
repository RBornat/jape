(* $Id$ *)

module M : Hashtype.T =
  struct
    type type__ = string
    let eq : type__ * type__ -> bool = fun (x, y) -> x = y
    let copy x : type__ = x
    let rec hash (s, n) =
      match String.length s with
        0 -> 0
      | 1 -> Char.code (String.get s 0) mod n
      | 2 -> (Char.code (String.get s 0) * 256 + Char.code (String.get s 1)) mod n
      | 3 -> (Char.code (String.get s 0) * 512 + Char.code (String.get s 1) * 256 + Char.code (String.get s 2)) mod n
      | l ->
          (Char.code (String.get s 0) * 1024 + Char.code (String.get s (l / 2)) * 512 +
             Char.code (String.get s (l / 2 + 1)) * 256 + Char.code (String.get s (l - 1))) mod n
  end
