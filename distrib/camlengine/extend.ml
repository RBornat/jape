(* $Id$ *)

module type T =
  sig
    module Arrayf :
      sig
        val iter : ('a -> 'b) -> 'a array -> unit
        val lfold : ('a * 'b -> 'a) -> 'a -> 'b array -> 'a
        val rfold : ('b * 'a -> 'a) -> 'a -> 'b array -> 'a
        val arrayof : int * (int -> 'a) -> 'a array
      end
    module Integer :
      sig
        val iter : (int -> 'a) -> int * int -> unit
        val lfold : ('b * int -> 'b) -> 'b -> int * int -> 'b
        val rfold : (int * 'b -> 'b) -> 'b -> int * int -> 'b
      end
    module Ref :
      sig
        (* infix +:= -:= ::=; *)
        val ( +:= ) : int ref * int -> unit
        val ( -:= ) : int ref * int -> unit
        val ( |::= ) : 'a * 'a list ref -> unit
        val ( ++ ) : int ref -> int
        val ( -- ) : int ref -> int
      end
  end


(* $Id$ *)

module M : T =
  struct
    module Integer =
      struct
        let rec lfold f r (m, n) =
          if m > n then r else lfold f (f (r, m)) (m + 1, n)
        let rec rfold f r (m, n) =
          if m > n then r else f (m, rfold f r (m + 1, n))
        let rec iter f = lfold (fun ((), n) -> f n; ()) ()
      end
    module Arrayf =
      struct
        open Array
        let rec lfold f r a =
          Integer.lfold (fun (r, i) -> f (r, Array.get a i)) r
            (0, Array.length a - 1)
        let rec rfold f r a =
          Integer.rfold (fun (i, r) -> f (Array.get a i, r)) r
            (0, Array.length a - 1)
        let rec iter f a =
          Integer.iter (fun i -> f (Array.get a i)) (0, Array.length a - 1)
        let rec arrayof (n, f) =
          let a = make n (f 0) in
          Integer.iter (fun i -> set a i (f i)) (1, n - 1); a
      end
    module Ref =
      struct
        
        let rec ( +:= ) (({contents = n} as r), (i : int)) = r := n + i
        let rec ( -:= ) (({contents = n} as r), (i : int)) = r := n - i
        let rec ( |::= ) (i, ({contents = n} as r)) = r := i :: n
        let rec ( ++ ) ({contents = n} as r) = r:=n+1; n
        let rec ( -- ) ({contents = n} as r) = r:=n-1; n
      end
  end

