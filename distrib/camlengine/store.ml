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

(* $Id$ *)

module F
  (AAA : sig module Dom : Hashtype.T type ran end)
  : T with type ran = AAA.ran
       and type dom = AAA.Dom.type__ 
  =
  struct
    open AAA
    open Dom

    let get = Array.get
    let set = Array.set
    let make = Array.make
    
    module Mapping =
      struct
        type dom = type__ and ran = AAA.ran ref
        type mapping = (dom * ran) list
        let empty : mapping = []
        let rec ( |-> ) (d, r) = [d, r]
        let rec ( ++ ) (m1, m2) = m2 @ m1
        let rec ( -- ) =
          function
            [], x -> []
          | (x', y) :: m, x ->
              if eq (x, x') then m else (x', y) :: ( -- ) (m, x)
        let rec at =
          function
            [], x -> None
          | (x', y) :: m, x -> if eq (x, x') then Some y else at (m, x)
        let rec lfold a1 a2 a3 =
          match a1, a2, a3 with
            f, r, [] -> r
          | f, r, p :: m -> lfold f (f (r, p)) m
      end
    open Mapping
    
    (* type 'a array = 'a Array.array *)
    
    type dom = type__ and ran = AAA.ran
    type store = Mapping.mapping array * int
    
    let rec new__ n = make n Mapping.empty, n
    let rec update ((a, n), x, y) =
      let h = hash (x, n) in
      let b = get a h in
      match at (b, x) with
        Some yr -> yr := y
      | None -> set a h (( ++ ) (b, ( |-> ) (copy x, ref y)))
    let rec delete ((a, n), x) =
      let h = hash (x, n) in
      let b = get a h in set a h (( -- ) (b, x))
    let rec at ((a, n), x) =
      match Mapping.at (get a (hash (x, n)), x) with
        Some {contents = y} -> Some y
      | None -> None
    let rec fold f r (a, n) =
      Extend.M.Arrayf.lfold
        (fun (r, bkt) ->
           Mapping.lfold (fun (r, (x, {contents = y})) -> f ((x, y), r)) r
             bkt)
        r a
    let rec statistics (a, n) =
      (* another fix for that stupid 109.19 *)
      let rec size xs = Mapping.lfold (fun (l, _) -> l + 1) 0 xs in
      Extend.M.Arrayf.rfold (fun (bkt, r) -> size bkt :: r) [] a
  end

