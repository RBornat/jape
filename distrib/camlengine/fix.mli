val fix : ('a -> 'a) -> 'a
val memofix : ('a, 'b) Hashtbl.t -> (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
