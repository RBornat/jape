(* $Id$ *)

module type T = 
sig    
    val char_explode : string -> char list
    val char_implode : char list -> string
    
    val (<*>) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c (* compose -- can't do without it *)

    val explode : string -> string list
    val implode : string list -> string
    val nj_fold : ('b * 'a -> 'a) -> 'b list -> 'a -> 'a
    val nj_revfold : ('b * 'a -> 'a) -> 'b list -> 'a -> 'a
    val null : 'a list -> bool
    val ord : string -> int
    val revapp : ('a -> unit) -> 'a list -> unit
    val snd_of_3 : ('a * 'b * 'c) -> 'b
    val thrd : ('a * 'b * 'c) -> 'c
    
    val fSome : 'a ->'a option
end

module M : T =
  struct
    (* useful functions for caml *)
    let (<*>) f g x = f (g x)
    
    let rec nj_fold f xs z = match xs with [] -> z | x::xs -> nj_fold f xs (f (x,z))
    let rec nj_revfold f xs z = match xs with [] -> z | x::xs -> f (x, nj_revfold f xs z)
    
    let char_explode s =
      let len = String.length s in
      let rec e n = if n=len then [] else String.get s n :: e (n+1) in
      e 0
      
    let explode = List.map (String.make 1) <*> char_explode
    
    let implode = String.concat ""
    
    (* let char_implode = implode <*> String.make 1 *)
    let char_implode cs = 
      let len = List.length cs in
      let s = String.create len in
      let rec ii n cs = 
        match cs with (c::cs) -> (s.[n]<-c; ii (n+1) cs)
        |             []      -> ()
      in
      (ii 0 cs; s) 
      
    let snd_of_3 (a,b,c) = b
    let thrd (a,b,c) = c
    
    let null xs = xs=[]
    
    let ord s = Char.code (String.get s 0)
    
    let rec revapp f xs =
      match xs with
        []    -> ()
      | x::xs -> revapp f xs; f x
    
    let fSome v = Some v
end    
