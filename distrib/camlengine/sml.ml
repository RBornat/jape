(* useful functions for caml *)

let (<*>) f g x = f (g x)

let rec nj_fold f xs z = 
  match xs with [] -> z | x::xs -> f (x, nj_fold f xs z)
let rec nj_revfold f xs z = 
  match xs with [] -> z | x::xs -> nj_revfold f xs (f (x,z))

let chars_of_string s =
  let len = String.length s in
  let rec e n = if n=len then [] else String.get s n :: e (n+1) in
  e 0
  
let explode = List.map (String.make 1) <*> chars_of_string

let implode = String.concat ""

(* let string_of_chars = implode <*> String.make 1 *)
let string_of_chars cs = 
  let len = List.length cs in
  let s = String.create len in
  let rec ii n cs = 
	match cs with (c::cs) -> (s.[n]<-c; ii (n+1) cs)
	|             []      -> ()
  in
  (ii 0 cs; s) 
  
let fst_of_3 (a,b,c) = a
let snd_of_3 (a,b,c) = b
let thrd (a,b,c) = c

let fst_of_6 (a,b,c,d,e,f) = a
let fst_of_7 (a,b,c,d,e,f,g) = a

let null xs = xs=[]

exception OrdOf_ of string * int
let ordof s i = (try Char.code (String.get s i) with _ -> raise (OrdOf_ (s,i)))
let ord s = ordof s 0

let rec revapp f xs =
  match xs with
	[]    -> ()
  | x::xs -> revapp f xs; f x

let fSome v = Some v
