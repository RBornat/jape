(* This is an implementation of a minimum waste algorithm.  I discovered it in
 * November 1994, but I bet that scholarship would show it is well known 
 * and really old.
 * RB 15/xi/96
 *
 * Suppose you have complete information about the best way to split a list 
 * xs into n segments - that is, you have a list

	   splits = [ bestsplit n (drop i xs) | i <- [0..#xs]]
 
 * then the best way to split a list into n+1 segments must be

	   take j xs : drop j splits

 * for some j.  
 * All we need to do is to increase j until we find the 'break point'.
 *)

exception Catastrophe_ = Miscellaneous.M.Catastrophe_

let (<*) = Listfuns.M.(<*)
let bracketedliststring = Listfuns.M.bracketedliststring
let consolereport = Miscellaneous.M.consolereport
let drop = Listfuns.M.drop
let take = Listfuns.M.take

(* In this first attempt I've ignored the fact that this is an n^2 algorithm
 * or worse ...
 *)

(* measurefn measures items to give an int (why not?),and it is a STRONG ASSUMPTION
 * that measurement is simply additive.  If not, we have to change the algorithm!!
 * w is the width we _must_ fit within.
 *)
	  
let minwastedebug = ref false
let rec minw w a n =
  let rec ma (ma, mi) = ma in
  let rec mi (ma, mi) = mi in
  let rec combine k m = max (k) (ma m), min (k) (mi m) in
  let rec better m m' = ma m - mi m < ma m' - mi m' in
  (* just look at waste *)
  let rec ok m = ma m <= w in
  let rec zerosplits i =
	if i = n then raise (Catastrophe_ ["zerosplits i=n"])
	else if i + 1 = n then let x = Array.get a i in [(x, x), [1]]
	else
	  match zerosplits (i + 1) with
		((y1, y2), [k]) :: _ as zs ->
		  let x = Array.get (a) (i) in ((x + y1, x + y2), [k + 1]) :: zs
	  | _ -> raise (Catastrophe_ ["zerosplits => []"])
  in
  (* *)
  let rec showsplit ((ma, mi : int * int), (s : int list)) =
	((((("((" ^ string_of_int ma) ^ ",") ^ string_of_int mi) ^ "),") ^
	   bracketedliststring string_of_int "," s) ^
	  ")"
  in
  (* *)

  (* split takes xs = gs@hs and a measure of gs, together with the 
   * list ss of best ways of splitting hs into n bits; it's the case 
   * that #hs=#ss (if I get it right), but ML won't believe that ...
   *)

  (* nowadays gs is represented by array a[i..j-1] and hs by a[j..n-1] *)
  
  let rec split i j mgs ss =
	let rec foundone first =
	  first :: split (i + 1) j (mgs - Array.get (a) (i)) ss
	in
	(* *)
	let _ =
	  if !minwastedebug then
		consolereport
		  ["split ["; string_of_int (j - i); "] "; string_of_int mgs; " [";
		   string_of_int (n - j); "] ";
		   bracketedliststring showsplit "," (take 3 ss);
		   if List.length ss > 3 then ", ..." else ""]
	in
	(* *)
	match i = j, j = n, ss with
	  true, _, [m, s] -> [combine mgs m, 0 :: s]
	| _, _, [m, s] -> foundone (combine mgs m, j - i :: s)
	| _, false, (m1, s1) :: ((m2, s2) :: _ as ss') ->
		let h1 = Array.get (a) (j) in
		let m1' = combine mgs m1 in
		let mgs' = mgs + h1 in
		let m2' = combine mgs' m2 in
		if mgs' = ma m2' && not (better m2' m1') then
		  (* we have gone past the best point, I believe *)
		  let first = m1', j - i :: s1 in
		  if i = j then [first] else foundone first
		else split i (j + 1) mgs' ss'
	| _ ->
		raise
		  (Catastrophe_
			 ["forgot something or other in minwaste: lengths are ";
			  string_of_int (j - i); " _ "; string_of_int (n - j); " ";
			  string_of_int (List.length ss)])
  in
  let rec choose ss =
	let ss' = split 0 0 0 ss in
	let (prevm, prevs) = List.hd ss in
	let (thism, thiss) = List.hd ss' in
	(* *)
	let _ =
	  if !minwastedebug then
		consolereport
		  ["choose "; bracketedliststring showsplit "," ss; " ";
		   bracketedliststring showsplit "," ss']
	in
	(* *)
	if List.exists
		 (function
			0 -> true
		  | _ -> false)
		 thiss
	then
	  prevs
	else if ok thism then thiss
	else if ma thism = ma prevm then prevs
	else choose ss'
  in
  if n <= 1 then [n] else choose (zerosplits 0)

let rec minwforcache (w, ns) = minw w (Array.of_list ns) (List.length ns)

module Hashtab = Cache.F (struct type dom = int * int list 
                                 type ran = int list
                                 let eval = minwforcache 
                                 let size = 127 
                           end)

let minw = Hashtab.lookup
let resetminwcache = Hashtab.reset

let rec minwaste measurefn w xs =
  let rec recon a1 a2 =
	match a1, a2 with
	  n :: ns, xs ->
		if n <= List.length xs then take n xs :: recon ns (drop n xs)
		else
		  raise
			(Catastrophe_
			   ["minwaste 2: ";
				bracketedliststring string_of_int "," (n :: ns); "; ";
				string_of_int (List.length xs)])
	| [], [] -> []
	| [], _ -> raise (Catastrophe_ ["minwaste"])
  in
  let wns = w, measurefn <* xs in recon (minw wns) xs
