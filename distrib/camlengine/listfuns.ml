(* $Id$ *)

module type Listfuns =
  sig
    (* nonfix All Some;
       infixr 7 <|;
       infixr 6 MAP;
       infix  5 </ //;
       infixr 5 doubleslosh />;
       infix  4 ||| slosh INTER;
       infixr 4 UNION;
       infix  0 nonmember member subset;
     *)
 
    val ( <| ) : ('a -> bool) * 'a list -> 'a list
    val MAP : ('a -> 'b) * 'a list -> 'b list
    val doubleslosh : ('a -> 'b) * ('a -> bool) -> 'a list -> 'b list
    val ( </ ) : ('a * 'b -> 'b) * 'b -> 'a list -> 'b
    val ( /> ) : 'a * ('a * 'b -> 'a) -> 'b list -> 'a
    val ( // ) : ('a * 'a -> 'a) * 'a list -> 'a
    exception Reduce
    val ( ||| ) : 'a list * 'b list -> ('a * 'b) list
    exception Zip
    val first : ('a -> bool) -> 'a list -> 'a
    exception First
    val FIRST : ('a -> bool) -> 'a list -> 'a option
    val Some : ('a -> bool) -> 'a list -> bool
    val All : ('a -> bool) -> 'a list -> bool
    val member : 'a * 'a list -> bool
    val subset : 'a list * 'a list -> bool
    val nonmember : 'a * 'a list -> bool
    val slosh : 'a list * 'a list -> 'a list
    val UNION : 'a list * 'a list -> 'a list
    val INTER : 'a list * 'a list -> 'a list
    val set : 'a list -> 'a list
    val seteq : ('a * 'a -> bool) -> 'a list -> 'a list
    val last : 'a list -> 'a
    exception Last_
    val take : int -> 'a list -> 'a list
    val drop : int -> 'a list -> 'a list
    val zip : 'a list * 'b list -> ('a * 'b) list
    (* Bird-Meertens zip, uneven lists allowed *)
    val takewhile : ('a -> bool) -> 'a list -> 'a list
    val dropwhile : ('a -> bool) -> 'a list -> 'a list
    (* get ready for change to proper fold semantics *)
    val foldr : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
    val foldl : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b
    val NJfold : ('a * 'b -> 'b) -> 'a list -> 'b -> 'b
    val isprefix : ('a * 'a -> bool) -> 'a list -> 'a list -> bool
    val extract : ('a -> bool) -> 'a list -> 'a * 'a list
    exception Extract_
    val replacenth : 'a list -> int -> 'a -> 'a list
    val interpolate : 'a -> 'a list -> 'a list
    val catelim_interpolate :
      ('a -> 'b list -> 'b list) -> 'b -> 'a list -> 'b list -> 'b list
    val flatten : 'a list list -> 'a list
    val split : ('a -> bool) -> 'a list -> 'a list * 'a list
    (* yess, nos *)
    val sort : ('a * 'a -> bool) -> 'a list -> 'a list
    (* given op<, sorts in < order *)
    val sortandcombine :
      ('a * 'a -> bool) -> ('a * 'a -> 'a) -> 'a list -> 'a list
    val remdups : 'a list -> 'a list
    val earlierlist : ('a * 'a -> bool) -> 'a list * 'a list -> bool
    val sortunique : ('a * 'a -> bool) -> 'a list -> 'a list
    val sorteddiff : ('a * 'a -> bool) -> 'a list -> 'a list -> 'a list
    val sortedsame : ('a * 'a -> bool) -> 'a list -> 'a list -> 'a list
    val sortedmergeandcombine :
      ('a * 'a -> bool) -> ('a * 'a -> 'a) -> 'a list * 'a list -> 'a list
    val sortedmerge : ('a * 'a -> bool) -> 'a list * 'a list -> 'a list
    val sortedlistsub : ('a * 'b -> bool) -> 'a list -> 'b list -> 'a list
    val matchbag : ('a -> 'b option) -> 'a list -> ('a * 'b * 'a list) list
    val ( >< ) : 'a list * 'b list -> ('a * 'b) list
    val allpairs : 'a list -> ('a * 'a) list
    val listsub : ('a * 'b -> bool) -> 'a list -> 'b list -> 'a list
    val eqlists : ('a * 'a -> bool) -> 'a list * 'a list -> bool
    val eqbags : ('a * 'a -> bool) -> 'a list * 'a list -> bool
    val numbered : 'a list -> (int * 'a) list
    val toposort : 'a list -> ('a -> 'a list) -> 'a list * 'a list list
    (* roots -> dependencies
     * -> (topological sort (roots first), list of cycles) 
     *)

    val minwaste : ('a -> int) -> int -> 'a list -> 'a list list
    val minwastedebug : bool ref
    val resetminwcache : unit -> unit
    val liststring : ('a -> string) -> string -> 'a list -> string
    val liststring2 : ('a -> string) -> string -> string -> 'a list -> string
    val bracketedliststring : ('a -> string) -> string -> 'a list -> string
    val catelim_liststring :
      ('a -> string list -> string list) -> string -> 'a list ->
        string list -> string list
    val catelim_liststring2 :
      ('a -> string list -> string list) -> string -> string -> 'a list ->
        string list -> string list
    val catelim_bracketedliststring :
      ('a -> string list -> string list) -> string -> 'a list ->
        string list -> string list
    val catelim2stringfn : ('a -> string list -> string list) -> 'a -> string
    val stringfn2catelim : ('a -> string) -> 'a -> string list -> string list
  end
(* $Id$ *)

module
  Listfuns
  (AAA :
    sig
      val consolereport : string list -> unit
      val hashlist : ('a -> int) -> 'a list -> int
      val cache :
        string -> ('a * int * 'b -> string) -> int -> ('a -> 'b) ->
          (int -> 'a -> 'b) * (unit -> unit)
      (* function         eval                   reset        *)
      exception Catastrophe_ of string list
    end)
  :
  Listfuns =
  struct
    open AAA
    
    
    
    
    
    
    
    
    
    (* ocaml can't deal with local fixity *)
      
    exception First exception Zip exception Reduce
    (* Bird-Meertens folding *)
    let rec foldr a1 a2 a3 =
      match a1, a2, a3 with
        f, z, [] -> z
      | f, z, x :: xs -> f x (foldr f z xs)
    let rec foldl a1 a2 a3 =
      match a1, a2, a3 with
        f, z, [] -> z
      | f, z, x :: xs -> foldl f (f z x) xs
    (* old-style folding *)
    let NJfold = fold
    (* <| is infix filter *)
    let rec ( <| ) =
      function
        P, [] -> []
      | P, x :: xs -> if P x then x :: ( <| ) (P, xs) else ( <| ) (P, xs)
    (* MAP is infix map *)
    let rec MAP =
      function
        f, [] -> []
      | f, x :: xs -> f x :: MAP (f, xs)
    (* ||| is infix zip *)
    let rec ( ||| ) =
      function
        [], [] -> []
      | x :: xs, y :: ys -> (x, y) :: ( ||| ) (xs, ys)
      | _, _ -> raise Zip
    (* this appears to be map f o filter P *)
    let rec doubleslosh =
      fun (f, P) ->
        let rec F =
          function
            [] -> []
          | x :: xs -> if P x then f x :: F xs else F xs
        in
        F
    (* /> looks like foldl *)
    let rec ( /> ) ((e : 'a), (( ++ ) : 'a * 'b -> 'a)) : 'b list -> 'a =
      (* infix ++; *)
      let rec F a1 a2 =
        match a1, a2 with
          r, [] -> r
        | r, x :: xs -> F (( ++ ) (r, x), xs)
      in
      F e
    (* </ looks like foldr *)
    let rec ( </ ) (( ++ ), e) =
      (* infix ++; *) let rec F =
        function
          [] -> e
        | x :: xs -> ( ++ ) (x, F xs)
      in
      F
    (* wassis? *)
    let rec ( // ) =
      function
        ( ++ ), x :: xs -> ( /> ) (x, (fun (x, y) -> ( ++ ) x y)) xs
      | ( ++ ), [] -> raise Reduce
    let rec first a1 a2 =
      match a1, a2 with
        P, [] -> raise First
      | P, x :: xs -> if P x then x else first P xs
    let rec FIRST a1 a2 =
      match a1, a2 with
        P, [] -> None
      | P, x :: xs -> if P x then Some x else FIRST (P, xs)
    let rec Some =
      fun P xs ->
        match FIRST (P, xs) with
          Some _ -> true
        | None -> false
    let rec All =
      fun P xs ->
        match FIRST ((fun x -> not (P x)), xs) with
          Some _ -> false
        | None -> true
    let rec member = fun (x, S) -> List.exists (fun x' -> x = x') S
    let rec nonmember = fun (x, S) -> not (member (x, S))
    let rec INTER = fun (S, T) -> ( <| ) ((fun x -> member (x, S)), T)
    let rec slosh = fun (S, T) -> ( <| ) ((fun x -> nonmember (x, T)), S)
    let rec UNION = fun (S, T) -> S @ T
    (*
    val set = [] /> (fn (s, e) => if e member s then s else e :: s)
    *)
    
    (* now we get this out in the same order as we gave it ... *)
    let rec seteq eq xs =
      revfold
        (fun (x, ys) ->
           if List.exists (fun x' -> eq (x, x')) ys then ys else x :: ys)
        xs []
    let rec set (xs : 'a list) =
      seteq (fun (x, y) -> x = y : 'a * 'a -> bool) xs
    let rec subset (xs, ys) = All ((fun x -> member (x, ys)), xs)
    let rec interpolate a1 a2 =
      match a1, a2 with
        sep, [] -> []
      | sep, [s] -> [s]
      | sep, s1 :: ss -> s1 :: sep :: interpolate sep ss
    let rec catelim_interpolate a1 a2 a3 a4 =
      match a1, a2, a3, a4 with
        f, sep, [], ys -> ys
      | f, sep, [x], ys -> f x ys
      | f, sep, x :: xs, ys -> f x (sep :: catelim_interpolate f sep xs ys)
    let rec catelim2stringfn f x = implode (f x [])
    let rec stringfn2catelim f x ss = f x :: ss
    let rec catelim_liststring obstring punct =
      catelim_interpolate obstring punct
    let rec catelim_liststring2 obstring sepn sep2 xs tail =
      match xs with
        [] -> tail
      | [x] -> obstring x tail
      | [x1; x2] -> obstring x1 (sep2 :: obstring x2 tail)
      | x :: xs ->
          obstring x (sepn :: catelim_liststring2 obstring sepn sep2 xs tail)
    let rec catelim_bracketedliststring obstring punct xs tail =
      "[" :: catelim_liststring obstring punct xs ("]" :: tail)
    let rec liststring obstring punct =
      catelim2stringfn (catelim_liststring (stringfn2catelim obstring) punct)
    let rec liststring2 obstring sepn sep2 =
      catelim2stringfn
        (catelim_liststring2 (stringfn2catelim obstring) sepn sep2)
    let rec bracketedliststring obstring punct =
      catelim2stringfn
        (catelim_bracketedliststring (stringfn2catelim obstring) punct)
    let rec replacenth a1 a2 a3 =
      match a1, a2, a3 with
        x :: xs, 0, y -> y :: xs
      | x :: xs, n, y -> x :: replacenth xs (n - 1) y
      | [], _, _ -> []
    exception Last_
    let rec last =
      function
        [x] -> x
      | _ :: xs -> last xs
      | [] -> raise Last_
    (* these things revised to Bird-Meertens standards - no Nth here! *)
    let rec take a1 a2 =
      match a1, a2 with
        0, xs -> []
      | n, [] -> []
      | n, x :: xs -> x :: take (n - 1) xs
    let rec drop a1 a2 =
      match a1, a2 with
        0, xs -> xs
      | n, [] -> []
      | n, x :: xs -> drop (n - 1) xs
    let rec takewhile a1 a2 =
      match a1, a2 with
        f, x :: xs -> if f x then x :: takewhile f xs else []
      | f, [] -> []
    let rec dropwhile a1 a2 =
      match a1, a2 with
        f, x :: xs -> if f x then dropwhile f xs else x :: xs
      | f, [] -> []
    let rec zip =
      function
        [], _ -> []
      | _, [] -> []
      | x :: xs, y :: ys -> (x, y) :: zip (xs, ys)
    let rec isprefix a1 a2 a3 =
      match a1, a2, a3 with
        eq, [], ys -> true
      | eq, x :: xs, y :: ys -> eq (x, y) && isprefix eq xs ys
      | eq, _, _ -> false
    exception Extract_
    let rec extract a1 a2 =
      match a1, a2 with
        f, [] -> raise Extract_
      | f, x :: xs ->
          if f x then x, xs else let (y, ys) = extract f xs in y, x :: ys
    let rec flatten xss = fold (fun (x, y) -> x @ y) xss []
    let rec split a1 a2 =
      match a1, a2 with
        f, [] -> [], []
      | f, x :: xs ->
          let (yess, nos) = split f xs in
          if f x then x :: yess, nos else yess, x :: nos
    (* smooth applicative merge sort
     * Taken from "ML for the Working Programmer", Paulson, pp 99-100
       - filched by us from SMLNJ profile.script
       - and changed, because that function sorted in > order.
     *)
    exception Matchinmergepairs
    (* spurious, shut up compiler *)
    let rec sort ( < ) ls =
      let rec merge =
        function
          [], ys -> ys
        | xs, [] -> xs
        | x :: xs, y :: ys ->
            if x < y then x :: merge (xs, y :: ys)
            else y :: merge (x :: xs, ys)
      in
      let rec mergepairs =
        function
          ([l] as ls), k -> ls
        | l1 :: l2 :: ls, k ->
            if k mod 2 = 1 then l1 :: l2 :: ls
            else mergepairs (merge (l1, l2) :: ls, k / 2)
        | _ -> raise Matchinmergepairs
      in
      let rec nextrun =
        function
          run, [] -> run, []
        | run, x :: xs ->
            if x < List.hd run then nextrun (x :: run, xs) else run, x :: xs
      in
      let rec samsorting =
        function
          [], ls, k -> List.hd (mergepairs (ls, 0))
        | x :: xs, ls, k ->
            let (run, tail) = nextrun ([x], xs) in
            samsorting (tail, mergepairs (run :: ls, k + 1), k + 1)
      in
      match ls with
        [] -> []
      | _ -> samsorting (ls, [], 0)
    let rec sortandcombine ( < ) ( ++ ) ls =
      (* infix ++ *)
      let rec merge =
        function
          [], ys -> ys
        | xs, [] -> xs
        | x :: xs, y :: ys ->
            if x < y then x :: merge (xs, y :: ys)
            else if y < x then y :: merge (x :: xs, ys)
            else ( ++ ) (x, y) :: merge (xs, ys)
      in
      let rec mergepairs =
        function
          ([l] as ls), k -> ls
        | l1 :: l2 :: ls, k ->
            if k mod 2 = 1 then l1 :: l2 :: ls
            else mergepairs (merge (l1, l2) :: ls, k / 2)
        | _ -> raise Matchinmergepairs
      in
      let rec nextrun =
        function
          run, [] -> run, []
        | run, x :: xs ->
            if x < List.hd run then nextrun (x :: run, xs) else run, x :: xs
      in
      let rec samsorting =
        function
          [], ls, k -> List.hd (mergepairs (ls, 0))
        | x :: xs, ls, k ->
            let (run, tail) = nextrun ([x], xs) in
            samsorting (tail, mergepairs (run :: ls, k + 1), k + 1)
      in
      match ls with
        [] -> []
      | _ -> samsorting (ls, [], 0)
    (* remdups removes consecutive duplicates *)
    let rec remdups =
      function
        [] -> []
      | [x] -> [x]
      | x1 :: x2 :: xs ->
          let rest = remdups (x2 :: xs) in
          if x1 = x2 then rest else x1 :: rest
    let rec sortunique ( < ) ooo = remdups (sort (fun (x, y) -> x < y) ooo)
    let rec earlierlist a1 a2 =
      match a1, a2 with
        ( < ), (x :: xs, y :: ys) ->
          x < y || not (y < x) && earlierlist (fun (x, y) -> x < y) (xs, ys)
      | _, ([], []) -> false
      | _, ([], _) -> true
      | _, _ -> false
    (* lists sorted by < or <=; does set diff or bag diff accordingly *)
    let rec sorteddiff a1 a2 a3 =
      match a1, a2, a3 with
        ( < ), [], ys -> []
      | ( < ), xs, [] -> xs
      | ( < ), x1 :: xs, y1 :: ys ->
          if x1 = y1 then sorteddiff (fun (x, y) -> x < y) xs ys
          else if x1 < y1 then
            x1 :: sorteddiff (fun (x, y) -> x < y) xs (y1 :: ys)
          else sorteddiff (fun (x, y) -> x < y) (x1 :: xs) ys
    (* lists sorted by < or <=; does set or bag intersect accordingly *)
    let rec sortedsame a1 a2 a3 =
      match a1, a2, a3 with
        ( < ), [], ys -> []
      | ( < ), xs, [] -> []
      | ( < ), x1 :: xs, y1 :: ys ->
          if x1 = y1 then x1 :: sortedsame (fun (x, y) -> x < y) xs ys
          else if x1 < y1 then sortedsame (fun (x, y) -> x < y) xs (y1 :: ys)
          else sortedsame (fun (x, y) -> x < y) (x1 :: xs) ys
    (* given sorted by < -- no duplicates -- lists. designed to be folded ... *)
    let rec sortedmergeandcombine ( < ) ( + ) (xs, ys) =
      let rec s a1 a2 =
        match a1, a2 with
          [], ys -> ys
        | xs, [] -> xs
        | x1 :: xs, y1 :: ys ->
            if x1 < y1 then x1 :: s xs (y1 :: ys)
            else if y1 < x1 then y1 :: s (x1 :: xs) ys
            else x1 + y1 :: s xs ys
      in
      s xs ys
    let rec sortedmerge ( < ) (xs, ys) =
      sortedmergeandcombine (fun (x, y) -> x < y) (fun (x, _) -> x) (xs, ys)
    (* this ignores elements of ys after the last one that actually occurs in xs *)
    let rec sortedlistsub eq xs ys =
      let rec g a1 a2 =
        match a1, a2 with
          xs, y :: ys ->
            let rec f =
              function
                x :: xs -> if eq (x, y) then g xs ys else x :: f xs
              | [] -> []
            in
            f xs
        | xs, [] -> xs
      in
      g xs ys
    (* matchbag P XS = { (x, P x, XS -- [x]) | x<-XS ; x in dom P } *)
    
    let rec matchbag =
      fun P xs ->
        let rec match__ a1 a2 a3 =
          match a1, a2, a3 with
            r, pre, [] -> r
          | r, pre, x :: xs ->
              match P x with
                Some y -> match__ ((x, y, revapp pre xs) :: r) (x :: pre) xs
              | None -> match__ r (x :: pre) xs
        and revapp a1 a2 =
          match a1, a2 with
            [], ys -> ys
          | x :: xs, ys -> revapp xs (x :: ys)
        in
        match__ [] [] xs
    
    let rec ( >< ) (xs, ys) =
      flatten (MAP ((fun x -> MAP ((fun y -> x, y), ys)), xs))
    (* this function isn't xs><xs -- (xs,xs): 
     * it's the upper triangle (or the lower one) of the matrix xs><xs, without
     * the diagonal.  So it finds all distinct pairs, not pairing x with x and only
     * adding one of <x,y> and <y,x>.  Distinct, that is, if the list xs doesn't contain
     * duplicates.  The _first_ element of the list appears only as the first of a pair;
     * the _last_ element of the list appears only as the second of a pair.
     *)
    let rec allpairs xs =
      sml__hash__2
        (fold
           (fun (x, (ys, ps)) ->
              x :: ys, fold (fun (y, ps) -> (x, y) :: ps) ys ps)
           xs ([], []))
    (* this function is and (map E (bernardszip (xs,ys))) handle bernardszip_ => false, 
       but I'm trying to avoid consing ... Is that necessary or even a good idea?
       There's a lot of tupling going on ...
     *)
    let rec eqlists a1 a2 =
      match a1, a2 with
        E, ([], []) -> true
      | E, (x :: xs, y :: ys) -> E (x, y) && eqlists E (xs, ys)
      | E, _ -> false
    let rec numbered xs =
      let rec r a1 a2 =
        match a1, a2 with
          n, x :: xs -> (n, x) :: r (n + 1) xs
        | _, [] -> []
      in
      r 0 xs
    (* list subtraction, often faster than the list function slosh *)
    let rec listsub eq xs ys =
      let rec S a1 a2 =
        match a1, a2 with
          [], ys -> []
        | xs, [] -> xs
        | xs, y :: ys ->
            let rec strip =
              function
                [] -> []
              | x :: xs -> if eq (x, y) then xs else x :: strip xs
            in
            S (strip xs, ys)
      in
      S (xs, ys)
    let rec eqbags =
      fun E (xs, ys) -> length xs = length ys && null (listsub E xs ys)
    (* first attempt at a topological sort: doesn't try to coalesce cycles *)
    let rec toposort roots depf =
      let rec ts visited (root, (order, cycles)) =
        if member (root, visited) then
          let cycle =
            root :: rev (root :: takewhile (fun x -> x <> root) visited)
          in
          order, cycle :: cycles
        else if member (root, order) then order, cycles
        else
          let children = depf root in
          let (order, cycles) =
            (* revfold to get answer out in children order (whatever it is) *)
            revfold (ts (root :: visited)) children (order, cycles)
          in
          root :: order, cycles
      in
      (* revfold to get answer out in roots order *)
      revfold (ts []) roots ([], [])
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
      let rec combine k m = Integer.max (k, ma m), Integer.min (k, mi m) in
      let rec better m m' = ma m - mi m < ma m' - mi m' in
      (* just look at waste *)
      let rec ok m = ma m <= w in
      let rec zerosplits i =
        if i = n then raise (Catastrophe_ ["zerosplits i=n"])
        else if i + 1 = n then let x = Array.get a i in [(x, x), [1]]
        else
          match zerosplits (i + 1) with
            ((y1, y2), [k]) :: _ as zs ->
              let x = Array.get a i in ((x + y1, x + y2), [k + 1]) :: zs
          | _ -> raise (Catastrophe_ ["zerosplits => []"])
      in
      (* *)
      let rec showsplit ((ma, mi : int * int), (s : int list)) =
        ((((("((" ^ makestring ma) ^ ",") ^ makestring mi) ^ "),") ^
           bracketedliststring makestring "," s) ^
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
          first :: split (i + 1) j (mgs - Array.get a i) ss
        in
        (* *)
        let _ =
          if !minwastedebug then
            consolereport
              ["split ["; makestring (j - i); "] "; makestring mgs; " [";
               makestring (n - j); "] ";
               bracketedliststring showsplit "," (take 3 ss);
               if length ss > 3 then ", ..." else ""]
        in
        (* *)
        match i = j, j = n, ss with
          true, _, [m, s] -> [combine mgs m, 0 :: s]
        | _, _, [m, s] -> foundone (combine mgs m, j - i :: s)
        | _, false, (m1, s1) :: ((m2, s2) :: _ as ss') ->
            let h1 = Array.get a j in
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
                  makestring (j - i); " _ "; makestring (n - j); " ";
                  makestring (length ss)])
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
    let rec minwforcache (w, ns) = minw w (Array.arrayoflist ns) (length ns)
    let (minw, resetminwcache) =
      cache "minwaste"
        (fun ((w, ns), hash, rs) ->
           ((((((((("(" ^ makestring hash) ^ ", ") ^ "(") ^ makestring w) ^
                  ",") ^
                 bracketedliststring makestring "," ns) ^
                " , ") ^
               ") = ") ^
              bracketedliststring makestring "," rs) ^
             ")")
        127 minwforcache
    let hash (w, ns) =
      Bits.xorb (Bits.lshift (w, 2), hashlist (fun i -> i) ns)
    let rec minwaste measurefn w xs =
      let rec recon a1 a2 =
        match a1, a2 with
          n :: ns, xs ->
            if n <= length xs then take n xs :: recon ns (drop n xs)
            else
              raise
                (Catastrophe_
                   ["minwaste 2: ";
                    bracketedliststring makestring "," (n :: ns); "; ";
                    makestring (length xs)])
        | [], [] -> []
        | [], _ -> raise (Catastrophe_ ["minwaste"])
      in
      let wns = w, MAP (measurefn, xs) in recon (minw (hash wns) wns) xs
  end      
