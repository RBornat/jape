(* $Id$ *)

module type SimpleCache =
  sig
    val hashstring : string -> int
    val hashlist : ('a -> int) -> 'a list -> int
    val simplecache :
      string -> ('a * int * 'b -> string) -> int -> ('a -> 'b) ->
        (int -> 'a -> 'b) * (unit -> unit)
    (* function         eval                   reset        *)

    val simplestore :
      string -> ('a * int * 'b -> string) -> int ->
        (int -> 'a -> 'b option) * (int -> 'a -> 'b -> unit) *
          (int -> 'a -> unit) * (unit -> unit) * (unit -> 'a list) *
          (unit -> 'b list)
    (* sources               targets            *)

    val simplecachedebug : bool ref
  end
module SimpleCache (AAA : sig val consolereport : string list -> unit end) :
  SimpleCache =
  struct
    open AAA
    open Array
    
    let simplecachedebug = ref false
    (* for the millennium, better hashing. Well, up to 15 characters will count ... RB 30/xii/99 *)
    let rec hashstring s =
      fold (fun (c, h) -> Bits.xorb (Bits.lshift (h, 2), ord c)) (explode s) 0
    (* for the millennium, better hashing. Well, up to 15 elements will count ... RB 30/xii/99 *)
    let rec hashlist hashf bs =
      fold (fun (b, h) -> Bits.xorb (Bits.lshift (h, 2), hashf b)) bs 0
    let rec nextprime i =
      let rec isprime k =
        if k * k > i then true else not (i mod k = 0) && isprime (k + 1)
      in
      if isprime 2 then i else nextprime (i + 1)
    (* hashing now done with arrays of lists (why not exploit ML?)
     * rehashed when full, which means a load factor of 1.
     *)
    
    let rec insertreport name report =
      fun H i stuff ->
        consolereport
          [name; ": inserting "; report stuff; " at "; makestring i; " (";
           makestring (List.length (Array.get (!H) i)); ")"]
    let rec hitreport name report stuff =
      consolereport [name; ": hit "; report stuff]
    let rec insert name report =
      fun H i stuff ->
        if !simplecachedebug then insertreport name report H i stuff;
        Array.set (!H) i (stuff :: Array.get (!H) i)
    let rec blank n = Array.make n []
    let rec rehashall name report =
      fun H ->
        let oldn = Array.length !H in
        let oldH = !H in
        let i = ref 0 in
        let n = nextprime (2 * oldn + 1) in
        let name' = name ^ " (rehash)" in
        H := blank n;
        if !simplecachedebug then
          begin
            let rs = ref [] in
            let i = ref 0 in
            consolereport
              ["rehashing "; name; " size "; makestring oldn; " => ";
               makestring n];
            while !i < oldn do
              rs := List.length (Array.get oldH (!i)) :: !rs; i := !i + 1
            done;
            consolereport
              (fold (fun (k, ss) -> makestring k :: " " :: ss) !rs [])
          end;
        while !i < oldn do
          let hs = ref (Array.get oldH (!i)) in
          while not (null !hs) do
            let (a, hash, b) = List.hd !hs in
            insert name' report H (hash mod n) (a, hash, b); hs := List.tl !hs
          done;
          i := !i + 1
        done
    let rec data =
      fun H ->
        let i = ref 0 in
        let rs = ref [] in
        let n = Array.length !H in
        while !i < n do rs := Array.get (!H) (!i) :: !rs; i := !i + 1 done;
        fold (fun (x, y) -> x @ y) !rs []
    let rec sources = fun H () -> map (fun (a, _, _) -> a) (data H)
    let rec targets = fun H () -> map (fun (a, _, b) -> b) (data H)
    let rec simplecache name report k f =
      let k = nextprime k in
      let H = ref (blank k) in
      let load = ref 0 in
      let rec eval hash a =
        let i = hash mod Array.length !H in
        let rec ev =
          function
            (a', _, b as h) :: hs ->
              if a = a' then
                begin
                  if !simplecachedebug then hitreport name report h;
                  b, h :: hs
                end
              else let (b, hs) = ev hs in b, h :: hs
          | [] ->
              let b = f a in
              let stuff = a, hash, b in
              if !simplecachedebug then insertreport name report H i stuff;
              load := !load + 1;
              b, [stuff]
        in
        let (b, hs) = ev (Array.get (!H) i) in
        Array.set (!H) i hs;
        if !load > Array.length !H then rehashall name report H;
        b
      in
      eval, (fun () -> H := blank k)
    let rec simplestore name report k =
      let k = nextprime k in
      let H = ref (blank k) in
      let load = ref 0 in
      let rec index f hash a = f (hash mod Array.length !H) a in
      let rec lookup i a =
        let rec find =
          function
            (a', _, b as h) :: hs ->
              if a = a' then
                begin
                  if !simplecachedebug then hitreport name report h; Some b
                end
              else find hs
          | [] -> None
        in
        find (Array.get (!H) i)
      in
      let rec delete i a =
        let rec del =
          function
            (a', _, _ as h) :: hs ->
              if a = a' then
                begin
                  if !simplecachedebug then
                    consolereport [name; ": deleting "; report h];
                  load := !load - 1;
                  hs
                end
              else h :: del hs
          | [] -> []
        in
        Array.set (!H) i (del (Array.get (!H) i))
      in
      let rec update hash a b =
        let i = hash mod Array.length !H in
        let stuff = a, hash, b in
        let rec upd =
          function
            (a', _, _ as h) :: hs ->
              if a = a' then
                begin
                  if !simplecachedebug then
                    consolereport
                      [name; ": replacing "; report h; " by "; report stuff];
                  stuff :: hs
                end
              else h :: upd hs
          | [] ->
              if !simplecachedebug then insertreport name report H i stuff;
              load := !load + 1;
              [stuff]
        in
        Array.set (!H) i (upd (Array.get (!H) i));
        if !load > Array.length !H then rehashall name report H
      in
      index lookup, update, index delete, (fun () -> H := blank k), sources H,
      targets H
  end
