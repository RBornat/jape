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
  
module SimpleCache (AAA : sig val consolereport : string list -> unit 
                              val nj_fold : ('b * 'a -> 'a) -> 'b list -> 'a -> 'a
                              val explode : string -> char list
                          end) :
  SimpleCache =
  struct
    open AAA
    open Array
    
    let simplecachedebug = ref false
    (* for the millennium, better hashing. Well, up to 15 characters will count ... RB 30/xii/99 *)
    let rec hashstring s =
      nj_fold (fun (c, h) -> (h lsl 2) lxor Char.code c) (explode s) 0
    (* for the millennium, better hashing. Well, up to 15 elements will count ... RB 30/xii/99 *)
    let rec hashlist hashf bs =
      nj_fold (fun (b, h) -> (h lsl 2) lxor hashf b) bs 0
    let rec nextprime i =
      let rec isprime k =
        if k * k > i then true else not (i mod k = 0) && isprime (k + 1)
      in
      if isprime 2 then i else nextprime (i + 1)
    (* hashing now done with arrays of lists (why not exploit ML?)
     * rehashed when full, which means a load factor of 1.
     *)
    
    let rec insertreport name report =
      fun hh i stuff ->
        consolereport
          [name; ": inserting "; report stuff; " at "; string_of_int i; " (";
           string_of_int (List.length (Array.get (!hh) i)); ")"]
    let rec hitreport name report stuff =
      consolereport [name; ": hit "; report stuff]
    let rec insert name report =
      fun hh i stuff ->
        if !simplecachedebug then insertreport name report hh i stuff;
        Array.set (!hh) i (stuff :: Array.get (!hh) i)
    let rec blank n = Array.make n []
    let rec rehashall name report =
      fun hh ->
        let oldn = Array.length !hh in
        let oldH = !hh in
        let i = ref 0 in
        let n = nextprime (2 * oldn + 1) in
        let name' = name ^ " (rehash)" in
        hh := blank n;
        if !simplecachedebug then
          begin
            let rs = ref [] in
            let i = ref 0 in
            consolereport
              ["rehashing "; name; " size "; string_of_int oldn; " => ";
               string_of_int n];
            while !i < oldn do
              rs := List.length (Array.get oldH (!i)) :: !rs; i := !i + 1
            done;
            consolereport
              (nj_fold (fun (k, ss) -> string_of_int k :: " " :: ss) !rs [])
          end;
        while !i < oldn do
          let hs = ref (Array.get oldH (!i)) in
          while !hs<>[] do
            let (a, hash, b) = List.hd !hs in
            insert name' report hh (hash mod n) (a, hash, b); hs := List.tl !hs
          done;
          i := !i + 1
        done
    let rec data hh = 
        let i = ref 0 in
        let rs = ref [] in
        let n = Array.length !hh in
        while !i < n do rs := Array.get (!hh) (!i) :: !rs; i := !i + 1 done;
        nj_fold (fun (x, y) -> x @ y) !rs []
    let rec sources = fun hh () -> List.map (fun (a, _, _) -> a) (data hh)
    let rec targets = fun hh () -> List.map (fun (a, _, b) -> b) (data hh)
    let rec simplecache name report k f =
      let k = nextprime k in
      let hh = ref (blank k) in
      let load = ref 0 in
      let rec eval hash a =
        let i = hash mod Array.length !hh in
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
              if !simplecachedebug then insertreport name report hh i stuff;
              load := !load + 1;
              b, [stuff]
        in
        let (b, hs) = ev (Array.get (!hh) i) in
        Array.set (!hh) i hs;
        if !load > Array.length !hh then rehashall name report hh;
        b
      in
      eval, (fun () -> hh := blank k)
    let rec simplestore name report k =
      let k = nextprime k in
      let hh = ref (blank k) in
      let load = ref 0 in
      let rec index f hash a = f (hash mod Array.length !hh) a in
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
        find (Array.get (!hh) i)
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
        Array.set (!hh) i (del (Array.get (!hh) i))
      in
      let rec update hash a b =
        let i = hash mod Array.length !hh in
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
              if !simplecachedebug then insertreport name report hh i stuff;
              load := !load + 1;
              [stuff]
        in
        Array.set (!hh) i (upd (Array.get (!hh) i));
        if !load > Array.length !hh then rehashall name report hh
      in
      index lookup, update, index delete, (fun () -> hh := blank k), sources hh,
      targets hh
  end
