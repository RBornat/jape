(* $Id$ *)

module type Context =
  sig
    type vid
    and term
    and resnum
    and seq
    and proviso
    and visproviso
    and idclass
    type ('a, 'b) mapping and cxt
    val newcxt : cxt
    val dont_rewrite_with_this : cxt
    (* interrogation functions *)
    val varmap : cxt -> (vid, term) mapping
    val resmap : cxt -> (int, (resnum * term)) mapping
    val provisos : cxt -> visproviso list
    val usedVIDs : cxt -> vid list
    val nextresnum : cxt -> int
    (* assignment functions *)
    val withvarmap : cxt * (vid, term) mapping -> cxt
    val withresmap : cxt * (int, (resnum * term)) mapping -> cxt
    val withprovisos : cxt * visproviso list -> cxt
    val withvisibleprovisos : cxt * proviso list -> cxt
    val withusedVIDs : cxt * vid list -> cxt
    val withexterior : cxt * (seq list * seq) -> cxt
    val withresnum : cxt * int -> cxt
    (* augmentation functions *)
    val plusvarmap : cxt * (vid, term) mapping -> cxt
    val plusresmap : cxt * (int, (resnum * term)) mapping -> cxt
    val plusprovisos : cxt * visproviso list -> cxt
    val plusvisibleprovisos : cxt * proviso list -> cxt
    val plususedVIDs : cxt * vid list -> cxt
    (* 'side-effecting' functions *)
    val freshVID : cxt -> idclass -> vid -> cxt * vid
    val freshproofvar : cxt -> idclass -> vid -> cxt * term
    val freshresnum : cxt -> cxt * int
    (* normalising functions *)
    val selfparentprovisos : cxt -> cxt
    val cxtstring : cxt -> string
  end

module type Rew_Context =
  sig
    type cxt and proviso and visproviso and rewinf
    (* for now, provisosigs are ints *)
    val getprovisos : cxt -> visproviso list * rewinf option
    val setprovisos : cxt -> visproviso list * rewinf option -> cxt
    val getprovisosig : cxt -> int
    val incprovisosig : cxt -> cxt
  end

module type FactsContext =
  sig
    type cxt and seq and term and ('a, 'b) mapping and rewinf
    (* this information perhaps ought to be elsewhere in the proof state, but it's here
     * now, so what the hell.
     *
     * It records the 'exteriority' of a proof: information about the base judgment and 
     * the given judgements, which are where the proof touches the outside world.
     *
     * Exterior(ss*s,inf,fvinf):
     *   ss are the givens, s is the base sequent of the proof;
     *   inf is the rewinf of that collection of sequents;
     *   fvinf is information about the ways that names relate to each other, used
     *     to decide 'what dominates what', and some other stuff.  It consists of
     *       avs  : all variables in the exterior
     *       fvs  : all free variables in the exterior
     *       vmap : the 'what dominates what' mapping
     *       bhfvs: all free variables of base hypotheses
     *       bcfvs: all free variables of base conclusions
     *
     *       (I promise, one day, to write down what vmap is and how it's used.
     *        Until that day, look in facts.sml, where it's used
     *       )
     *       
     *)
     
    type exterior =
        NoExterior
      | Exterior of
          ((seq list * seq) * rewinf option *
             (term list * term list * (term, term list) mapping * term list *
                term list)
               option)
    val setexterior : cxt -> exterior -> cxt
    val getexterior : cxt -> exterior
    val exteriorstring : exterior -> string
    val exteriorinf : cxt -> rewinf option
  end
(* $Id$ *)

module M :
  sig include Context include Rew_Context include FactsContext end =
  struct
    open Mappingfuns open Term open Proviso open Rewinf
    
    type seq = seq
    type exterior =
        NoExterior
      | Exterior of
          ((seq list * seq) * rewinf option *
             (term list * term list * (term, term list) mapping * term list *
                term list)
               option)
    type cxt =
        Context of
          < varmap : (vid, term) mapping;
            resmap : (int, (resnum * term)) mapping;
            provisos : visproviso list * rewinf option; provisosig : int;
            outside : exterior; usedVIDs : vid list; nextresnum : int >
    let rec pid s = s
    let rec exteriorstring =
      function
        NoExterior -> "NoExterior"
      | Exterior e ->
          "Exterior" ^
            triplestring
              (pairstring (bracketedliststring seqstring " AND ") seqstring
                 ",")
              (optionstring rewinfstring)
              (optionstring
                 (quintuplestring termliststring termliststring
                    (mappingstring termstring termliststring) termliststring
                    termliststring ","))
              ", " e
    let pint : int -> string = string_of_int
    let rec cxtstring =
      fun
        (Context
           {varmap = varmap;
            resmap = resmap;
            provisos = ps, inf;
            provisosig = provisosig;
            outside = outside;
            usedVIDs = usedVIDs;
            nextresnum = nextresnum}) ->
        implode
          ["Context{"; "varmap="; mappingstring pid termstring varmap; ", ";
           "resmap=";
           mappingstring pint (pairstring resnumstring termstring ",") resmap;
           ", "; "provisos=(";
           bracketedliststring visprovisostringall " AND " ps; ",";
           optionstring rewinfstring inf; "), "; "provisosig=";
           string_of_int provisosig; ", "; "outside="; exteriorstring outside;
           ", "; "usedVIDs="; bracketedliststring pid "," usedVIDs; ", ";
           "nextresnum="; string_of_int nextresnum; "}"]
    
    let rec varmap = fun (Context {varmap = varmap}) -> varmap
    let rec resmap = fun (Context {resmap = resmap}) -> resmap
    let rec provisos = fun (Context {provisos = ps, _}) -> ps
    let rec getprovisos = fun (Context {provisos = provisos}) -> provisos
    let rec getprovisosig =
      fun (Context {provisosig = provisosig}) -> provisosig
    let rec getexterior = fun (Context {outside = outside}) -> outside
    let rec usedVIDs = fun (Context {usedVIDs = usedVIDs}) -> usedVIDs
    let rec nextresnum = fun (Context {nextresnum = nextresnum}) -> nextresnum
    let rec withvarmap =
      fun
        (Context
           {varmap = varmap;
            resmap = resmap;
            provisos = provisos;
            provisosig = provisosig;
            outside = outside;
            usedVIDs = usedVIDs;
            nextresnum = nextresnum}, map) ->
        Context
          (let module M =
             struct
               class a =
                 object
                   val varmap = map
                   val resmap = resmap
                   val provisos = provisos
                   val provisosig = provisosig
                   val outside = outside
                   val usedVIDs = usedVIDs
                   val nextresnum = nextresnum
                   method varmap = varmap
                   method resmap = resmap
                   method provisos = provisos
                   method provisosig = provisosig
                   method outside = outside
                   method usedVIDs = usedVIDs
                   method nextresnum = nextresnum
                 end
             end
           in
           new M.a)
    let rec plusvarmap =
      fun
        (Context
           {varmap = varmap;
            resmap = resmap;
            provisos = provisos;
            provisosig = provisosig;
            outside = outside;
            usedVIDs = usedVIDs;
            nextresnum = nextresnum}, map) ->
        Context
          (let module M =
             struct
               class a =
                 object
                   val varmap = ( ++ ) (varmap, map)
                   val resmap = resmap
                   val provisos = provisos
                   val provisosig = provisosig
                   val outside = outside
                   val usedVIDs = usedVIDs
                   val nextresnum = nextresnum
                   method varmap = varmap
                   method resmap = resmap
                   method provisos = provisos
                   method provisosig = provisosig
                   method outside = outside
                   method usedVIDs = usedVIDs
                   method nextresnum = nextresnum
                 end
             end
           in
           new M.a)
    let rec withresmap =
      fun
        (Context
           {varmap = varmap;
            resmap = resmap;
            provisos = provisos;
            provisosig = provisosig;
            outside = outside;
            usedVIDs = usedVIDs;
            nextresnum = nextresnum}, map) ->
        Context
          (let module M =
             struct
               class a =
                 object
                   val varmap = varmap
                   val resmap = map
                   val provisos = provisos
                   val provisosig = provisosig
                   val outside = outside
                   val usedVIDs = usedVIDs
                   val nextresnum = nextresnum
                   method varmap = varmap
                   method resmap = resmap
                   method provisos = provisos
                   method provisosig = provisosig
                   method outside = outside
                   method usedVIDs = usedVIDs
                   method nextresnum = nextresnum
                 end
             end
           in
           new M.a)
    let rec plusresmap =
      fun
        (Context
           {varmap = varmap;
            resmap = resmap;
            provisos = provisos;
            provisosig = provisosig;
            outside = outside;
            usedVIDs = usedVIDs;
            nextresnum = nextresnum}, map) ->
        Context
          (let module M =
             struct
               class a =
                 object
                   val varmap = varmap
                   val resmap = ( ++ ) (resmap, map)
                   val provisos = provisos
                   val provisosig = provisosig
                   val outside = outside
                   val usedVIDs = usedVIDs
                   val nextresnum = nextresnum
                   method varmap = varmap
                   method resmap = resmap
                   method provisos = provisos
                   method provisosig = provisosig
                   method outside = outside
                   method usedVIDs = usedVIDs
                   method nextresnum = nextresnum
                 end
             end
           in
           new M.a)
    let rec withprovisos =
      function
        Context
          {varmap = varmap;
           resmap = resmap;
           provisos = provisos;
           provisosig = provisosig;
           outside = outside;
           usedVIDs = usedVIDs;
           nextresnum = nextresnum}, [] ->
          Context
            (let module M =
               struct
                 class a =
                   object
                     val varmap = varmap
                     val resmap = resmap
                     val provisos = [], Some nullrewinf
                     val provisosig =
                       match provisos with
                         [], _ -> provisosig
                       | _ -> provisosig + 1
                     val outside = outside
                     val usedVIDs = usedVIDs
                     val nextresnum = nextresnum
                     method varmap = varmap
                     method resmap = resmap
                     method provisos = provisos
                     method provisosig = provisosig
                     method outside = outside
                     method usedVIDs = usedVIDs
                     method nextresnum = nextresnum
                   end
               end
             in
             new M.a)
      | Context
          {varmap = varmap;
           resmap = resmap;
           provisos = provisos;
           provisosig = provisosig;
           outside = outside;
           usedVIDs = usedVIDs;
           nextresnum = nextresnum}, ps ->
          Context
            (let module M =
               struct
                 class a =
                   object
                     val varmap = varmap
                     val resmap = resmap
                     val provisos = ps, None
                     val provisosig = provisosig + 1
                     val outside = outside
                     val usedVIDs = usedVIDs
                     val nextresnum = nextresnum
                     method varmap = varmap
                     method resmap = resmap
                     method provisos = provisos
                     method provisosig = provisosig
                     method outside = outside
                     method usedVIDs = usedVIDs
                     method nextresnum = nextresnum
                   end
               end
             in
             new M.a)
    let rec withvisibleprovisos (cxt, ps) =
      withprovisos (cxt, List.map (fun p -> mkvisproviso (true, p)) ps)
    let rec plusprovisos =
      function
        cxt, [] -> cxt
      | Context
          {varmap = varmap;
           resmap = resmap;
           provisos = ps, _;
           provisosig = provisosig;
           outside = outside;
           usedVIDs = usedVIDs;
           nextresnum = nextresnum}, ps' ->
          Context
            (let module M =
               struct
                 class a =
                   object
                     val varmap = varmap
                     val resmap = resmap
                     val provisos = ps' @ ps, None
                     val provisosig = provisosig + 1
                     val outside = outside
                     val usedVIDs = usedVIDs
                     val nextresnum = nextresnum
                     method varmap = varmap
                     method resmap = resmap
                     method provisos = provisos
                     method provisosig = provisosig
                     method outside = outside
                     method usedVIDs = usedVIDs
                     method nextresnum = nextresnum
                   end
               end
             in
             new M.a)
    (* this function used by rewrite, which is manipulating the provisosig intelligently *)
    let rec setprovisos =
      fun
        (Context
           {varmap = varmap;
            resmap = resmap;
            provisos = provisos;
            provisosig = provisosig;
            outside = outside;
            usedVIDs = usedVIDs;
            nextresnum = nextresnum})
        ps ->
        Context
          (let module M =
             struct
               class a =
                 object
                   val varmap = varmap
                   val resmap = resmap
                   val provisos = ps
                   val provisosig = provisosig
                   val outside = outside
                   val usedVIDs = usedVIDs
                   val nextresnum = nextresnum
                   method varmap = varmap
                   method resmap = resmap
                   method provisos = provisos
                   method provisosig = provisosig
                   method outside = outside
                   method usedVIDs = usedVIDs
                   method nextresnum = nextresnum
                 end
             end
           in
           new M.a)
    let rec plusvisibleprovisos (cxt, ps) =
      plusprovisos (cxt, List.map (fun p -> mkvisproviso (true, p)) ps)
    let nextprovisosig = ref 0
    (* at 1000 contexts/sec, this will last 2^30/1000 = 1M seconds.  Long enough (we are
     * certainly not doing 1K contexts/sec, and when we do, we will undoubtedly have 
     * 64-bit desktop machines).
     * RB 14/viii/97
     *)
    let rec incprovisosig =
      fun
        (Context
           {varmap = varmap;
            resmap = resmap;
            provisos = provisos;
            provisosig = provisosig;
            outside = outside;
            usedVIDs = usedVIDs;
            nextresnum = nextresnum}) ->
        let rec bang () =
          raise (Catastrophe_ ["STOP, STOP, STOP!!!! too many contexts!!!!"])
        in
        begin try _RR nextprovisosig with
          _ -> bang ()
        end;
        if !nextprovisosig <= 0 then bang ();
        Context
          (let module M =
             struct
               class a =
                 object
                   val varmap = varmap
                   val resmap = resmap
                   val provisos = provisos
                   val provisosig = !nextprovisosig
                   val outside = outside
                   val usedVIDs = usedVIDs
                   val nextresnum = nextresnum
                   method varmap = varmap
                   method resmap = resmap
                   method provisos = provisos
                   method provisosig = provisosig
                   method outside = outside
                   method usedVIDs = usedVIDs
                   method nextresnum = nextresnum
                 end
             end
           in
           new M.a)
    let rec withexterior =
      fun
        (Context
           {varmap = varmap;
            resmap = resmap;
            provisos = provisos;
            provisosig = provisosig;
            outside = outside;
            usedVIDs = usedVIDs;
            nextresnum = nextresnum}, s) ->
        Context
          (let module M =
             struct
               class a =
                 object
                   val varmap = varmap
                   val resmap = resmap
                   val provisos = provisos
                   val provisosig = provisosig
                   val outside = Exterior (s, None, None)
                   val usedVIDs = usedVIDs
                   val nextresnum = nextresnum
                   method varmap = varmap
                   method resmap = resmap
                   method provisos = provisos
                   method provisosig = provisosig
                   method outside = outside
                   method usedVIDs = usedVIDs
                   method nextresnum = nextresnum
                 end
             end
           in
           new M.a)
    let rec setexterior =
      fun
        (Context
           {varmap = varmap;
            resmap = resmap;
            provisos = provisos;
            provisosig = provisosig;
            outside = outside;
            usedVIDs = usedVIDs;
            nextresnum = nextresnum})
        s ->
        Context
          (let module M =
             struct
               class a =
                 object
                   val varmap = varmap
                   val resmap = resmap
                   val provisos = provisos
                   val provisosig = provisosig
                   val outside = s
                   val usedVIDs = usedVIDs
                   val nextresnum = nextresnum
                   method varmap = varmap
                   method resmap = resmap
                   method provisos = provisos
                   method provisosig = provisosig
                   method outside = outside
                   method usedVIDs = usedVIDs
                   method nextresnum = nextresnum
                 end
             end
           in
           new M.a)
    let rec exteriorinf =
      fun
        (Context
           {varmap = varmap;
            resmap = resmap;
            provisos = provisos;
            provisosig = provisosig;
            outside = outside;
            usedVIDs = usedVIDs;
            nextresnum = nextresnum}) ->
        match outside with
          NoExterior -> None
        | Exterior (_, inf, _) -> inf
    let rec withusedVIDs =
      fun
        (Context
           {varmap = varmap;
            resmap = resmap;
            provisos = provisos;
            provisosig = provisosig;
            outside = outside;
            usedVIDs = usedVIDs;
            nextresnum = nextresnum}, vs) ->
        Context
          (let module M =
             struct
               class a =
                 object
                   val varmap = varmap
                   val resmap = resmap
                   val provisos = provisos
                   val provisosig = provisosig
                   val outside = outside
                   val usedVIDs = vs
                   val nextresnum = nextresnum
                   method varmap = varmap
                   method resmap = resmap
                   method provisos = provisos
                   method provisosig = provisosig
                   method outside = outside
                   method usedVIDs = usedVIDs
                   method nextresnum = nextresnum
                 end
             end
           in
           new M.a)
    let rec plususedVIDs =
      fun
        (Context
           {varmap = varmap;
            resmap = resmap;
            provisos = provisos;
            provisosig = provisosig;
            outside = outside;
            usedVIDs = usedVIDs;
            nextresnum = nextresnum}, vs) ->
        Context
          (let module M =
             struct
               class a =
                 object
                   val varmap = varmap
                   val resmap = resmap
                   val provisos = provisos
                   val provisosig = provisosig
                   val outside = outside
                   val usedVIDs = mergeVIDs usedVIDs vs
                   val nextresnum = nextresnum
                   method varmap = varmap
                   method resmap = resmap
                   method provisos = provisos
                   method provisosig = provisosig
                   method outside = outside
                   method usedVIDs = usedVIDs
                   method nextresnum = nextresnum
                 end
             end
           in
           new M.a)
    let rec freshVID =
      fun (Context {usedVIDs = usedVIDs} as cxt) class__ v ->
        let v' = uniqueVID class__ usedVIDs [] v in
        plususedVIDs (cxt, [v']), v'
    (* if this function is applied when the base sequent hasn't been rewritten,
     * it isn't very useful.  But it can happen sometimes, when you are just
     * making freshRule to find out something about the rule.
    *)
    let rec freshproofvar cxt class__ v =
      let (cxt', v') = freshVID cxt class__ v in
      let var = Id (v', class__) in
      match cxt' with
        Context {usedVIDs = usedVIDs; outside = Exterior (_, Some r, _)} ->
          plusprovisos
            (cxt',
             nj_fold
               (fun (u, ps) ->
                  mkvisproviso (false, mkNotinProviso (var, u)) :: ps)
               (isUnknown <| rewinf_vars r) []),
          var
      | _ -> cxt', var
    let rec withresnum =
      fun
        (Context
           {varmap = varmap;
            resmap = resmap;
            provisos = provisos;
            provisosig = provisosig;
            outside = outside;
            usedVIDs = usedVIDs;
            nextresnum = nextresnum}, num) ->
        Context
          (let module M =
             struct
               class a =
                 object
                   val varmap = varmap
                   val resmap = resmap
                   val provisos = provisos
                   val provisosig = provisosig
                   val outside = outside
                   val usedVIDs = usedVIDs
                   val nextresnum = num
                   method varmap = varmap
                   method resmap = resmap
                   method provisos = provisos
                   method provisosig = provisosig
                   method outside = outside
                   method usedVIDs = usedVIDs
                   method nextresnum = nextresnum
                 end
             end
           in
           new M.a)
    let rec freshresnum cxt =
      let n = nextresnum cxt in withresnum (cxt, n + 1), n
    let newcxt =
      Context
        (let module M =
           struct
             class a =
               object
                 val varmap = empty
                 val resmap = empty
                 val provisos = [], Some nullrewinf
                 val provisosig = 0
                 val outside = NoExterior
                 val usedVIDs = []
                 val nextresnum = 1
                 method varmap = varmap
                 method resmap = resmap
                 method provisos = provisos
                 method provisosig = provisosig
                 method outside = outside
                 method usedVIDs = usedVIDs
                 method nextresnum = nextresnum
               end
           end
         in
         new M.a)
    let rec selfparentprovisos =
      fun
        (Context
           {varmap = varmap;
            resmap = resmap;
            provisos = ps, ri;
            provisosig = provisosig;
            outside = outside;
            usedVIDs = usedVIDs;
            nextresnum = nextresnum}) ->
        Context
          (let module M =
             struct
               class a =
                 object
                   val varmap = varmap
                   val resmap = resmap
                   val provisos = List.map provisoselfparent ps, ri
                   val provisosig = provisosig
                   val outside = outside
                   val usedVIDs = usedVIDs
                   val nextresnum = nextresnum
                   method varmap = varmap
                   method resmap = resmap
                   method provisos = provisos
                   method provisosig = provisosig
                   method outside = outside
                   method usedVIDs = usedVIDs
                   method nextresnum = nextresnum
                 end
             end
           in
           new M.a)
    (* this context is provided so that you can get a neutral reading of rewinf from some
     * formula that you haven't rewritten, or don't know has been rewritten
     *)
    let dont_rewrite_with_this =
      Context
        (let module M =
           struct
             class a =
               object
                 val varmap = empty
                 val resmap = empty
                 val provisos = [], Some nullrewinf
                 val provisosig = - 463
                 val outside = NoExterior
                 val usedVIDs = []
                 val nextresnum = - 999
                 method varmap = varmap
                 method resmap = resmap
                 method provisos = provisos
                 method provisosig = provisosig
                 method outside = outside
                 method usedVIDs = usedVIDs
                 method nextresnum = nextresnum
               end
           end
         in
         new M.a)
  end
