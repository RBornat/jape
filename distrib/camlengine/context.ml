(*
	$Id$

    This file is part of the jape proof engine, which is part of jape.

    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).

*)

module type Type =
sig
    type seq and rewinf and term and ('a,'b) mapping and vid and resnum and visproviso
    type exterior =
        NoExterior
      | Exterior of
          ((seq list * seq) * rewinf option *
             (term list * term list * (term, term list) mapping * term list *
                term list)
               option)
    type cxtrec = 
          { varmap : (vid, term) mapping;
            resmap : (int, (resnum * term)) mapping;
            provisos : visproviso list * rewinf option; provisosig : int;
            outside : exterior; usedVIDs : vid list; nextresnum : int }
    type cxt = Context of cxtrec
end

module Type : Type with type seq = Sequent.Type.seq 
                    and type rewinf = Rewinf.rewinf
                    and type term = Term.Funs.term
                    and type ('a,'b) mapping = ('a,'b) Mappingfuns.mapping
					and type vid = Term.Funs.vid
					and type resnum = Term.Funs.resnum
                    and type visproviso = Proviso.visproviso
=
struct 
    type seq = Sequent.Type.seq
     and rewinf = Rewinf.rewinf
     and term = Term.Funs.term
     and ('a,'b) mapping = ('a,'b) Mappingfuns.mapping
     and vid = Term.Funs.vid
     and resnum = Term.Funs.resnum
     and visproviso = Proviso.visproviso
    
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
    type cxtrec = 
          { varmap : (vid, term) mapping;
            resmap : (int, (resnum * term)) mapping;
            provisos : visproviso list * rewinf option; provisosig : int;
            outside : exterior; usedVIDs : vid list; nextresnum : int }
    
    type cxt = Context of cxtrec
end

module type Cxtstring =
  sig
    type cxt and exterior
    val cxtstring : cxt -> string
    val exteriorstring : exterior -> string
  end
  
module Cxtstring : Cxtstring with type cxt = Type.cxt
                              and type exterior = Type.exterior
=
  struct
    open Type
    
    open Listfuns
    open Sequent.Funs
    open Stringfuns
    open Optionfuns
    open Rewinf
    open Term.Termstring
    open Mappingfuns
    open Sml
    open Proviso
    
    type cxt = Type.cxt
     and exterior = Type.exterior
     
    let exteriorstring =
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
    let pint = string_of_int
    let pid = Term.Funs.string_of_vid
    let cxtstring =
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
  end
      
module type Cxt =
  sig
    type vid and term and resnum and seq and proviso and visproviso 
     and idclass and ('a, 'b) mapping and cxt
    val newcxt : cxt
    val dont_rewrite_with_this : cxt
    (* interrogation functions *)
    val varmap : cxt -> (vid, term) mapping
    val resmap : cxt -> (int, (resnum * term)) mapping
    val provisos : cxt -> visproviso list
    val usedVIDs : cxt -> vid list
    val nextresnum : cxt -> int
    (* assignment functions *)
    val withvarmap : cxt -> (vid, term) mapping -> cxt
    val withresmap : cxt -> (int, (resnum * term)) mapping -> cxt
    val withprovisos : cxt -> visproviso list -> cxt
    val withvisibleprovisos : cxt -> proviso list -> cxt
    val withusedVIDs : cxt -> vid list -> cxt
    val withexterior : cxt -> (seq list * seq) -> cxt
    val withresnum : cxt -> int -> cxt
    (* augmentation functions *)
    val plusvarmap : cxt -> (vid, term) mapping -> cxt
    val plusresmap : cxt -> (int, (resnum * term)) mapping -> cxt
    val plusprovisos : cxt -> visproviso list -> cxt
    val plusvisibleprovisos : cxt -> proviso list -> cxt
    val plususedVIDs : cxt -> vid list -> cxt
    (* 'side-effecting' functions *)
    val freshVID : cxt -> idclass -> vid -> cxt * vid
    val freshproofvar : cxt -> idclass -> vid -> cxt * term
    val freshresnum : cxt -> cxt * int
    (* normalising functions *)
    val selfparentprovisos : cxt -> cxt
  end

module Cxt : Cxt with type cxt = Type.cxt
                  and type seq = Sequent.Type.seq
                  and type term = Term.Funs.term
                  and type ('a,'b) mapping = ('a,'b) Mappingfuns.mapping
                  and type vid = Term.Funs.vid
                  and type resnum = Term.Funs.resnum
                  and type proviso = Proviso.proviso
                  and type visproviso = Proviso.visproviso
                  and type idclass = Idclass.idclass
=
  struct
    open Type
    
    type cxt = Type.cxt
     and seq = Sequent.Type.seq
     and term = Term.Funs.term
     and ('a,'b) mapping = ('a,'b) Mappingfuns.mapping
     and vid = Term.Funs.vid
     and resnum = Term.Funs.resnum
     and proviso = Proviso.proviso
     and visproviso = Proviso.visproviso
     and idclass = Idclass.idclass
    
    open Mappingfuns
    open Rewinf
    open Proviso
    open Term.Funs
    open Term.Store
    open Sml
    open Listfuns
    
    let fNotinProviso v = Provisotype.NotinProviso v

    let varmap = fun (Context {varmap = varmap}) -> varmap
    let resmap = fun (Context {resmap = resmap}) -> resmap
    let provisos = fun (Context {provisos = ps, _}) -> ps
    let usedVIDs = fun (Context {usedVIDs = usedVIDs}) -> usedVIDs
    let nextresnum = fun (Context {nextresnum = nextresnum}) -> nextresnum
    
    let withvarmap =
      fun (Context cxt) map -> Context {cxt with varmap=map}
    let plusvarmap =
      fun (Context ({varmap=varmap} as cxt)) map -> 
        Context {cxt with varmap = (varmap ++ map)}
    let withresmap =
      fun (Context cxt) map -> Context {cxt with resmap=map}
    let plusresmap =
      fun (Context ({resmap=resmap} as cxt)) map -> 
        Context {cxt with resmap = (resmap ++ map)}
    let withprovisos c ps =
      match c, ps with
        (Context ({provisos=provisos; provisosig=provisosig} as cxt)), [] -> 
		   Context {cxt with provisos = [], Some nullrewinf;
                                     provisosig = match provisos with
                                                    [], _ -> provisosig
                                                  | _     -> provisosig + 1}
	  | (Context ({provisosig=provisosig} as cxt)), ps -> 
		   Context {cxt with provisos = ps, None;
                                    provisosig = provisosig + 1}
    let withvisibleprovisos cxt ps =
      withprovisos cxt (List.map (fun p -> mkvisproviso (true, p)) ps)
    let plusprovisos c ps =
      match c, ps with
        cxt, [] -> cxt
      | (Context ({provisos = ps, _; provisosig = provisosig} as cxt)), ps' ->
          Context {cxt with provisos = ps' @ ps, None;
                            provisosig = provisosig + 1}

    let plusvisibleprovisos cxt ps =
      plusprovisos cxt (List.map (fun p -> mkvisproviso (true, p)) ps)
    let withexterior =
      fun (Context cxt) s ->
        Context {cxt with outside = Exterior (s, None, None)}
    let withusedVIDs =
      fun (Context cxt) vs ->
        Context {cxt with usedVIDs = vs}
    let plususedVIDs =
      fun (Context ({usedVIDs = usedVIDs} as cxt)) vs ->
        Context {cxt with usedVIDs = mergeVIDs usedVIDs vs}
    let freshVID =
      fun (Context {usedVIDs = usedVIDs} as cxt) class__ v ->
        let v' = uniqueVID class__ usedVIDs [] v in
        plususedVIDs cxt [v'], v'
    (* if this function is applied when the base sequent hasn't been rewritten,
     * it isn't very useful.  But it can happen sometimes, when you are just
     * making freshRule to find out something about the rule.
    *)
    let freshproofvar cxt class__ v =
      let (cxt', v') = freshVID cxt class__ v in
      let var = registerId (v', class__) in
      match cxt' with
        Context {usedVIDs = usedVIDs; outside = Exterior (_, Some r, _)} ->
          plusprovisos cxt'
            (nj_fold
               (fun (u, ps) ->
                  mkvisproviso (false, fNotinProviso (var, u)) :: ps)
               (isUnknown <| rewinf_vars r) []),
          var
      | _ -> cxt', var
    let withresnum =
      fun (Context cxt) num ->
        Context {cxt with nextresnum = num}
    let freshresnum cxt =
      let n = nextresnum cxt in withresnum cxt (n + 1), n
    let newcxt =
      Context {varmap = empty; resmap = empty; provisos = [], Some nullrewinf;
               provisosig = 0; outside = NoExterior; usedVIDs = [];
               nextresnum = 1}
    let selfparentprovisos =
      fun (Context ({provisos = ps, ri} as cxt)) ->
        Context {cxt with provisos = List.map provisoselfparent ps, ri}
    (* this context is provided so that you can get a neutral reading of rewinf from some
     * formula that you haven't rewritten, or don't know has been rewritten
     *)
    let dont_rewrite_with_this =
      Context {varmap = empty; resmap = empty; provisos = [], Some nullrewinf;
               provisosig = -463; outside = NoExterior; usedVIDs = [];
               nextresnum = -999}
  end

module type RewCxt =
  sig
    type cxt and visproviso and rewinf
    (* for now, provisosigs are ints *)
    val getprovisos : cxt -> visproviso list * rewinf option
    val setprovisos : cxt -> visproviso list * rewinf option -> cxt
    val getprovisosig : cxt -> int
    val incprovisosig : cxt -> cxt
  end

module RewCxt : RewCxt with type cxt = Type.cxt 
                        and type visproviso = Proviso.visproviso
                        and type rewinf = Rewinf.rewinf
=
  struct
    open Type
    
    type cxt = Type.cxt 
     and visproviso = Proviso.visproviso
     and rewinf = Rewinf.rewinf
     
    (* for now, provisosigs are ints *)
    let getprovisos = fun (Context {provisos = provisos}) -> provisos
    (* this function used by rewrite, which is manipulating the provisosig intelligently *)
    let setprovisos =
      fun (Context cxt) ps ->
        Context {cxt with provisos = ps}
    let getprovisosig =
      fun (Context {provisosig = provisosig}) -> provisosig
    let nextprovisosig = ref 0
    (* at 1000 contexts/sec, this will last 2^30/1000 = 1M seconds.  Long enough (we are
     * certainly not doing 1K contexts/sec, and when we do, we will undoubtedly have 
     * 64-bit desktop machines).
     * RB 14/viii/97
     *)
    let incprovisosig =
      fun (Context cxt) ->
        let bang () =
          raise (Miscellaneous.Catastrophe_ ["STOP, STOP, STOP!!!! too many contexts!!!!"])
        in
        (try incr nextprovisosig with _ -> bang ());
        if !nextprovisosig <= 0 then bang () else ();
        Context {cxt with provisosig = !nextprovisosig}
  end

module type ExteriorFuns =
  sig
    type cxt and exterior and rewinf
     
    val setexterior : cxt -> exterior -> cxt
    val getexterior : cxt -> exterior
    val exteriorinf : cxt -> rewinf option
  end

module ExteriorFuns : ExteriorFuns with type cxt = Type.cxt 
									and type exterior = Type.exterior
									and type rewinf = Rewinf.rewinf
=
  struct
    open Type
    type cxt = Type.cxt 
     and exterior = Type.exterior
     and rewinf = Rewinf.rewinf
     
    let setexterior =
      fun (Context cxt) s -> Context {cxt with outside = s}
    let getexterior = fun (Context {outside = outside}) -> outside
    let exteriorinf =
      function (Context {outside=NoExterior})        -> None
	  |        (Context {outside=Exterior(_,inf,_)}) -> inf
  end
