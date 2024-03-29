(*
    Copyright (C) 2003-19 Richard Bornat & Bernard Sufrin
     
        richard@bornat.me.uk
        sufrin@comlab.ox.ac.uk

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

type seq     = Sequent.seq
 and rewinf  = Rewinf.rewinf
 and element = Termtype.element
 and name    = Name.name
 and cxt     = Cxttype.cxt
 and thing   = Thing.thing
 and proviso = Proviso.proviso
 and term    = Termtype.term
 and vid     = Termtype.vid
 
module type Access =
  sig
    type fmt and path and prooftree
    val followPath : prooftree -> path -> prooftree
    val onestep : prooftree -> path -> (path * prooftree) option
    val pathPrefix : prooftree -> path -> path -> bool
    val findTip : prooftree -> path -> seq
    val getTip : prooftree -> path -> seq * rewinf * fmt
    val allTipPaths : prooftree -> path list
    val allTipConcs : prooftree -> (path * element list) list
    val validelement : bool -> prooftree -> element -> path -> bool
    val validhyp : prooftree -> element -> path -> bool
    val validconc : prooftree -> element -> path -> bool
    val stillopen : prooftree -> path -> bool
    val maxtreeresnum : prooftree -> int
    val isTip : prooftree -> bool
    val hasTip : prooftree -> bool
    val rootPath : prooftree -> path
    val parentPath : prooftree -> path -> path
    val siblingPath : prooftree -> path -> bool -> path
    (* true gives left, false gives right *)
    val subgoalPath : prooftree -> path -> int list -> path
    val reason : (name -> bool) -> prooftree -> string option
    val subtrees : prooftree -> prooftree list
    val sequent : prooftree -> seq
    val rule : prooftree -> name option
    val args : prooftree -> Termtype.term list
    val params : prooftree -> Termtype.term list option
    val stepprovisos : prooftree -> (bool * Proviso.proviso) list option
    val thinned : prooftree -> element list * element list
    val format : prooftree -> fmt
    val raw_depends : name list -> prooftree -> name list
    val depends : prooftree -> name list
    val findAnyGoal : prooftree -> path option
    val findRightwardsGoal : bool -> prooftree -> path -> path option
    val string_of_fmt : fmt -> string
    val string_of_path : path -> string
    val string_of_prooftree : prooftree -> string
    val string_of_proofnode : prooftree -> string
    
    val find: (prooftree -> bool) -> prooftree -> prooftree option
    val fold_left: ('a -> prooftree -> 'a) -> 'a -> prooftree -> 'a
  end

module type Tree =
  sig
    type 'a prooftree and treeformat and fmtpath and visformat and vispath
    
    type prooftree_step =
      | Apply of (name * term list * bool) (* bool is 'isresolution step'. RB 9.iii.2005 *)
      | Given of (string * int * bool)     (* bool is 'isresolution step'. RB 9.iii.2005 *)
      | UnRule of (string   * name list)
                (* step name  rule dependencies *)

    val string_of_prooftree_step : prooftree_step -> string
    val step_label : prooftree_step -> string
    val step_resolve : prooftree_step -> bool
    val mkTip : cxt -> seq -> treeformat -> treeformat prooftree
    val mkJoin :
      cxt -> string -> prooftree_step -> term list -> treeformat -> seq ->
             treeformat prooftree list -> element list * element list ->
             treeformat prooftree
    val get_prooftree_fmt : treeformat prooftree -> fmtpath -> treeformat
    val set_prooftree_fmt :
      treeformat prooftree -> fmtpath -> treeformat -> treeformat prooftree
    val get_prooftree_cutnav :
      treeformat prooftree -> fmtpath -> (int * int) option
    val set_prooftree_cutnav :
      treeformat prooftree -> fmtpath -> (int * int) option ->
        treeformat prooftree
    val rewriteProoftree :
      seq list -> bool -> cxt -> treeformat prooftree ->
        cxt * treeformat prooftree * vid list
    val insertprooftree :
      cxt -> fmtpath -> treeformat prooftree -> treeformat prooftree ->
        fmtpath * treeformat prooftree
    val truncateprooftree :
      cxt -> fmtpath -> treeformat prooftree -> fmtpath * treeformat prooftree
    val replaceTip :
      cxt -> fmtpath -> treeformat prooftree -> seq -> treeformat prooftree
    val makewhole :
      cxt -> (treeformat prooftree * fmtpath) option ->
        treeformat prooftree -> fmtpath -> fmtpath * treeformat prooftree
    val augmenthyps :
      cxt -> treeformat prooftree -> element list -> treeformat prooftree * (bool*bool*term) list
    val deepest_samehyps : treeformat prooftree -> fmtpath -> fmtpath
    val isCutStep : treeformat prooftree -> fmtpath -> bool
    val catelim_prooftree2tactic :
      treeformat prooftree -> proviso list -> seq list -> string list -> string list
    val visproof :
      (name -> bool) -> bool -> bool -> treeformat prooftree ->
        (Termtype.resnum, string) Mappingfuns.mapping * visformat prooftree
    (* showallsteps    proved          hideuselesscuts *)
    val visible_subtrees : bool -> treeformat prooftree -> treeformat prooftree list option
                        (* showallsteps *)
    
    val pathtoviewpath : bool -> treeformat prooftree -> fmtpath -> vispath option
                      (* showallsteps *)
    val viewpathtopath : bool -> treeformat prooftree -> vispath -> fmtpath option
                      (* showallsteps *)

    module Fmttree : Access
                     with type fmt = treeformat 
                      and type prooftree = treeformat prooftree
                      and type path = fmtpath
    module Vistree : Access
                     with type fmt = visformat
                      and type prooftree = visformat prooftree
                      and type path = vispath
    
    val foldedfmt     : string ref  (* LAYOUT "" ()    *)
    val filteredfmt   : string ref  (* LAYOUT "" (...) *)
    val unfilteredfmt : string ref  (* LAYOUT ""       *)
    val rawfmt        : string ref  (* when named layouts are exhausted by doubleclicking *)
    val nohidefmt     : string ref  (* when there's nothing hidden *)
       
    val showallproofsteps : bool ref
    val hideuselesscuts : bool ref
    val prooftreedebug : bool ref
    val prooftreedebugheavy : bool ref
    val prooftreerewinfdebug : bool ref
    val cuthidingdebug : bool ref
    exception FollowPath_ of (string * int list)
    exception FindTip_
    exception AlterProof_ of string list
    val reasonstyle : string ref
    val sayTheorem : bool ref
    val sayDerived : bool ref
    val sayResolve : bool ref
  end

module Tree : Tree with type treeformat = Treeformat.Fmt.treeformat
                    and type fmtpath = Treeformat.Fmt.fmtpath
                    and type visformat = Treeformat.VisFmt.visformat
                    and type vispath = Treeformat.VisFmt.vispath
=
  struct
    open Cxtfuns
    open Cxtstring
    open Cxtexterior
    open Idclass
    open Listfuns
    open Mappingfuns
    open Miscellaneous
    open Name
    open Structurerule
    open Optionfuns
    open Proviso
    open Rewinf
    open Rewrite
    open Rew
    open Sequent
    open Sml
    open Stringfuns
    open Tactictype
    open Tactic
    open Termfuns
    open Termstore
    open Termstring
    open Termtype
    open Thing
    open Treeformat.Fmt
    open Treeformat.VisFmt
    open Treelayout

    type treeformat = Treeformat.Fmt.treeformat
     and fmtpath = Treeformat.Fmt.fmtpath
     and visformat = Treeformat.VisFmt.visformat
     and vispath = Treeformat.VisFmt.vispath
     and rewinf = Rewinf.rewinf
      
    let consolereport = Miscellaneous.consolereport
    let idf = fun x -> x
    
    (* -------------------------------------------------------------------------------- *)
              
    let prooftreedebug = ref false
    let prooftreedebugheavy = ref false
    (* these two affect the translation of proof -> visproof and also path -> vispath.
     * That makes it tricky to deal with the translation of old paths (see displaystyle.sml).
     * As we say: bugger.
     *)
    let showallproofsteps = ref false
    let hideuselesscuts = ref false
    let cuthidingdebug = ref false
    let foldedfmt     = ref "{%s}"  (* LAYOUT "" ()    *)
    and filteredfmt   = ref "%s"    (* LAYOUT "" (...) *)
    and unfilteredfmt = ref "%s"    (* LAYOUT ""       *)
    and rawfmt = ref "[%s]"         (* when named layouts are exhausted by doubleclicking *)
    and nohidefmt = ref "()"        (* when there's nothing hidden *)
       
    type prooftree_step =
      | Apply of (name * term list * bool)
      | Given of (string * int * bool)
      | UnRule of (string * name list)       (* step name, rule dependencies *)

(* the 'hastipval' optimisation, added to stop exponential behaviour when traversing the tree, 
* may or may not be an optimisation ...
*)
    type 'a prooftree =
      | Tip  of (seq * rewinf * 'a)
      | Join of 'a joinrec
      
    and 'a joinrec = {why       : string;
                      how       : prooftree_step;
                      cutnav    : (int * int) option;
                      args      : (term list * rewinf);
                      fmt       : 'a;
                      hastipval : bool;
                      seq       : (seq * rewinf);
                      trs       : ('a prooftree list * rewinf);
                      thins     : ((element list * element list) * rewinf)
                   }
                   (* thins: resources consumed - elements that 
                      matched hypotheses and conclusions
                      when the rule was applied.  This information is 
                      essential for the boxdraw display module; it 
                      isn't possible to get it from the rule and its 
                      arguments (and now it's used by
                      DischargeProviso RB 17/08/20)
                    *) 

(* some functions to extract things from trees, because otherwise I get confused. RB *)

    let join_args j = fst j.args
    let join_hastip j = j.hastipval
    let join_seq j = fst j.seq
    let join_subtrees j = fst j.trs
    let join_subtrees_rewinf j = snd j.trs
    let join_thinned j = fst j.thins
    
    let withsubtrees j ts = {j with trs=ts}
    let withhastipval j hastipval = {j with hastipval=hastipval}
    let withfmt j fmt = {j with fmt=fmt}
      
    let step_label = function
      | Apply (n, _, _) -> parseablestring_of_name n
      | Given (s, _, _) -> s
      | UnRule (s, _)   -> s
    let step_resolve = function
      | Apply (_, _, b) -> b
      | Given (_, _, b) -> b
      | _               -> false
    let rewinfProoftree = function
      | Tip (seq, rewinf, fmt) -> rewinf
      | Join j                 -> List.fold_left (curry2 rewinf_merge) 
                                                      nullrewinf 
                                                      [snd j.args; snd j.seq; snd j.trs; snd j.thins]
    let tip_seq (seq, rewinf, hastipval) = seq
    let joinopt f = function 
      | Tip _  -> None
      | Join j -> Some (f j)
    let howrule = function 
      | Apply (r, _, _) -> Some r
      | _               -> None
    let howparams = function 
      | Apply (_, ps, _) -> Some ps
      | _                -> None
    let rule = function 
      | Tip _  -> None
      | Join j -> howrule j.how
    let params = function
      | Tip _  -> None
      | Join j -> howparams j.how
    let args = function 
      | Tip _  -> []
      | Join j -> join_args j
    let ruleprovisos name = 
      match thinginfo name with
      | Some (Rule ((_, provisos, _, _), _), _) -> provisos
      | Some (Theorem (_, provisos, _), _)      -> provisos
      | _                                       -> 
          raise (Catastrophe_ ["prooftree.ruleprovisos can't find thing named ";
                               string_of_name name])
    let howprovisos = howrule &~ (_Some <.> ruleprovisos)
    let stepprovisos = rule &~ (_Some <.> ruleprovisos)
    let thinned = function 
      | Join j -> join_thinned j
      | _      -> [], []
    let isTip = function
      | Tip _ -> true
      | _ -> false
    let hasTip = function
      | Tip _  -> true
      | Join j -> join_hastip j
    let tshaveTip ts =
      nj_fold (fun (a, b) -> a || b) ((hasTip <* ts)) false
    let subtrees = function
      | Tip _  -> []
      | Join j -> join_subtrees j
    let sequent = function
      | Tip t -> tip_seq t
      | Join j -> join_seq j
    let rec format = function
      | Tip (_, _, fmt) -> fmt
      | Join j          -> j.fmt


    (* -------------------------- generic tree traversal -------------------------- *)
    
    (* when I developed Jape, I didn't have much understanding (shame on me!) of 
       abstract typing in Caml. There are lots of things that could use generic 
       tree traversal, and perhaps the refactoring me will do more of it. For the moment
       I need a fold and a find. And yes, I'm going to be brave and call them fold and 
       find. The fold is of course a fold_left, so I'll call it that.
       RB 17/09/20
     *)
     
     let rec fold_left f z t =
       let z' = f z t in
       match t with
       | Join j -> List.fold_left (fold_left f) z' (join_subtrees j)
       | Tip  _ -> z'

    let rec find p t =
      if p t then Some t else
      match t with
      | Tip  _ -> None
      | Join j -> Optionfuns.findfirst (find p) (join_subtrees j)
      
    (* and something designed for fold_left *)
    let raw_depends ds = function
      | Tip  _ -> ds
      | Join j -> match j.how with
                  | UnRule (_, ss)  -> ss @ ds
                  | Apply (n, _, _) -> n :: ds
                  | Given _         -> ds
    
    let depends tree = sortunique nameorder (fold_left raw_depends [] tree)
    
    (* -------------------------- navigation with int lists -------------------------- *)
        
    exception FollowPath_ of (string * int list) 
    exception FindTip_
    
    (* A new means of addressing the tree, to serve the needs of the CUTIN tactic, which
     * inserts cuts low down in a tree.
     *
     * A cut inserted into a tree produces two new nodes, and wrecks all paths to nodes
     * above it, if we use the old nth-subtree way of describing a path. But it is essential
     * that old paths continue to work -- think of goals, targets, paths stored by
     * LETGOALPATH, and maybe other things as well -- so we can't use that old navigation system.
     *
     * Luckily a cut introduces exactly two unique elements into a proof: the conclusion element
     * and the hypothesis element. Their resnums can be used to label the two new nodes uniquely. If
     * we include them in paths, they have to be negative numbers.  I rely on the fact that 
     * resources are never zero, so that addresses of the two new nodes are just negative numbers.
     *
     * Then there is a choice: which are the two new nodes?  The apparently obvious choice is
     * to label the left subtree with the conclusion resource, the right with the hypothesis
     * resource.  But that doesn't work, because if we do a CUTIN at a tip, having recorded
     * the path to the tip for some reason, the old path would lead to the root of the internal
     * cut, not the reconstructed tip.  So the conclusion resource labels the left subtree, 
     * and the hypothesis resource labels the cut node itself.  Odd, but (given considerable 
     * ingenuity in the design of algorithms) it works.  Drawback: it makes comparison of paths
     * (e.g. by looking at prefixes) almost impossible.  Sorry.
     * 
     * To make old paths work, we skip to the right subtree if we are addressing
     * an inner cut node with a path that doesn't start with the right or left subtree label.
     *
     * RB 19/i/00
     *)
     
    type 'a navkind =
      | NormalNav of 'a prooftree list
      | CutNav of (int * int * 'a prooftree * 'a prooftree)
    
    let decode_cutnav j =
      match j.cutnav, j.trs with
      | Some (l, r), ([tl; tr], rewinf) -> CutNav (l, r, tl, tr)
      | _          , (ts, rewinf)       -> NormalNav ts
    
    (* I've tried to make this fast *)
    let rec joinstep go stop skip j path =
      match j.cutnav, j.trs with
      | Some (l, r), ([tl; tr], _) ->
          (match path with
           | n :: ns ->
               if n = l then go n tl ns
               else if n = r then
                 if null ns then stop () else joinstep go stop skip j ns
               else skip (l, r, tl, tr) path
           | [] -> skip (l, r, tl, tr) path
          )
      |_, (ts, _) ->
          (* this is what makes the tip path work *)
          match path with
          | n :: ns -> go n (try Listfuns.guardednth ts n with
                             | Listfuns.Bad_nth -> raise (FollowPath_ ("out of range", path))
                            ) ns
          | []      -> stop ()
    
    let pathto t =
      match t with
      | Join {cutnav=Some(l,r);} -> [r]
      | _                        -> []
    
    let rec followPath_ns t ns =
      match t with
      | Tip _ -> if null ns then t else raise (FollowPath_ ("at tip", ns))
      | Join j ->
          joinstep (fun _ -> followPath_ns) (fun _ -> t)
            (fun (l, r, tl, tr) -> followPath_ns tr) j ns
    
    let onestep_ns t path =
      match t with
      | Tip _ ->
          if null path then None
          else raise (FollowPath_ ("onestep at tip", path))
      | Join {cutnav=Some (l, r); trs=([tl; tr], _);} ->
          (match path with
           | n :: ns -> if n = l then Some (ns, tl)
                        else if n = r then Some (ns, tr)
                        else Some (path, tr)
           | []      -> Some (path, tr)
          )
      | Join {trs=(ts,_)} ->
          match path with
          | n :: ns -> Some (ns, (try Listfuns.guardednth ts n with
                                  | Listfuns.Bad_nth -> raise (FollowPath_ ("onestep out of range", path))
                                 )
                            )
          | [] -> None
    
    let rec fakePath_ns rf t ns =
      match t with
      | Tip _ ->
          if null ns then List.rev rf else raise (FollowPath_ ("at tip", ns))
      | Join j ->
          joinstep (fun n -> fakePath_ns (n :: rf)) (fun _ -> List.rev rf)
            (fun (l, r, tl, tr) -> fakePath_ns (r :: rf) tr) j ns
    
    let getTip_ns t ns =
      match followPath_ns t ns with
      | Tip t -> t
      | _     -> raise FindTip_
    
    let findTip_ns t p = (fun (s,_,_)->s) (getTip_ns t p)
    
    let allTips_ns t =
      let rec moretips a1 a2 a3 =
        match a1, a2, a3 with
        | ns, (Tip _ as t), tips -> (List.rev ns, t) :: tips
        | ns, Join j, tips ->
            match decode_cutnav j with
            | NormalNav ts ->
                nj_fold (fun ((n, t), tips) -> moretips (n :: ns) t tips)
                  (numbered ts) tips
            | CutNav (l, r, tl, tr) ->
                moretips (l :: ns) tl (moretips ns tr tips)
      in
      moretips [] t []
    
    let allTipPaths_ns t = (fst <* allTips_ns t)
    
    let allTipConcs_ns t =
         (function
          | ns, Tip (Seq (_, hs, gs), _, _) -> ns, explodeCollection gs
          | _                               -> 
              raise (Catastrophe_ ["allTips gave non-Tip in allTipConcs_ns"])
         ) <* allTips_ns t
    (* functions which look for a path *)
    
    let search opt n = (opt &~~ (fun ns -> Some (n :: ns)))
    
    let rec _F =
      function
      | [] -> None
      | (n, t) :: nts -> (search (_G t) n |~~ (fun _ -> _F nts))
    and _G t =
      match t with
      | Tip _ -> Some []
      | Join j ->
          match decode_cutnav j with
          | NormalNav ts -> _F (numbered ts)
          | CutNav (l, r, tl, tr) -> (search (_G tl) l |~~ (fun _ -> _G tr))
    
    let findAnyGoal_ns = _G
    
    let rec findRightwardsGoal_ns skip t path =
      match t with
      | Tip _  -> if skip || not (null path) then None else Some []
      | Join j ->
          let rec go n subt ns =
            (search (findRightwardsGoal_ns skip subt ns) n |~~
               (fun _ ->
                  match decode_cutnav j with
                  | NormalNav ts -> _F (drop (n + 1) (numbered ts))
                  | CutNav (l, r, tl, tr) -> _G tr))
          in
          joinstep go (fun _ -> _G t)
            (fun (l, r, tl, tr) -> findRightwardsGoal_ns skip tr) j path
    
    (* Find the deepest parent of a particular node which has the same hypotheses.
     * I check on the way back down, because it is more efficient, and actually it is the only right thing to do!
     * Note that a FRESH proviso in effect introduces a var hypothesis, so we don't go past a rule which has a
     * FRESH proviso, even though we have the same hypotheses.
     *
     * At least that's what I _think_ it's doing. RB 9.iii.2005
     *)
    (* new subtlety. We don't want to cut back so far as to expose a hidden step. I
       know this is a sort of temporary thing, and perhaps we need a new mechanism to deal with
       it, but for now this is what I'm doing. RB 29/09/20
     *)
    (* and now augmenthyps actually deals with FRESH provisos, because of the way 
       that a hypothesis selection not-at-a-tip works. So there's no need (?) to steer clear of 
       FRESH provisos. RB 05/10/20
     *)
    let rec deepest_samehyps tree (FmtPath ks) =
      (* a strange recursion. shs goes to the end of path to find hs, and gives an empty path to that subtree.
         On the way back, if hs are the same, and it isn't a FRESH proviso node, it gives the same hs and an empty path.
         Otherwise it gives back None, and a path to the previously-chosen node -- and the None stops the search for a node,
         because lower nodes just add their portion of the path.
         The new subtlety means that if a node looks good, but is hidden, take the previously-chosen path, but don't
         stop looking at lower nodes.
       *)
      let rec shs t path =
        let rec topres () =
          let (Seq (_, hs, _)) = sequent t in Some hs, pathto t
        in
        let rec check (hopt, ns) f =
          match hopt with
          | None    -> None, f ns
          | Some hs ->
              let (Seq (_, hs', _)) = sequent t in
              if (match hs, hs' with
                  | Collection (_, BagClass FormulaClass, es),
                    Collection (_, BagClass FormulaClass, es') ->
                      null (listsub sameresource es es')
                  | _ -> hs = hs') 
                 (* not needed: see above. RB 05/10/20
                    &&
                    (match stepprovisos t with
                     | Some provisos -> not (List.exists (isFreshProviso <.> snd) provisos)
                     | None          -> true
                    )
                  *)
              then Some hs, (match format t with
                             | TreeFormat(HideRootFormat,_,_)
                             | TreeFormat(HideCutFormat,_,_)   -> f ns      (* we don't want this one -- take previous *)
                             | _                               -> pathto t  (* this'll do *)
                            )
              else None   , f ns
        in
        let rec go n subt ns = check (shs subt ns) (fun ns -> n :: ns) in
        let rec skip (l, r, tl, tr) ns = check (shs tr ns) idf in
          match t with
        | Tip _  -> if null path then topres () 
                    else raise (FollowPath_ ("overflow in deepest_samehyps", path))
        | Join j -> joinstep go topres skip j path
        in
        FmtPath (snd (shs tree ks))
    
    let rec rootPath_ns t = pathto t
    
    let rec parentPath_ns t path =
      let rec f t ns =
        match t with
        | Tip _ ->
            if null ns then None
            else raise (FollowPath_ ("overflow in parentPath", path))
        | Join j ->
            let rec me _ = Some (pathto t) in
            let rec go n t ns = (search (f t ns) n |~~ me) in
            let rec skip (l, r, tl, tr) ns = (f tr ns |~~ me) in
            joinstep go (fun _ -> None) skip j ns
      in
      match f t path with
      | Some res -> res
      | None -> raise (FollowPath_ ("parentPath of root", path))
    
    let rec siblingPath_ns t path left =
      let rec badLeft () =
        raise (FollowPath_ ("left sibling of leftmost", path))
      in
      let rec badRight () =
        raise (FollowPath_ ("right sibling of rightmost", path))
      in
      let rec f t ns =
        match t with
        | Tip _ ->
            if null ns then None
            else raise (FollowPath_ ("overflow in siblingPath", path))
        | Join j ->
            let rec go n t ns =
              (search (f t ns) n |~~
                 (fun _ ->
                    match decode_cutnav j with
                    | NormalNav ts ->
                        if left then
                          if n = 0 then badLeft () else Some [n - 1]
                        else if n + 1 >= List.length ts then badRight ()
                        else Some [n + 1]
                    | CutNav (l, r, tl, tr) ->
                        if left then badLeft () else Some (pathto tr)))
            in
            let rec skip (l, r, tl, tr) ns =
              (f tr ns |~~ (fun _ -> if left then Some [l] else badRight ()))
            in
            joinstep go (fun _ -> None) skip j ns
      in
      match f t path with
      | Some res -> res
      | None -> raise (FollowPath_ ("siblingPath of root", path))
    
    (* for subgoalPath we could use path@extras but I don't like leaving cutroot addresses in paths *)
    let rec subgoalPath_ns t path extras =
      let rec f t ns =
        match t with
        | Tip _ ->
            if not (null ns) then
              raise (FollowPath_ ("overflow in subgoalPath", path))
            else if not (null extras) then
              raise (FollowPath_ ("subgoal of Tip", path))
            else pathto t
        | Join j ->
            let go n t ns = n :: f t ns in
            let skip (l, r, tl, tr) ns = f tr ns in
            joinstep go (fun _ -> if null extras then pathto t else subgoalPath_ns t extras [])
                     skip j ns
      in
      f t path
    
    (* Hypothesis/conclusion el occurs at position ns;
     * check that it's still there at position ns'.
     * ns must be a prefix of ns', but because resources work the way they do, 
     * I don't check that.  
     * (Which is lucky, given that paths just got more complicated. RB 18/i/00)
     * (But oh dear, since we need to combine hypothesis clicks, we need to know which is a prefix of what! RB 25/vii/00)
     *)
    let rec validelement_ns ishyp proof el ns =
      (* because of resource identity we can do this thing really quickly! *)
      (* nj 109.19 complains if this next thing is written as a val *)
      let rec isin es = List.exists (fun e' -> sameresource (el, e')) es in
      let rec elements tree =
        let (Seq (_, hs, cs)) =
          match tree with
          | Join j -> join_seq j
          | Tip t -> tip_seq t
        in
        explodeCollection (if ishyp then hs else cs)
      in
      try isin (elements (followPath_ns proof ns)) with
      | _ -> false
    let validhyp_ns = validelement_ns true
    let validconc_ns = validelement_ns false
    let rec stillopen_ns proof ns =
      try
        match followPath_ns proof ns with
        | Tip _ -> true
        | _ -> false
      with
      | _ -> false
    
    (* -------------------------- printing proof trees -------------------------- *)
        
    let shyidforFORMULAE = "FORMULAE"
    let shyidforFROM     = "FROM"
    let shyidforINFER    = "INFER"
    let shyidforIS       = "IS"
    let shyidforWHERE    = "WHERE"
    
    let givenssep        = " AND "
    let provisosep       = " AND "
    
    let rec string_of_prooftree_step = function
      | Apply a ->
          "Apply" ^
            string_of_triple parseablestring_of_name string_of_termlist string_of_bool "," a
      | Given g ->
          "Given" ^
            string_of_triple idf string_of_int string_of_bool "," g
      | UnRule u ->
          "Unrule" ^
            string_of_pair idf (bracketed_string_of_list parseablestring_of_name ",") "," u
    
    let string_of_ns = bracketed_string_of_list string_of_int ","
    
    let string_of_Join tlf subtreesf j =
      implode
        ["Join{"; "reason="; j.why; "; how="; string_of_prooftree_step j.how;
         "; cutnav="; string_of_option (string_of_pair string_of_int string_of_int ",") j.cutnav;
         "; args="; string_of_pair string_of_termlist string_of_rewinf ", " j.args;
         "; fmt="; tlf j.fmt; 
         "; hastipval="; string_of_bool j.hastipval;
         "; seq="; string_of_pair elementstring_of_seq string_of_rewinf ", " j.seq;
         "; subtrees="; string_of_pair subtreesf string_of_rewinf ", " j.trs; 
         "; j.thins=";
         (let string_of_ths =
            bracketed_string_of_list (debugstring_of_element string_of_term) ", "
          in
          string_of_pair (string_of_pair string_of_ths string_of_ths ", ") string_of_rewinf ", "
                         j.thins
         );
         "}"
        ]
    
    let string_of_tip tlf t = "Tip" ^ string_of_triple elementstring_of_seq string_of_rewinf tlf ", " t
    
    let string_of_proofnode tlf node =
      match node with
      | Tip t  -> string_of_tip tlf t
      | Join j -> string_of_Join tlf (fun _ -> "...") j
      
    let rec string_of_prooftree tlf t =
      let rec pft tlf rp t =
        (string_of_ns (pathto t @ List.rev rp) ^ " = ") ^
          (match t with
           | Tip t -> string_of_tip tlf t
           | Join j ->
               implode
                 (interpolate "\n"
                    (string_of_Join tlf (fun _ -> "... see below ...") j ::
                       (match decode_cutnav j with
                        | NormalNav ts ->
                              ((fun (i, t) -> pft tlf (i :: rp) t) <*
                               numbered ts)
                        | CutNav (l, r, tl, tr) ->
                            [pft tlf (l :: rp) tl; pft tlf rp tr]))))
      in
      pft tlf [] t
    
    exception Can'tHash_
    (* moved outside for OCaml *)
       
    let mkUnRuleTac u =
      match u with
      | ("FLATTEN", [t]) -> AssocFlatTac t
      | ("EVALUATE", ts) -> EvalTac ts
      | _ -> raise (Catastrophe_
               ["mkUnRuleTac given "; 
                string_of_pair 
                  idf (bracketed_string_of_list string_of_term ",") "," u
               ])
    
    let step_argmap ps args =
      (* this is one of a pair of TEMPORARY hacks, caused by an inability to treat proof replay
         as identity matching (current culprit, the fact that Given doesn't have any recorded
         collection arguments, but it may go deeper).  We strip BagFormula arguments in
         additiveLeft mode, because they make proof checking disastrously non-linear.
         
         The other one of the pair (see tactic.ml) strips the corresponding arguments when the 
         proof tactic is read.
         
         This is so successful :-) that it might become a permanent hack.
       *)
      (* the mystery continues: Given has to be an absolute match ... *)
      let argmap = 
        try ps|||args with Zip_ -> raise (Catastrophe_ ["params and args won't zip in prooftree.step_argmap"])
      in
      if !autoAdditiveLeft then
        (match split (function (_,Collection _) -> true | _ -> false) argmap with
         | ([_], oks) -> (* if not (List.exists (existsterm isUnknown) args) then oks else argmap *)
                         oks (* it can't be slower, even if it contains unknowns ... *)
         | _          -> argmap)
      else argmap
      
    let rec catelim_prooftree2tactic tree provisos givens tail =
      (* a proof as an executable tactic *)
      let seq = sequent tree in
      let rec thisone j =
        let res b t = if b then ResolveTac t else t in
        match j.how with
        | Apply (n, ps, b) -> res b (SubstTac (n, step_argmap ps (join_args j)))
        | Given (_, i, b)  -> res b (GivenTac (term_of_int i))
        | UnRule (r, _)    -> mkUnRuleTac (r, join_args j)
      in
      let rec traverse =
        function
        | Tip t, ts -> NextgoalTac :: ts
        | Join j, ts ->
            let rec tr ts = thisone j :: nj_fold traverse (join_subtrees j) ts in
            match layouts_of_format (j.fmt) with
            | [] -> tr ts
            | ls ->
                nj_fold (fun (l, t) -> LayoutTac (t, l)) ls
                  (SeqTac (tr [])) ::
                  ts
      in
      let rec hashables =
        function
        | Tip _ , hs -> hs
        | Join j, hs ->
            let rec catalogue =
              function
              | Collection (_, _, els), hs ->
                  nj_fold
                    (function
                     | Element (_, _, t), hs -> catalogue (t, hs)
                     | _, hs -> hs)
                    els hs
              | arg, hs ->
                  match hashterm arg with
                  | Some h -> arg :: hs
                  | None   ->
                      if existsterm (function | Literal (_, Number _) -> true | _ -> false) arg
                      then raise Can'tHash_
                      else hs
            in
            nj_fold hashables (join_subtrees j) 
                              (match j.how with
                               | Apply (_, ps, _) -> nj_fold catalogue (join_args j) hs
                               | _                -> hs
                              )
      in
      let rec proof tail =
        try
          let count = ref 0 in
          let rec intorder (i, _) (j, _) = i < j in
          let (lookup, reset) =
            (* terms in the tree get mapped to numbers, so that it can be read in
               more quickly.
             *)
            let module Cache =
              struct
                open Hashtbl
                module Store = Make (struct
                                       type t = term
                                       let equal = (=)
                                       let hash = _The <.> hashterm
                                     end
                                    )
                let store = Store.create 17 (* it will grow *)
                let lookup t = 
                  try Store.find store t
                  with Not_found -> let r = !count in
                                    incr count; Store.add store t r; r
                let reset () = Store.clear store
              end
            in Cache.lookup, Cache.reset
          in
          let hs = try sortunique (<) (hashables (tree, [])) with Can'tHash_ -> []
          in
          let body =
            if null hs then () else showargasint := Some lookup;
            let r = "\n" :: shyidforIS :: "\n" ::
                    catelim_string_of_tactic (SeqTac (traverse (tree, [])))
                      ("\n" :: tail) 
            in showargasint := None; r
          in
          let hashables_body =
            if null hs then body
            else
              "\n" :: shyidforFORMULAE :: "\n" ::
                catelim_string_of_list
                  (fun (i, t) tail ->
                     string_of_int i :: " " :: catelim_string_of_term (comma_enbracket t) tail)
                  ",\n"
                  (sortunique intorder ((fun t -> lookup t, t) <* hs))
                  body
          in
          "\n" :: Paragraph.catelim_string_of_rulebody "\n" provisos givens seq hashables_body
        with
        | exn -> showargasint := None; raise exn
      in
      proof tail
    
    (* -------------------------- rewriting proof trees -------------------------- *)
        
    let rec rew_thins cxt = rew_Pair (rew_elements true cxt)
    let rec getrawinfList rawf (xs, z) = nj_fold rawf xs z
    let rec getrawinfPair rawf1 rawf2 ((x1, x2), z) = rawf1 (x1, rawf2 (x2, z))
    let rec getrewinfList rawf xs =
      raw2rew_ (getrawinfList rawf (xs, nullrawinf))
    let rec getrewinfPair rawf1 rawf2 pair =
      raw2rew_ (getrawinfPair rawf1 rawf2 (pair, nullrawinf))
    let rec getrewinfProoftreeList ts =
      nj_fold rewinf_merge (List.map rewinfProoftree ts) nullrewinf
    let prooftreerewinfdebug = ref false
    
    (* if we just rewrite a couple of unknowns in a huge sequent, getrewinfSeq is a very
     * expensive way (it turns out) of assessing the state of the new sequent.  In general
     * the rewinf is much smaller than the sequent.  The same applies elsewhere.
     *)
    let rec updaterewinf cxt rewinf =
      let (vars, uVIDs, badres, psig) = rawinf_of_rew rewinf in
      let changedres =
        nj_fold
          (fun (i, irs) ->
             match rew_resnum cxt (ResUnknown i) with
             | None -> irs
             | Some r -> (i, r) :: irs)
          badres []
      in
      let changeduts =
        nj_fold
          (fun (u, uts) ->
             match (varmap cxt <@> u) with
             | None -> uts
             | Some t -> (u, rewrite cxt t) :: uts)
          uVIDs []
      in
      let addedinf =
        getrewinfList (rawinfTerm cxt) ((snd <* changeduts))
      in
      let changedus = (fst <* changeduts) in
      let vars' =
        sortedlistsub
          (function
           | Unknown (_, u, _), u' -> u = u'
           | _ -> false)
          vars changedus
      in
      let uVIDs' = sortedlistsub (fun (x, y) -> x = y) uVIDs changedus in
      let badres' =
        sortedlistsub (fun (x, y) -> x = y) badres
          ((fst <* changedres)) @
          nj_fold
            (function
             | ResUnknown i, is -> i :: is
             | _, is -> is)
            ((snd <* changedres)) []
      in
      rewinf_merge
        (raw2rew_ (mkrawinf (vars', uVIDs', badres', psig)), addedinf)
    let rec rew_stuff cxt rew (x, inf) =
      match rew cxt x with
      | Some x -> x, updaterewinf cxt inf
      | None   -> x, inf
    let rec getrewinfSeq s cxt seq =
      if !prooftreerewinfdebug then
        consolereport ["getrewinfSeq "; s; " "; string_of_seq seq];
      raw2rew_ (rawinfSeq cxt (seq, nullrawinf))
    let getrewinfargs cxt = getrewinfList (rawinfTerm cxt)
    let getrewinfthins cxt = getrewinfPair (rawinfElements cxt) (rawinfElements cxt)
    let getrewinfels cxt els = raw2rew_ (rawinfElements cxt (els, nullrawinf))
    let rec rew_Prooftree a1 a2 =
      match a1, a2 with
      | cxt, (Tip (seq, rewinf, fmt) (* as t *)) ->
          if rew_worthwhile true cxt rewinf then
            begin
              if !prooftreerewinfdebug then
                consolereport
                  ["rewriting tip "; string_of_seq seq; "; ";
                   string_of_rewinf rewinf];
              match rew_Seq true cxt seq with
              | Some seq -> Some (Tip (seq, updaterewinf cxt rewinf, fmt))
              | _ -> None
            end
          else None
      | cxt,
        Join j ->
          let (args, argsinf) = j.args in
          let (seq, seqinf) = j.seq in
          let (ts, tsinf) = j.trs in
          let (thins, thinsinf) = j.thins in
          let ra = rew_worthwhile true cxt argsinf in
          let rs = rew_worthwhile true cxt seqinf in
          let rts = rew_worthwhile true cxt tsinf in
          let rthins = rew_worthwhile true cxt thinsinf in
          if ra || rs || rts || rthins then
            (* let rec fst (x, _) = x in *)
            let a =
              if ra then
                rew_stuff cxt
                  (fun cxt -> option_rewritelist (rew_Term true cxt)) j.args
              else j.args
            in
            let s =
              if rs then
                begin
                  if !prooftreerewinfdebug then
                    consolereport
                      ["rewriting join "; string_of_seq seq; "; ";
                       string_of_rewinf seqinf];
                  rew_stuff cxt (rew_Seq true) j.seq
                end
              else j.seq
            in
            let t =
              if rts then
                rew_stuff cxt
                  (fun cxt -> option_rewritelist (rew_Prooftree cxt)) j.trs
              else j.trs
            in
            let thins = if rthins then rew_stuff cxt rew_thins j.thins else j.thins in
            Some (Join {j with args=a; seq=s; trs=t; thins=thins})
          else None
    
    let rec rewriteProoftree givens grounded cxt tree =
      let cxt = rewritecxt cxt in
      (* desperation ... *)
      let _ =
        if !prooftreedebug then
          consolereport
            ["before rewrite prooftree is ";
             string_of_prooftree string_of_treeformat tree]
      in
      (* ... end desperation *)
      let tree = anyway (rew_Prooftree cxt) tree in
      let inf = rewinfProoftree tree in
      let _ =
        if !prooftreedebug then
          consolereport
            ["context is "; string_of_cxt cxt; "\nprooftree is ";
             string_of_prooftree string_of_treeformat tree]
      in
      let _ =
        if not (null (rewinf_badres inf)) then
          raise
            (Catastrophe_
               ["rewriteProoftree found ResUnknowns ";
                bracketed_string_of_list string_of_int ", " (rewinf_badres inf)])
      in
      (* don't forget the givens when considering grounded provisos *)
      let tvars = tmerge (rewinf_vars inf) (match exteriorinf cxt with
                                            | Some ri -> rewinf_vars ri
                                            | None    -> [])
      in
      let cxt = match
                  if grounded then groundedprovisos tvars (provisos cxt) else None
                with
                | Some ps -> anyway rew_cxt (withprovisos cxt ps)
                | None    -> anyway rew_cxt cxt
      in
      let cinf = rewinfCxt cxt in
      (* vars only in givens are in the exterior of the proof, but they aren't thereby 'in use' *)
      let gvars = nj_fold (uncurry2 tmerge) ((seqvars termvars tmerge <* givens)) [] in
      let cvars = sorteddiff earliervar (rewinf_vars cinf) gvars in
      (* val _ =
        consolereport ["gvars are ", string_of_termlist gvars, 
                       "; rewinf_vars cinf are ", string_of_termlist (rewinf_vars cinf), 
                       "; cvars are ", string_of_termlist cvars,
                       "; rewinf_vars inf are ", string_of_termlist (rewinf_vars inf)]
       *)
      let usedVIDs =
        orderVIDs (vid_of_var <* mergevars (rewinf_vars inf) cvars)
      in
      withusedVIDs (withresmap (withvarmap cxt empty) empty) usedVIDs, tree, usedVIDs
    
    (* -------------------------- Tree construction : only for treeformat prooftrees ----------------------------- *)
        
        (* There is an important subtlety in the design of mkTip.
         * It is necessary to record the uVIDs which occur at each
         * node, and at the same time to record if there are any substitutions which might
         * be reduced.  It is important, however, *not* to rewrite the tip sequent at this
         * point, else we can't construct tips which contain 'stable substitutions' (see
         * the WITHSUBSTSEL stuff).  So the cxt which is provided as an argument must be
         * one with which the tip sequent has already been rewritten; use dont_rewrite_with_this
         * if you want a neutral reading.
         * RB 10/x/96
         *)
        
    let rec mkTip cxt seq fmt = Tip (seq, getrewinfSeq "mkTip" cxt seq, fmt)
    (* I can see no reason why mkJoin ought not to rewrite away - it will save 
     * work in rewriteProoftree later.  Hope I'm not wrong ...
     * RB 10/x/96
     *)
    let rec mkJoin cxt why how args fmt seq tops thins =
      let args = (rewrite cxt <* args) in
      let seq = rewriteseq cxt seq in
      let thins = anyway (rew_thins cxt) thins in
      Join {why         = why; 
            how         = how; 
            cutnav      = None;
            args        = (args, getrewinfargs cxt args); 
            fmt         = fmt; 
            hastipval   = tshaveTip tops;
            seq         = (seq, getrewinfSeq "mkJoin" cxt seq);
            trs         = (tops, getrewinfProoftreeList tops);
            thins       = (thins, getrewinfthins cxt thins)
           }
    
    exception AlterProof_ of string list
    
    let rec applytosubtree_ns path t f =
      let ans () =
        let (infchanged, t') = f t in infchanged, pathto t', t'
      in
      match t with
      | Tip _   -> if null path then ans ()
                   else raise (AlterProof_ ["path extends beyond tip"])
      | Join j -> let res infchanged ns subts =
                    let subinf =
                      if infchanged then getrewinfProoftreeList subts
                      else join_subtrees_rewinf j
                    in
                    let t' =
                      Join (withhastipval (withsubtrees j (subts, subinf)) (tshaveTip subts))
                    in
                    let nextinf = infchanged && subinf <> join_subtrees_rewinf j in
                    nextinf, ns, t'
                  in
                  let go n subt ns =
                    let (infchanged, ns, subt) = applytosubtree_ns ns subt f in
                    let subts =
                      match decode_cutnav j with
                       | NormalNav sts -> take n sts @ subt :: drop (n + 1) sts
                      | CutNav (l, r, tl, tr) -> [subt; tr]
                    in
                    res infchanged (n :: ns) subts
                  in
                  let skip (l, r, tl, tr) ns =
                    let (infchanged, ns, subt) = applytosubtree_ns ns tr f in
                    res infchanged ns [tl; subt]
                  in
                  joinstep go ans skip j path
    
    let rec get_prooftree_fmt tree (FmtPath ns) = format (followPath_ns tree ns)
    
    let rec set_prooftree_fmt tree (FmtPath ns) fmt' =
      if !prooftreedebug then
        consolereport ["setting format "; string_of_treeformat fmt'; " at "; string_of_ns ns];
      thrd (applytosubtree_ns ns tree
              (function
               | Join j                 -> false, Join {j with fmt=fmt'}
               | Tip (seq, rewinf, fmt) -> false, Tip (seq, rewinf, fmt'))
              )
    
    let rec get_prooftree_cutnav tree (FmtPath ns) =
      match followPath_ns tree ns with
      | Join j -> j.cutnav
      | Tip _ -> raise FindTip_ (* or should it be FollowPath_ ? *)

    let rec set_prooftree_cutnav tree (FmtPath ns) cutnav =
      thrd (applytosubtree_ns ns tree
              (function
               | Join j -> if match cutnav, j.trs with
                              | None  , _           -> true
                              | Some _, ([_; _], _) -> true
                              | _                   -> false
                           then
                             false, Join {j with cutnav=cutnav}
                           else
                             raise (AlterProof_ ["cutnav applied to obviously non-cut node"])
               | Tip _ -> raise (AlterProof_ ["cutnav applied to tip"])))
    
    let rec truncateprooftree cxt (FmtPath ns) tree =
      let (_, ns, tree) =
        applytosubtree_ns ns tree
          (function
           | Join j -> let t' = mkTip cxt (rewriteseq cxt (join_seq j)) (j.fmt) in
                       true, t'
           | tip -> false, tip)
      in
      FmtPath ns, tree
    
    let rec insertprooftree cxt (FmtPath ns) tree subtree =
      let (_, ns, tree) =
        applytosubtree_ns ns tree
          (fun inspoint ->
             if eqseqs (rewriteseq cxt (sequent inspoint),
                        rewriteseq cxt (sequent subtree))
             then
               let t' = set_prooftree_fmt subtree (FmtPath (rootPath_ns subtree))
                                                  (treeformatmerge (format inspoint, format subtree))
               in
               true, t'
             else
               raise (AlterProof_ ["sequents not equal (insertprooftree) -- ";
                                   string_of_seq (sequent tree); " => ";
                                   string_of_seq (anyway (rew_Seq true cxt) (sequent tree));
                                   " -- "; string_of_seq (sequent subtree); " => ";
                                   string_of_seq
                                     (anyway (rew_Seq true cxt) (sequent subtree))]))
      in
      FmtPath ns, tree
    
    (* the same comment as in mkTip, about contexts and rewriting, applies to replaceTip *)
    (* this looks a bit dangerous ... I guess I've used it carefully *)
    (* I've made it check that it doesn't change hypotheses *)
    let rec replaceTip cxt (FmtPath ns) tree seq' =
      let (_, ns, tree) =
        applytosubtree_ns ns tree
          (function
           | Tip (seq, _, fmt) -> 
               let hs = seq_lefts seq in
               let hs' = seq_lefts seq' in
               if null (listsub sameresource hs hs') && null (listsub sameresource hs' hs) then
                 (let t = mkTip cxt seq' fmt in true, t)
               else raise (Catastrophe_ ["replaceTip "; string_of_seq seq; " => "; string_of_seq seq'])
           | _                      -> 
               raise (AlterProof_ ["replaceTip applied to Join"]))
      in
      tree
    
    let rec makewhole a1 a2 a3 a4 =
      match a1, a2, a3, a4 with
      | cxt, Some (oldtree, (FmtPath oldns as oldpath)), newtree,
        FmtPath newns ->
          let (FmtPath ns, tree) =
            insertprooftree cxt oldpath oldtree newtree
          in
          FmtPath (subgoalPath_ns tree ns newns), tree
      | cxt, None, newtree, newpath -> newpath, newtree
    
    (* this is the most dangerous thing I have tried so far. It must be done ONLY if
     * autoAdditiveLeft is true.  It must not be applied to a subtree which contains
     * a rule that has a FRESH proviso.
     *
     * But (I hadn't noticed) the horror happens now we have true forward steps applied to 
     * a hypothesis without a conclusion. So now we are going to have to generate new
     * provisos from (slumbering) FRESHness in the tree. Ho hum. This function therefore
     * calculates a list of 'fresh in the hypotheses' variables. RB 9.iii.2005
     *)
    let augmenthyps cxt tree els =
      let rewinf = getrewinfels cxt els in
      let augseq =
        fun (Seq (st, hs, cs)) ->
          try Seq (st, _The (augmentCollection hs els), cs) with
          | _The_ ->
              raise (Catastrophe_ ["prooftree.augmenthyps can't augment non-Collection hyps"])
      in
      let rec aug names =
        function
        | Tip (seq, inf, fmt) ->
            Tip (augseq seq, rewinf_merge (inf, rewinf), fmt), names (* Tips have no FRESH provisos, I'm sure *)
        | Join j              ->
            let (args, arginf) = j.args in
            let (seq, seqinf) = j.seq in
            let (trs, trsinf) = j.trs in
            let names' =
              match j.how with
              | Apply(n,ps,b) ->
                  let provisos = 
                    Optionfuns.optionfilter (function (b, Provisotype.FreshProviso(true,_,r,v)) -> Some (b,r,v) | _ -> None) 
                                            (ruleprovisos n) 
                  in
                  if null provisos then [] else
                    let argmap = Mappingfuns.mkmap (step_argmap ps args) in
                    (fun (b,r,v) -> 
                       try (b,r,_The(argmap<@>v)) 
                       with None_ -> raise (Catastrophe_["can't translate "; string_of_term v; " in prooftree.augmenthyps"]))
                    <* provisos
              | _ -> []
            in
            let rec augargs =
              function
              | [] -> 
                  raise (Catastrophe_ ["no Collection argument for sequent "; string_of_seq seq])
              | arg :: args ->
                  if isCollection arg then
                    _The (augmentCollection arg els) :: args
                  else arg :: augargs args
            in
            let trs', names'' = List.fold_right (fun t (ts,ns) -> let t',ns' = aug ns t in (t'::ts, ns')) 
                                                trs ([],names'@names) in
            Join {j with args=(match j.how with
                               | UnRule _ -> (try augargs args with _ -> args), arginf
                               | Given  _ -> (try augargs args with _ -> args), arginf (* Given doesn't have args, unfortunately *)
                               | _        -> augargs args, rewinf_merge (arginf, rewinf)
                              );
                         seq=(augseq seq, rewinf_merge (seqinf, rewinf));
                         trs=(trs', rewinf_merge (trsinf, rewinf))
                 }, names''
      in
      aug [] tree
    
    let rec maxtreeresnum t =
      match t with
      | Join j ->
          nj_fold (uncurry2 max)
            (maxseqresnum (join_seq j) ::
               (maxtreeresnum <* join_subtrees j))
            0
      | Tip t -> maxseqresnum (tip_seq t)
            
    let isCutRule = isstructurerule CutRule
    let rec isCutStep t =
      fun (FmtPath ns) ->
        try
          match rule (followPath_ns t ns) with
          | Some name -> isCutRule name
          | None -> false
        with
        | _ -> false
    let rec isCutjoin j =
      match j.how with
      | Apply (r, _, _) -> isCutRule r
      | _ -> false
    
    (* ---------------------- translating trees and paths -------------------------- *)

    let rec join_fmtNsubts j = j.fmt, join_subtrees j
    let fmtNsubts = joinopt join_fmtNsubts
    
    let pathedsubtrees =
      function
      | {cutnav=Some (l, r); trs=([tl; tr], rewinf)} ->  [[l], tl; [], tr]
      | {                    trs=(ts      , rewinf)} ->  (fun (i, t) -> [i], t) <* numbered ts
    
    (* new complication: when we hide a node, we have to try to preserve its aopt in what we continue to show.
       Maybe I could solve this with very clever manipulation of the .fmt value, but it makes my head hurt.
       So now visibles, and the rest of the tranproof mechanism, builds a resnum -> string mapping, just to
       handle aopt.
     *)
    
    let hsub = listsub sameresource
    
    let aopt_mapping tree = 
      let rec register_aopt jhs aenv t =
        let lefts = seq_lefts (sequent t) in
        let aenv = match format t with
                   | TreeFormat (_,_,Some s) ->
                       let es = hsub lefts jhs in
                       let do_e aenv = function
                                       | Element(_,r,_)       -> (r,s)::aenv 
                                       | _ (* can't happen *) -> aenv
                       in
                       List.fold_left do_e aenv es
                   | _ -> aenv
        in
        match t with
        | Tip _  -> aenv
        | Join j -> List.fold_left (register_aopt lefts) aenv (join_subtrees j)
      in
      Mappingfuns.fromlist (register_aopt [] [] tree)
    
    let rec visibles showall j =
      let pts = pathedsubtrees j in
      let default () = pts, [] in
      let rec hideroots fmt ascut (ins, outs) =
        let rec h ((n, (p, t as pt)), (ins, outs)) =
          let rec nohide () = pt :: ins, outs in
          let rec hr hard j' =
            let (Seq (_, hs', _)) = join_seq j' in
            if hard || (let (Seq (_, hs, _)) = join_seq j in hs = hs') then
              let hs = (fun (ns, t) -> p @ ns, t) <* fst (visibles showall j') in
              hs @ ins, pt :: outs
            else nohide ()
          in
          match t with
          | Tip _  -> nohide ()
          | Join j ->
              match j.fmt with
              | TreeFormat (HideRootFormat, _, aopt) ->
                  let r = hr true j in
                  (* cut steps keep their shape, else they don't mean anything *)
                  (* for a moment, I'm going to abolish hideroot above cut, on the left *)
                  if ascut && n=0 (* && List.length (fst r) <> 1 + List.length ins *) then
                    (if !cuthidingdebug then
                       consolereport ["visibles not HIDEROOTING ";
                                      string_of_Join string_of_treeformat
                                                     (bracketed_string_of_list
                                                        (string_of_seq <.> sequent) ",")
                                                     j];
                     nohide ())
                  else r
              | TreeFormat (_, RotatingFilter (i, nfs), aopt) ->
                  if try
                       match fmt, List.nth nfs i with
                       | Some (true, s, _), (true, s', _) -> s = s'
                       | _ -> false
                     with
                     | _ -> false
                  then
                    hr false j
                  else nohide ()
              | _ -> nohide ()
        in
        nj_fold h (numbered ins) ([], outs)
      in
      let res =
        if showall then default ()
        else
          match j.fmt with
          | TreeFormat (_, RotatingFilter (i, nfs), aopt) ->
              (try match Listfuns.guardednth nfs i with
                   | _, _, Some which as fmt ->
                       let inouts =
                         nj_fold
                           (fun ((i, (_, t as pt)), (ins, outs)) ->
                              if hasTip t || member (i, which) then
                                pt :: ins, outs
                              else ins, pt :: outs)
                           (numbered pts) ([], [])
                       in
                       hideroots (Some fmt) false inouts
                   | fmt -> hideroots (Some fmt) (isCutjoin j) (pts, [])
               with
               | Listfuns.Bad_nth -> default ()
              )
          | _ -> hideroots None (isCutjoin j) (pts, [])
      in
      if !prooftreedebug || !screenpositiondebug then
        (let onelevel = bracketed_string_of_list (string_of_seq <.> sequent) "," in
         let string_of_pt =
           bracketed_string_of_list (string_of_pair string_of_ns (string_of_seq <.> sequent) ",") ", "
         in
         consolereport
           ["visibles "; string_of_bool showall; " ";
            string_of_Join string_of_treeformat onelevel j; " => ";
            string_of_pair string_of_pt string_of_pt ", " res]);
      res
    
    let visfn f showall =
      function
      | Tip  _ -> None
      | Join j -> Some (f showall j)
      
    let visible_subtrees showall =
      optf fst <.> joinopt (visibles showall)
      
    let invisible_subtrees showall =
      optf snd <.> joinopt (visibles showall)

    let pathtoviewpath showall t (FmtPath ns) =
      let rec _P ns rns t =
        let r = pathto t in
        if r = ns then Some (VisPath (List.rev rns))
        else if not (null r) && isprefix (fun (x, y) -> x = y) r ns then
          _P (drop (List.length r) ns) rns t
        else
          visible_subtrees showall t &~~
            (fun ts ->
               let rec find (i, (tns, t)) =
                 if isprefix (fun (x, y) -> x = y) tns ns then
                   Some (i, drop (List.length tns) ns, t)
                 else None
               in
               findfirst find (numbered ts) &~~
                  (fun (i, ns', t) -> _P ns' (i :: rns) t)
            )
      in
      _P ns [] t
    
    (* this didn't get the right answer for the root of cuts, so I added pathto to the final result. RB 22/ii/00 *) 
    let viewpathtopath showall t (VisPath ns) =
      let rec rev1 a1 a2 =
        match a1, a2 with
        | [], ys -> ys
        | x :: xs, ys -> rev1 xs (x :: ys)
      in
      let rec _P a1 a2 a3 =
        match a1, a2, a3 with
        | n :: ns, rns, t ->
            (visible_subtrees showall t &~~
               (fun ts ->
                  try
                    let (ns', t) = Listfuns.guardednth ts n in _P ns (rev1 ns' rns) t
                  with
                  | Listfuns.Bad_nth -> None))
        | [], rns, t -> Some (FmtPath (rev1 rns (pathto t)))
      in
      _P ns [] t
    
    (* **************************************** export **************************************** *)
    
    let reasonstyle = ref "long"
    let sayTheorem = ref true
    let sayDerived = ref true
    let sayResolve = ref true
    
    let visible_subtrees showall =
      optf (fun pts -> (snd <* pts)) <.> visible_subtrees showall
    
    let rec visreason proved showall t =
      joinopt (join_visreason proved showall) t
    
    and join_visreason proved showall j =
      let shortnames = !reasonstyle = "short" in
      let rec justify name =
        if shortnames then [string_of_name name]
        else
          match thingnamed name with
          | Some (Theorem _, _) ->
              [if proved name then (if !sayTheorem then "Theorem " else "") else "Conjecture "; string_of_name name]
          | Some (Rule (_, false), _) ->
              [if proved name then (if !sayDerived then "Derived Rule " else "") else "Conjectured Rule "; string_of_name name]
          | _ -> [string_of_name name]
      in
      let rec default_reason () =
        match j.how with
        | Given _         -> [j.why]
        | Apply (n, _, _) -> justify n
        | UnRule (s, _)   -> [s]
      in
      let rec rprintf cs invis def =
        (* doesn't evaluate invis() or def() until it's needed *)
        (* bloody OCaml constant syntax.
           0x25 %
           0x68 h
           0x73 s
         *)
        let rec rpr =
          function
          | [] -> []
          | 0x25 (* % *) :: 0x68 (* h *) :: cs -> 
              let ss = invis () in ss @ rprintf cs (fun () -> ss) def
          | 0x25 (* % *) :: 0x73 (* s *) :: cs -> 
              let ss = def () in ss @ rprintf cs invis (fun () -> ss)
          | 0x25 (* % *) :: c            :: cs -> UTF.utf8_of_ucode c :: rpr cs
          | [0x25]                             -> rpr []
          | c :: cs                            -> UTF.utf8_of_ucode c :: rpr cs
        in
        rpr cs
      in
      implode
        (if showall then
           if step_resolve (j.how) && !sayResolve then "Resolve " :: default_reason ()
           else default_reason ()
         else
           (* for some reason this put the default reason in the justification. I took it out
              again ... RB 31.05.2005
            *)
           let invisf () =
             interpolate "," ( (* implode (default_reason ()) :: *) invisiblereasons proved showall j)
           in
           let f (TreeFormat (_, fmt, aopt)) =
             match fmt with
             | RotatingFilter (i, nfs) ->
                 let nohidf () = [!nohidefmt]
                 in
                 let (fmt, invis) =
                   (try match Listfuns.guardednth nfs i with
                        | _, "", Some [] -> !foldedfmt    , invisf
                        | _, "", Some _  -> !filteredfmt  , invisf
                        | _, "", None    -> !unfilteredfmt, nohidf
                        | _, s , Some _  -> s             , invisf
                        | _, s , None    -> s             , nohidf
                    with Listfuns.Bad_nth -> !rawfmt, nohidf)
                 in
                 rprintf (UTF.utf8_explode fmt) invis default_reason
             | _ -> default_reason ()
           in
           f (j.fmt))
    
    and invisiblereasons proved showall j =
      _The <*
        (bool_of_opt <|
         List.concat
           ((allreasons proved showall <.> snd) <* snd (visibles showall j)))
    
    and allreasons proved showall t =
      visreason proved showall t ::
        (match t with
         | Tip _ -> []
         | Join j ->
             List.concat ((allreasons proved showall <* join_subtrees j)))
    
    let rec join_multistep j vissubts =
      step_resolve (j.how) || List.exists (fun (ns, _) -> List.length ns > 1) vissubts
      
    let hyps = explodeCollection <.> snd_of_3 <.> seqexplode
    let concs = explodeCollection <.> thrd <.> seqexplode
    
    let rec tranproof proved showall hideuselesscuts t =
      let rec visp =
        function
        | Tip (seq, rewinf, TreeFormat (_,_,aopt)) ->
            [], Tip (seq, rewinf, VisFormat (false, false))
        | Join j as it ->
            let (viss, _) = visibles showall j in
            let lpsNsubts' = (visp <.> snd) <* viss in
            let lps = nj_fold (uncurry2 (sortedmerge earlierresource))
                              (fst <* lpsNsubts')
                              (sort earlierresource (fst (fst j.thins)))
            in
            let subts' = (snd <* lpsNsubts') in
            let hidecut =
              if showall || not(isCutjoin j) then false else
                let newhyps subt = listsub sameresource (hyps (sequent subt)) (hyps (fst j.seq)) in
                let usedresource r = List.exists (curry2 sameresource r) lps in
                match j.fmt with
                | TreeFormat (HideCutFormat, _, aopt) -> true
                | _                                   ->
                    match j.trs with
                    | [t1; t2], _ ->
                        let hideablecut () = 
                          not (hasTip t2) && (match newhyps t2 with
                                              | [ch] -> not (usedresource ch)
                                              | _    -> false (* why not? *))
                        in
                        (* visibles doesn't allow HIDEROOT just above a cut. (Really, it ought to 
                           prohibit it only on the left ...). But it's a cry for help, and we can 
                           help. We don't want to do the HIDEROOT, because it would introduce an
                           assumption box, but we can hide the cut.
                           _Unless_ it's a cut that unveils the conclusion: those are usually closed
                           by hyp, and need to stay visible.
                         *)
                        (match joinopt (fun j -> j.fmt) t1 with
                         | Some (TreeFormat (HideRootFormat, _, aopt)) -> 
                             (match newhyps t2, concs (fst j.seq) with 
                             | [ch], [c] ->
                                 not (eqelements eqterms (ch, c))
                              | _ -> false (* why not? *))
                         | _ -> hideuselesscuts && hideablecut())
                    | _ -> false
            in
            lps, Join {j with why=_The (visreason proved showall it);
                              cutnav = None;
                              fmt    = VisFormat (join_multistep j viss, hidecut);
                              hastipval = tshaveTip subts';
                              trs       = (subts', snd j.trs)
                      }
      in
      visp t
    
    (* ******************************* for export ********************************* *)
    
    let rec visproof proved showall hideuselesscuts tree =
      aopt_mapping tree, snd (tranproof proved showall hideuselesscuts tree)
    
    (* there surely ought to be a way to make these structures into one -- I suppose functors inside functors
     * ain't SML.
     *)
    module Fmttree : Access with type fmt = treeformat
                             and type path = fmtpath
                             and type prooftree = treeformat prooftree
    =
      struct
        type fmt = treeformat
        and  path = fmtpath
        type temp_prooftree = fmt prooftree
        type prooftree = temp_prooftree
        
        let fFmtPath v = FmtPath v
        let dePath = fun (FmtPath ns) -> ns
        let rePath (ns, x) = FmtPath ns, x
        let followPath t = followPath_ns t <.> dePath
        let onestep t =
          optf rePath <.> onestep_ns t <.> dePath
        let fakePath t = fakePath_ns [] t <.> dePath
        let pathPrefix t p1 p2 =
          isprefix (fun (x, y) -> x = y) (fakePath t p1) (fakePath t p2)
        let findTip t = findTip_ns t <.> dePath
        let getTip t = getTip_ns t <.> dePath
        let allTipPaths t = (fFmtPath <* allTipPaths_ns t)
        let allTipConcs t = (rePath <* allTipConcs_ns t)
        let validelement b t el = fun (FmtPath ns) -> validelement_ns b t el ns
        let validhyp = validelement true
        let validconc = validelement false
        let stillopen t = stillopen_ns t <.> dePath
        let maxtreeresnum = maxtreeresnum
        let isTip = isTip
        let hasTip = hasTip
        
        let rootPath           = fFmtPath <.> rootPath_ns
        let parentPath  t      = fFmtPath <.> parentPath_ns t <.> dePath
        let siblingPath t path = fFmtPath <.> siblingPath_ns t (dePath path)
        let subgoalPath t path = fFmtPath <.> subgoalPath_ns t (dePath path)
        
        let reason proved = visreason proved !showallproofsteps
        let subtrees = subtrees
        let sequent = sequent
        let rule = rule
        let params = params
        let args = args
        let stepprovisos = stepprovisos
        let thinned = thinned
        let format = format
        let raw_depends = raw_depends
        let depends = depends
        let findAnyGoal = optioncompose (fFmtPath, findAnyGoal_ns)
        let rec findRightwardsGoal skip t =
          fun (FmtPath ns) -> optf fFmtPath (findRightwardsGoal_ns skip t ns)
        let string_of_fmt = string_of_treeformat
        let string_of_path = string_of_fmtpath
        let string_of_prooftree = string_of_prooftree string_of_fmt
        let string_of_proofnode = string_of_proofnode string_of_fmt
        
        let find = find
        let fold_left = fold_left
      end
    
    module Vistree : Access  with type fmt = visformat
                             and type path = vispath
                             and type prooftree = visformat prooftree
    =
      struct
        type fmt = visformat
        and path = vispath
        type temp_prooftree = fmt prooftree
        type prooftree = temp_prooftree
        
        let fVisPath v = VisPath v
        let rec dePath = fun (VisPath ns) -> ns
        let rec rePath (ns, x) = VisPath ns, x
        let rec followPath t = followPath_ns t <.> dePath
        let rec onestep t =
          optf rePath <.> onestep_ns t <.> dePath
        let rec fakePath t = fakePath_ns [] t <.> dePath
        let rec pathPrefix t p1 p2 =
          isprefix (fun (x, y) -> x = y) (fakePath t p1) (fakePath t p2)
        let rec findTip t = findTip_ns t <.> dePath
        let rec getTip t = getTip_ns t <.> dePath
        let rec allTipPaths t = (fVisPath <* allTipPaths_ns t)
        let rec allTipConcs t = (rePath <* allTipConcs_ns t)
        let rec validelement b t el =
          fun (VisPath ns) -> validelement_ns b t el ns
        let validhyp = validelement true
        let validconc = validelement false
        let rec stillopen t = stillopen_ns t <.> dePath
        let maxtreeresnum = maxtreeresnum
        let isTip = isTip
        let hasTip = hasTip
        let rec rootPath t = VisPath []
        (* no inner cuts in visprooftrees *)
        let parentPath t =
          fVisPath <.> parentPath_ns t <.> dePath
        let siblingPath t path =
          fVisPath <.> siblingPath_ns t (dePath path)
        let subgoalPath t path =
          fVisPath <.> subgoalPath_ns t (dePath path)
        let rec reason proved = joinopt (fun j -> j.why) (* doesn't make use of proved, because it's already happened... *)
        let subtrees = subtrees
        let sequent = sequent
        let rule = rule
        let params = params
        let args = args
        let stepprovisos = rule &~ (_Some <.> ruleprovisos)
        let thinned = thinned
        let format = format
        let raw_depends = raw_depends
        let depends = depends
        let findAnyGoal = optioncompose (fVisPath, findAnyGoal_ns)
        let rec findRightwardsGoal skip t =
          fun (VisPath ns) -> optf fVisPath (findRightwardsGoal_ns skip t ns)
        let string_of_fmt = string_of_visformat
        let string_of_path = string_of_vispath
        let string_of_prooftree = string_of_prooftree string_of_fmt
        let string_of_proofnode = string_of_proofnode string_of_fmt

        let find = find
        let fold_left = fold_left
      end
  end



