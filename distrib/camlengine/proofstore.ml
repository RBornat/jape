(* $Id$ *)

module type T =
  sig
    type cxt and model and name and proofstage and prooftree and proviso
    and seq and thing
    
    (* the proof store *)
    val saveable : unit -> bool
    val saved : unit -> bool
    val freezesaved : unit -> unit
    val thawsaved : unit -> unit
    val saveproof :
      out_channel -> name -> proofstage -> prooftree ->
        proviso list -> seq list -> (seq * model) option -> unit
    val saveproofs : out_channel -> unit
    val proved : name -> bool
    val disproved : name -> bool
    val provedordisproved : name -> bool option
    val proofnamed :
      name ->
        (bool * prooftree * (bool * proviso) list * seq list *
           (seq * model) option)
          option
    val addproof :
      (string list -> unit) ->
        (string list * string * string * int -> bool) -> name -> bool ->
        prooftree -> seq list -> cxt -> (seq * model) option ->
        bool
    val clearproofs : unit -> unit
    val proofnames : unit -> name list
    val thingswithproofs : bool -> name list
    val namedthingswithproofs : bool -> name list -> name list
    (* bad naming here *)
    val needsProof : name -> thing -> bool
    val lacksProof : name -> bool
    val thmLacksProof : name -> bool
  end    

module M : T with type cxt = Context.Cxt.cxt
			  and type model = Forcedef.M.model 
			  and type name = Name.M.name
			  and type proofstage = Proofstage.M.proofstage
			  and type prooftree = Prooftree.Tree.Fmttree.prooftree
			  and type proviso = Proviso.M.proviso
			  and type seq = Sequent.Funs.seq
			  and type thing = Thing.M.thing
=
  struct
    open Context.Cxt
    open Forcedef.M
    open Listfuns.M
    open Mappingfuns.M
    open Menu.M
    open Miscellaneous.M
    open Name.M
    open Optionfuns.M
    open Panelkind.M
    open Paraparam.M
    open Proofstage.M
    open Prooftree.Tree
    open Prooftree.Tree.Fmttree
    open Proviso.M
    open Rewrite.Funs
    open Sml.M
    open Thing.M

    type cxt = Context.Cxt.cxt
     and model = Forcedef.M.model 
     and name = Name.M.name
     and proofstage = Proofstage.M.proofstage
     and prooftree = Fmttree.prooftree
     and proviso = Proviso.M.proviso
     and seq = Sequent.Funs.seq
     and thing = Thing.M.thing
    
    let
      (freezesaved, thawsaved, clearproofs, proofnamed, proof_depends,
       proof_children, addproof, saved, proofnames, provedthings, saveable,
       proved, disproved, provedordisproved, inproofstore, saveproof,
       saveproofs)
      =
      let proofs
        :
        (name,
         (bool * prooftree * name list * (seq * model) option))
         mapping ref =
        ref empty
      and allsaved = ref true
      and frozensaved = ref true in
      let rec freezesaved () = frozensaved := !allsaved
      and thawsaved () = allsaved := !frozensaved
      and clearproofs () = proofs := empty; allsaved := true
      and proofnamed name =
        match thinginfo name, at (!proofs, name) with
          Some (Theorem (_, provisos, seq), _), Some (v, tree, _, disproof) ->
            Some (v, tree, provisos, [], disproof)
        | Some (Rule ((_, provisos, givens, seq), _), _),
          Some (v, tree, _, disproof) ->
            Some (v, tree, provisos, givens, disproof)
        | _ -> None
      and proof_depends name =
        match at (!proofs, name) with
          Some (true, _, children, _) -> Some children
        | _ -> None
      and proof_children name =
        match proof_depends name with
          Some ns -> ns
        | None -> []
      (* when we prove something, we may prove a different sequent from the one that was 
       * proposed, and a different set of provisos.  We shall check, and update the 
       * statement of the theorem.  This has implications for conjectures that have 
       * already been used in an earlier proof, of course, and it also has implications 
       * for self-named conjectures - if the theorem sequent changes, then so should the 
       * name of the theorem ... But I don't know how to do that properly.
       *)
      (* First step: don't allow circular proofs.  
       * Future step:  check when a proof changes the meaning of a theorem
       * RB 8/x/96 
       *)
      and addproof alert query name proved proof givens cxt disproofopt =
        let cxt = rewritecxt cxt in
        let seq = rewriteseq cxt (sequent proof) in
        let provisos = provisos cxt in
        let ds = depends proof in
        let rec children n = if n = name then ds else proof_children n in
        (* don't bother with Rule/Theorem distinction: proof_depends is just as quick *)
        (* does derived rules stuff matter here? *)
        let rec putitin name thing givens place =
          addthing (name, thing, place);
          proofs :=
            ( ++ )
              (!proofs, ( |-> ) (name, (proved, proof, ds, disproofopt)));
          allsaved := false;
          true
        in
        let rec storedprovisos vps =
          let rec unvisproviso b vp = b, provisoactual vp in
          (unvisproviso true <* (provisovisible <| vps))
        in
        let rec doit () =
          match givens, thingnamed name with
            [], Some (Theorem (params, _, _), place) ->
              putitin name (Theorem (params, storedprovisos provisos, seq)) []
                place
          | _ :: _, Some (Theorem (params, _, _), place) ->
              alert
                ["replacing theorem "; parseablenamestring name;
                 " with derived rule"];
              putitin name
                (Rule ((params, storedprovisos provisos, givens, seq), false))
                givens place
          | _, Some (Rule ((params, _, _, _), _), place) ->
              putitin name
                (Rule ((params, storedprovisos provisos, givens, seq), false))
                givens place
          | _ ->
              raise
                (Catastrophe_ ["can't find theorem statement in addproof"])
        in
        if not proved then doit ()
        else
          (* if it's not proved we don't really care ... *)
          match toposort [name] children with
            _, [] -> doit ()
          | _, cycles ->
              (* no cycles *)
              alert
                ["can't add proof of "; parseablenamestring name;
                 " because it introduces circularity: ";
                 liststring
                   (fun bs -> liststring parseablenamestring " uses " bs)
                   "; and " cycles];
              false
      and saved () = !allsaved
      and proofnames () = dom !proofs in
      (* dom now gives them in insertion order, I believe *)
    
      let provedthings = proofnames in
      let rec saveable () = not (isempty !proofs)
      and proved n =
        match proofnamed n with
          Some (v, _, _, _, _) -> v
        | _ -> false
      and disproved n =
        match proofnamed n with
          Some (v, _, _, _, _) -> not v
        | _ -> false
      and provedordisproved n =
        match proofnamed n with
          Some (v, _, _, _, _) -> Some v
        | _ -> None
      in
      let inproofstore = opt2bool <*> proofnamed in

	  let menu2word _ = "MENU" in
	  let panel2word p =
		match getpanelkind p with
		  Some TacticPanelkind     -> "TACTICPANEL"
		| Some ConjecturePanelkind -> "CONJECTUREPANEL"
		| Some GivenPanelkind      -> 
			raise (Catastrophe_ ["proof in GIVENPANEL"])
		| None -> 
		   raise (Catastrophe_ ["proof in unknown panel "; namestring p])
      in
      let rec saveproof stream name stage tree provisos givens disproof =
        let rec badthing () =
          raise
            (Catastrophe_
               ["No stored conjecture/derived rule called ";
                parseablenamestring name])
        in
        let place =
          match thingnamed name with
            Some (th, place) ->
              begin match th with
                Theorem _ -> place
              | Rule (_, false) -> place
              | _ -> badthing ()
              end
          | _ -> badthing ()
        in
        let rec doit ss =
          let params =
            match thingnamed name with
              Some (Theorem (params, _, _), _) -> params
            | _ -> []
          in
          let body =
            catelim_prooftree2tactic tree provisos givens
              (catelim_modelstring disproof ss)
          in
          proofstage2word stage :: " " :: parseablenamestring name ::
            (if null params then body
             else
               "\n(" ::
                 catelim_liststring catelim_paraparamstring ", " params
                   (")" :: body))
        in
        List.iter (output_string stream)
          (match place with
             InMenu m ->
               menu2word m :: " " :: parseablenamestring m :: "\n" ::
                 doit ["END\n"]
           | InPanel p ->
               panel2word p :: " " :: parseablenamestring p :: "\n" ::
                 doit ["END\n"]
           | InLimbo -> doit [])
      and saveproofs stream =
        let rec show n =
          match proofnamed n with
            Some (v, tree, provisos, givens, disproof) ->
              saveproof stream n (if v then Proved else Disproved) tree
                ((snd <* (fst <| provisos))) givens
                disproof
          | _ -> ()
        in
        let names = proved <| thingnames () in
        let (sortednames, _) =
          toposort names (fun n -> proved <| proof_children n)
        in
        revapp show sortednames; allsaved := true
      in
      freezesaved, thawsaved, clearproofs, proofnamed, proof_depends,
      proof_children, addproof, saved, proofnames, provedthings, saveable,
      proved, disproved, provedordisproved, inproofstore, saveproof,
      saveproofs
    let rec thingswithproofs triv =
         (fun n ->
            match thingnamed n with
              None -> false
            | Some (Tactic _, _) -> false
            | Some (Macro _, _) -> false
            | Some (Rule (_, ax), _) -> not ax && (triv || proved n)
            | Some (Theorem _, _) -> triv || proved n) <|
         thingnames ()
    (* why is this different from thingswithproofs??? *)
    
    let rec namedthingswithproofs triv ts =
        (fun n ->
            match thingnamed n with
              None -> false
            | Some (Tactic _, _) -> true
            | Some (Macro _, _) -> true
            | Some (Rule (_, ax), _) -> (ax || triv) || proved n
            | Some (Theorem _, _) -> triv || proved n) <|
         ts
    let thmLacksProof = not <*> proved
    let rec needsProof a1 a2 =
      match a1, a2 with
        name, Rule (_, ax) -> not ax || not (proved name)
      | name, Tactic _ -> false
      | name, Macro _ -> false
      | name, Theorem _ -> not (proved name)
    let rec lacksProof name =
      needsProof name (fst (_The (thingnamed name)))
  end
