(*
    $Id$

    Copyright (C) 2003-4 Richard Bornat & Bernard Sufrin
     
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

open Termtype
open Termstring

open Idclass
open Stringfuns
open Optionfuns
open Sml

let int_of_resnum = Termtype.int_of_resnum
  
(* ------------------------------------------ the term store ------------------------------------------ *)

(* this is an attempt to stop space explosions when reading in large proofs, and when rewriting.  It costs
 * in time, but we can't afford to use 64MB to read in 1.4MB of very repetitious tactic text.
 * RB 26/i/00
 *)
(* After an initial experiment, light begins to dawn.  We shouldn't cache everything: it costs too much.
 * And we shouldn't cache stuff which includes unknowns, because it is ephemeral.  Ditto ResUnknowns.
 * So now I cache constant elements and constant collection and nothing else.  Computing the hash is
 * still not free, of course.
 * RB 27/i/00
 *)
(* Initial impressions were wrong.  With large proofs, space is so important that we must cache everything.
 * RB 21/iii/00
 *)
(* and of course, you idiot, what you should _really_ do is to make resources shared ... *)
(* and of course, you idiot, that won't work because of histories which contain proof trees ... *)

let termhashing = ref true
let
  (hashterm, hashelement, registerId, registerUnknown,
   registerApp, registerTup, registerLiteral, registerFixapp,
   registerSubst, registerBinding, registerCollection, registerElement,
   registerSegvar, resettermstore)
  =
   (let module Local =
      struct
        let hash_list f = Hashtbl.hash <.> List.map f
        let hash_2 fa fb (a,b) = Hashtbl.hash (fa a, fb b)
        let hash_3 fa fb fc (a,b,c) = Hashtbl.hash (fa a, fb b, fc c)
        
        let rec hashterm t =
          match t with
            Id (Some h, _, _) -> h
          | Id (None, v, c) -> hashId v c
          | Unknown (Some h, _, _) -> h
          | Unknown (None, v, c) -> hashUnknown v c
          | App (Some h, _, _) -> h
          | App (None, f, a) -> hashApp f a
          | Tup (Some h, _, _) -> h
          | Tup (None, s, ts) -> hashTup s ts
          | Literal (Some h, _) -> h
          | Literal (None, l) -> hashLiteral l
          | Fixapp (Some h, _, _) -> h
          | Fixapp (None, ss, ts) -> hashFixapp ss ts
          | Subst (Some h, _, _, _) -> h
          | Subst (None, r, _P, vts) -> hashSubst r _P vts
          | Binding (Some h, _, _, _) -> h
          | Binding (None, bindings, env, pat) -> hashBinding bindings env pat
          | Collection (Some h, _, _) -> h
          | Collection (None, c, es) -> hashCollection c es
        
        and hash_ts ts = hash_list hashterm ts
        
        and hashId v c = Hashtbl.hash (v, c)
        and hashUnknown v c = Hashtbl.hash (v, c)
        and hashApp f a = hash_ts [f; a]
        and hashTup s ts = Hashtbl.hash (s, hash_ts ts)
        and hashLiteral l = Hashtbl.hash l
        and hashFixapp ss ts = Hashtbl.hash (ss, hash_ts ts)
        and hashSubst r _P vts = 
              Hashtbl.hash (r, hashterm _P, hash_list (hash_2 hashterm hashterm) vts)
        and hashBinding bindings env pat =
              hash_3 (hash_3 hash_ts hash_ts hash_ts)
                     (hash_list (hash_2 hashterm Hashtbl.hash))
                     hashterm
                     (bindings, env, pat)
        and hashCollection c es = Hashtbl.hash (c, hash_es es)
        
        and hashelement e =
          match e with
            Segvar (Some h, _, _) -> h
          | Segvar (None, ts, t) -> hashSegvar ts t
          | Element (Some h, _, _) -> h
          | Element (None, r, t) -> hashElement r t
        
        and hash_es es = hash_list hashelement es
        
        and hashSegvar ts t = hash_ts (t::ts)
        and hashElement r t = Hashtbl.hash (r, hashterm t)

        module T = Hashtbl.Make (struct type t=term
                                        let equal=(=)
                                        let hash=hashterm
                                 end)           
        module E = Hashtbl.Make (struct type t=element
                                        let equal=(=)
                                        let hash=hashelement
                                 end)           
        let termtable = T.create 127 (* why not? It can only grow :-> *)
        let cacheterm t =
          try T.find termtable t with Not_found -> T.add termtable t t; t
        
        let elementtable = E.create 127 (* why not? It can only grow :-> *)
        let cacheelement e =
          try E.find elementtable e with Not_found -> E.add elementtable e e; e

        (* we only cache constant collections and elements. We may experiment, if this
         * is a success, with caching Ids, since they are small, frequent and not very 
         * diverse.
         * RB 27/i/00
         *)
        (* Profiling indicates that hashing constant collections and elements slows input down by 20%, 
         * and hashing identifiers makes that 24%.  Hmmm.
         * I won't make it a fixture till I profile some proof steps and proof reloads.
         * RB 31/i/00
         *)
        (* But it has been a fixture ever since. And now it's using Hashtbl. 
           RB 8/vii/2002
         *)
        (* for efficiency's sake I don't make a term with None, hash it and then re-enter it. *)
        let registerId (v, c) =
          if !termhashing then
            let h = hashId v c in cacheterm (Id (Some h, v, c))
          else Id (None, v, c)
        and registerUnknown (v, c) = Unknown (None, v, c)
        and registerApp (f, a) =
          if !termhashing then 
            let h = hashApp f a in cacheterm (App (Some h, f, a))
          else App (None, f, a)
        and registerTup (s, ts as sts) =
          if !termhashing then
            let h = hashTup s ts in cacheterm (Tup (Some h, s, ts))
          else Tup (None, s, ts)
        and registerLiteral l =
          if !termhashing then
            let h = hashLiteral l in cacheterm(Literal (Some h, l))
          else Literal (None, l)
        and registerFixapp (ss, ts) =
          if !termhashing then
            let h = hashFixapp ss ts in cacheterm (Fixapp (Some h, ss, ts))
          else Fixapp (None, ss, ts)
        and registerSubst (r, t, vts) =
          if !termhashing then
            let h = hashSubst r t vts in cacheterm (Subst (Some h, r, t, vts))
          else Subst (None, r, t, vts)
        and registerBinding (bindings, pat, body) =
          if !termhashing then
            let h = hashBinding bindings pat body in 
            cacheterm (Binding (Some h, bindings, pat, body))
          else Binding (None, bindings, pat, body)
        and registerCollection (c, els) =
          if !termhashing then 
            let h = hashCollection c els in cacheterm (Collection (Some h, c, els))
          else Collection (None, c, els)
        and registerElement (r, t) =
          if !termhashing then
            let h = hashElement r t in cacheelement (Element (Some h, r, t))
          else Element (None, r, t)
        and registerSegvar (ms, v) =
          if !termhashing then 
            let h = hashSegvar ms v in cacheelement (Segvar (Some h, ms, v))
          else Segvar (None, ms, v)
        
        let resettermstore () = T.clear termtable; E.clear elementtable
        let hashterm t = if !termhashing then Some (hashterm t) else None
        let hashelement e = if !termhashing then Some (hashelement e) else None
      end
    in
    Local.hashterm, Local.hashelement, Local.registerId, Local.registerUnknown,
    Local.registerApp, Local.registerTup, Local.registerLiteral, Local.registerFixapp,
    Local.registerSubst, Local.registerBinding, Local.registerCollection, Local.registerElement,
    Local.registerSegvar, Local.resettermstore
  )
