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

open Cxttype
open Mappingfuns
open Rewinf
open Proviso
open Termfuns
open Termstore
open Sml
open Listfuns

let _NotinProviso = Provisotype._NotinProviso

let varmap (Context { varmap }) = varmap

let resmap (Context { resmap }) = resmap

let provisos (Context { provisos = ps, _ }) = ps

let usedVIDs (Context { usedVIDs }) = usedVIDs

let nextresnum (Context { nextresnum }) = nextresnum

let withvarmap (Context cxt) map = Context { cxt with varmap = map }

let plusvarmap (Context ({ varmap } as cxt)) map =
  Context { cxt with varmap = varmap ++ map }

let withresmap (Context cxt) map = Context { cxt with resmap = map }

let plusresmap (Context ({ resmap } as cxt)) map =
  Context { cxt with resmap = resmap ++ map }

let withprovisos c ps =
  match (c, ps) with
  | Context ({ provisos; provisosig } as cxt), [] ->
      Context
        {
          cxt with
          provisos = ([], Some nullrewinf);
          provisosig =
            (match provisos with [], _ -> provisosig | _ -> provisosig + 1);
        }
  | Context ({ provisosig } as cxt), ps ->
      Context { cxt with provisos = (ps, None); provisosig = provisosig + 1 }

let withvisibleprovisos cxt ps =
  withprovisos cxt (List.map (fun p -> mkvisproviso (true, p)) ps)

let plusprovisos c ps =
  match (c, ps) with
  | cxt, [] -> cxt
  | Context ({ provisos = ps, _; provisosig } as cxt), ps' ->
      Context
        { cxt with provisos = (ps' @ ps, None); provisosig = provisosig + 1 }

let plusvisibleprovisos cxt ps =
  plusprovisos cxt (List.map (fun p -> mkvisproviso (true, p)) ps)

let withexterior (Context cxt) s =
  Context { cxt with outside = Exterior (s, None, None) }

let withusedVIDs (Context cxt) vs = Context { cxt with usedVIDs = vs }

let plususedVIDs (Context ({ usedVIDs } as cxt)) vs =
  Context { cxt with usedVIDs = mergeVIDs usedVIDs vs }

let freshVID (Context { usedVIDs } as cxt) class__ v =
  let v' = uniqueVID class__ usedVIDs [] v in
  (plususedVIDs cxt [ v' ], v')

(* if this function is applied when the base sequent hasn't been rewritten,
 * it isn't very useful.  But it can happen sometimes, when you are just
 * making freshRule to find out something about the rule.
 *)
let freshproofvar cxt class__ v =
  let cxt', v' = freshVID cxt class__ v in
  let var = registerId (v', class__) in
  match cxt' with
  | Context { usedVIDs; outside = Exterior (_, Some r, _) } ->
      ( plusprovisos cxt'
          (nj_fold
             (fun (u, ps) -> mkvisproviso (false, _NotinProviso (var, u)) :: ps)
             (isUnknown <| rewinf_vars r)
             []),
        var )
  | _ -> (cxt', var)

let withresnum (Context cxt) num = Context { cxt with nextresnum = num }

let freshresnum cxt =
  let n = nextresnum cxt in
  (withresnum cxt (n + 1), n)

let newcxt =
  Context
    {
      varmap = empty;
      resmap = empty;
      provisos = ([], Some nullrewinf);
      provisosig = 0;
      outside = NoExterior;
      usedVIDs = [];
      nextresnum = 1;
    }

let selfparentprovisos (Context ({ provisos = ps, ri } as cxt)) =
  Context { cxt with provisos = (List.map provisoselfparent ps, ri) }

(* this context is provided so that you can get a neutral reading of rewinf from some
 * formula that you haven't rewritten, or don't know has been rewritten
 *)
let dont_rewrite_with_this =
  Context
    {
      varmap = empty;
      resmap = empty;
      provisos = ([], Some nullrewinf);
      provisosig = -463;
      outside = NoExterior;
      usedVIDs = [];
      nextresnum = -999;
    }
