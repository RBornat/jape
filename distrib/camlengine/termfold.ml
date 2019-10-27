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

(* minwaste is capable of folding a sequence of tokens. But that's too damn ugly.
   This is an attempt to begin to solve the problem.
   
   If we are given a 'proper application' (i.e. not involving an application of an infix)
   F A B C D then we can try minwaste on the parts, rather than just tokens.  No matter how
   wasteful the result, if it fits we'll use it.
   
   Other hacks to be added ...
 *)
 
 open Invisibles
 open Listfuns
 open Minwaste
 open Miscellaneous
 open Optionfuns
 open Sml
 open Symboltype
 open Termstring
 open Text
 open UTF
 
 type font = Text.font
 and term = Termtype.term
 and textsize = Box.textsize
 and textlayout = Draw.textlayout

let termfolddebug = ref false

let rec appflatten t ts =
   if !termfolddebug then
     consolereport ["appflatten tries ("; debugstring_of_term t; ") ";
                    bracketed_string_of_list string_of_term ";\n" ts];
   isJuxtapos t &~~ (fun (f,a) -> appflatten f (a::ts) |~~ (fun _ -> Some (f::a::ts)))

let renderprio n mustbra t = 
  catelim_invisbracketedstring_of_prioterm true n mustbra t []

let invisQuote = (Stringfuns.enQuote <.> showInvisibleString)  

let rec infixnameflatten name t ts =
   if !termfolddebug then
     consolereport ["infixnameflatten tries "; Stringfuns.enQuote name; 
                    " ("; debugstring_of_term t; ") ";
                    bracketed_string_of_list (bracketed_string_of_list invisQuote ";") "; " ts];
   isInfixApp t &~~
   (fun (name', prio, assoc, a1, a2) ->
      if name=name' then
        match assoc with
          LeftAssoc  -> 
            let ts' = renderprio prio true a2 :: ts in
            infixnameflatten name a1 ts' |~~ (fun _ -> Some (renderprio prio false a1 :: ts'))
        | RightAssoc -> 
            let a1r = renderprio prio true a1 in
            (infixnameflatten name a2 ts &~~ (fun ts' -> Some(a1r::ts'))) |~~
            (fun _ -> Some(a1r :: renderprio prio false a2 :: ts))
        | TupleAssoc -> 
            (infixnameflatten name a2 ts |~~ (fun _ -> Some (renderprio prio false a2 :: ts))) &~~
            (fun ts' -> 
               infixnameflatten name a1 ts' |~~ (fun _ -> Some(renderprio prio false a1 :: ts')))
        | _ -> None (* we don't understand other associativities *)
      else None)
      
let rec infixprioflatten n mustbra t ts =
   if !termfolddebug then
     consolereport ["infixprioflatten tries "; string_of_int n; " "; string_of_bool mustbra; 
                    " ("; debugstring_of_term t; ") ";
                    bracketed_string_of_list (bracketed_string_of_list invisQuote ";") "; " ts];
   isInfixApp t &~~
   (fun (_, n', assoc, a1, a2) ->
      if n'>n || (n'=n && not mustbra) then
        (infixprioflatten n' (assoc=LeftAssoc) a2 ts |~~ 
            (fun _ -> Some(renderprio n' (assoc=LeftAssoc) a2 :: ts))) &~~
        (fun ts' ->
           infixprioflatten n' (assoc=RightAssoc) a1 ts' |~~ 
               (fun _ -> Some(renderprio n' (assoc=RightAssoc) a1 :: ts')))
      else None)
      
 let measure font = fst_of_3 <.> Japeserver.measurestring font
 
 let termfold (_, font, preleading, interleading, postleading, w, t) =
   let measure = measure font in
   let render t = catelim_invisbracketedstring_of_term true t [] in
   let renderargs t = catelim_invisbracketedstring_of_termarg true t [] in
   let tstring = render t in
   let mkTextInfo sss =
     let sys = (fun ss -> Syllable (font, implode ss)) <* sss in
     let text =
       Text (Linebreak preleading ::
               (interpolate (Linebreak interleading) sys @ [Linebreak postleading]))
     in
     Draw.measuretext MidBlock text
   in
   let default () =
     let ss = minwaste measure w tstring in
     List.length ss!=1, mkTextInfo ss
   in
   let tryfold ts tts default =
     let rec starts xs ys =
       match xs, ys with
         []   , _     -> true
       | x::xs, y::ys -> x=y && starts xs ys
       | _            -> false
     in
     let matchup t tts =
       if !termfolddebug then
         consolereport 
           ["matchup tries "; 
            bracketed_string_of_list invisQuote ";" t;
            "\n";
            bracketed_string_of_list invisQuote ";" tts];
       let rec getstart tt1 tts' =
         match tts' with 
           [] -> raise (Catastrophe_ ["Termfold.tryfold.matchup fails"])
         | t0::tts'' -> if starts t tts' then 
                          List.rev t @ tt1, Listfuns.drop (List.length t) tts'
                        else getstart (t0::tt1) tts''
       in
       let rec getend tt1 tts' = 
         match tts' with
           ""  :: tts'' -> getend tt1 tts''
         | " " :: tts'' -> getend (" "::tt1) tts''
         | c   :: tts'' -> if isInvisibleKet c then getend (c::tt1) tts''
                           else List.rev tt1, tts'
         | []           -> List.rev tt1, tts'
       in
       uncurry2 getend (getstart [] tts)
     in
     let rec split ts tts = 
       if !termfolddebug then
         consolereport 
           ["split tries "; 
            bracketed_string_of_list
              (bracketed_string_of_list invisQuote ";")
              ";" ts;
            " ";
            bracketed_string_of_list invisQuote ";" tts];
       match ts, tts with
         []      , []   -> []
       | (t::ts'), _::_ ->
           let tt1, tts' = matchup t tts in
           if !termfolddebug then
             consolereport 
               ["matchup gives "; 
                bracketed_string_of_list invisQuote ";" tt1;
                ",\n";
                bracketed_string_of_list invisQuote ";" tts'];
           tt1 :: split ts' tts'
       | _ -> 
           consolereport 
             ["Termfold.tryfold.split failed on "; 
              bracketed_string_of_list
                (bracketed_string_of_list invisQuote ";")
                ";" ts;
              " ";
              bracketed_string_of_list invisQuote ";" tts];
              raise (Catastrophe_ ["Termfold.tryfold.split failed"])
     in
     let sss = split ts tts in
     let ss = List.map implode sss in
     let folded = minwaste measure w ss in
     let (size, _ as r) = mkTextInfo folded in
     if Box.tsW size<=w then (true, r) else default()
   in
   match appflatten t [] with
     Some ts -> 
       if !termfolddebug then
         consolereport ["termfold splits "; Stringfuns.enQuote (string_of_term t);
                        " into "; 
                        bracketed_string_of_list (Stringfuns.enQuote <.> string_of_term) 
                                                "\n" ts];
       tryfold (List.map renderargs ts) tstring default
   | _ -> 
     (match isInfixApp t with
        Some(name,n,_,_,_) ->
          tryfold (_The (infixnameflatten name t [])) tstring
                  (fun _ -> tryfold (_The (infixprioflatten n false t [])) tstring default)
      | _ -> default ())
   
   (*
        if tsW size <= w then
          begin if !boxfolddebug then consolereport ["too narrow"]; el end
        else
          let e =
            match epi with
              ElementPlan      (_, e, _)      -> e
            | AmbigElementPlan ((_, e, _), _) -> e
            | ElementPunctPlan                ->
                raise (Catastrophe_ ["foldformula ElementPunctPlan"])
          in
          let estring = catelim_invisbracketedstring_of_element true e [] in
          let measure = fst_of_3 <.> measurestring TermFont in
          if !boxfolddebug then
            consolereport
              ["folding ";
               bracketed_string_of_list
                 (fun s -> string_of_pair string_of_int enQuote "," (measure s, s))
                 ", " estring];
          let sss = minwaste measure w estring in
          if !boxfolddebug then
            consolereport
              ["width is "; string_of_int w; ";\nformula folded to ";
               string_of_int (List.length sss); " lines: ";
               bracketed_string_of_list (fun ss -> enQuote (implode ss)) ", "
                 sss];
          let sys = (fun ss -> Syllable (TermFont, implode ss)) <* sss in
          let text =
            Text
              (Linebreak textleading ::
                 (interpolate (Linebreak termfontleading) sys @
                    [Linebreak textleading]))
          in
          if !boxfolddebug then
            consolereport ["text is "; string_of_text text];
          let (size, _ as textinfo) = Draw.measuretext MidBlock text in
          if !boxfolddebug then
            consolereport ["textsize is "; string_of_textsize size];
          textinfo, epi
*)

module FoldCache = Cache.F(struct type dom = string * font * int * int * int * int * term
                                  type ran = bool * (textsize * textlayout)
                                  let eval = termfold
                                  let size = 251
                           end)

let termfold font preleading interleading postleading w t =
  FoldCache.lookup (Japeserver.getfontname font, font, preleading, interleading, postleading, w, t)

let resetcache = FoldCache.reset
