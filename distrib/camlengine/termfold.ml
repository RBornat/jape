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
 open Termstring
 open Termtype
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
                    bracketedstring_of_list string_of_term ";\n" ts];
   match t with
     App(_, f, a) ->
       if isInfixApp t then None else appflatten f (a::ts) |~~ (fun _ -> Some (f::a::ts))
   | _            -> None (* later we will think about other shapes *)
 
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
     let sss = minwaste measure w tstring in
     mkTextInfo sss
   in
   let tryfold ts tts =
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
            bracketedstring_of_list (Stringfuns.enQuote <.> showInvisibleString) ";" t;
            "\n";
            bracketedstring_of_list (Stringfuns.enQuote <.> showInvisibleString) ";" tts];
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
            bracketedstring_of_list
              (bracketedstring_of_list (Stringfuns.enQuote <.> showInvisibleString) ";")
              ";" ts;
            " ";
            bracketedstring_of_list (Stringfuns.enQuote <.> showInvisibleString) ";" tts];
       match ts, tts with
         []      , []   -> []
       | (t::ts'), _::_ ->
           let tt1, tts' = matchup t tts in
           if !termfolddebug then
             consolereport 
               ["matchup gives "; 
                bracketedstring_of_list (Stringfuns.enQuote <.> showInvisibleString) ";" tt1;
                ",\n";
                bracketedstring_of_list (Stringfuns.enQuote <.> showInvisibleString) ";" tts'];
           tt1 :: split ts' tts'
       | _ -> 
           consolereport 
             ["Termfold.tryfold.split failed on "; 
              bracketedstring_of_list
                (bracketedstring_of_list (Stringfuns.enQuote <.> showInvisibleString) ";")
                ";" ts;
              " ";
              bracketedstring_of_list (Stringfuns.enQuote <.> showInvisibleString) ";" tts];
              raise (Catastrophe_ ["Termfold.tryfold.split failed"])
     in
     let sss = split ts tts in
     let ss = List.map implode sss in
     let folded = minwaste measure w ss in
     let (size, _ as r) = mkTextInfo folded in
     if Box.tsW size<=w then r else default()
   in
   match t with
     App _ ->
       (match appflatten t [] with
          Some ts -> 
            if !termfolddebug then
              consolereport ["termfold splits "; Stringfuns.enQuote (string_of_term t);
                             " into "; 
                             bracketedstring_of_list (Stringfuns.enQuote <.> string_of_term) 
                                                     "\n" ts];
            tryfold (List.map renderargs ts) tstring
        | _       -> default ())
   | _ -> default ()
   
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
               bracketedstring_of_list
                 (fun s -> string_of_pair string_of_int enQuote "," (measure s, s))
                 ", " estring];
          let sss = minwaste measure w estring in
          if !boxfolddebug then
            consolereport
              ["width is "; string_of_int w; ";\nformula folded to ";
               string_of_int (List.length sss); " lines: ";
               bracketedstring_of_list (fun ss -> enQuote (implode ss)) ", "
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
                                  type ran = textsize * textlayout
                                  let eval = termfold
                                  let size = 251
                           end)

let termfold font preleading interleading postleading w t =
  FoldCache.lookup (Japeserver.getfontname font, font, preleading, interleading, postleading, w, t)

let resetcache = FoldCache.reset
