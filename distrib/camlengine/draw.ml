(* $Id$

       Functions for use with drawing packages (e.g. treedraw, boxdraw)

*)

open Box 
open Japeserver 
open Mappingfuns 
open Sml.M
open Text.M

exception Catastrophe_ = Miscellaneous.Catastrophe_

type term       = Term.Funs.term
 and element    = Term.Funs.element
 and reason     = Absprooftree.reason
 and textlayout = Text.M.textlayout
 and text       = Text.M.text
 and textalign  = Text.M.textalign
 and textsize   = Box.textsize
 and pos        = Box.pos
 and size       = Box.size
 and box        = Box.box
 and textbox    = Box.textbox
 and font       = Text.M.font
 and displayclass = Displayclass.displayclass
 
let consolereport = Miscellaneous.consolereport
let element2text  = Absprooftree.element2text
let elementstring = Term.Termstring.elementstring
let findfirst     = Optionfuns.findfirst
let interpolate   = Listfuns.interpolate
let optionstring  = Optionfuns.optionstring
let pairstring    = Stringfuns.M.pairstring
let proofpane     = Displayfont.ProofPane
let reason2text   = Absprooftree.reason2text
let reason2fontNstring = Absprooftree.reason2fontNstring
let term2text     = Absprooftree.term2text
let termstring    = Term.Termstring.termstring
let triplestring  = Stringfuns.M.triplestring

let fontinfo = fontinfo
and blacken = blacken
and greyen = greyen
and drawLine = drawLine

type class__ = int (* could be Japeserver.class__, if I played it right ... *)
    
let rec drawinproofpane () = Japeserver.drawinpane proofpane

type 'a plan = Formulaplan of (textlayout * textbox * 'a)

let rec planstring f =
  fun (Formulaplan plan) ->
    "Formulaplan" ^ triplestring textlayoutstring textboxstring f "," plan
(* was plan2string *)
let rec plan2string =
  function
    Formulaplan (Textlayout [_, _, s], _, _) -> s
  | p ->
      raise
        (Catastrophe_
           ["multisyllable text in plan2string -- ";
            planstring (fun _ -> "...") p])
let rec plantextlayout = fun (Formulaplan (tl, _, _)) -> tl
let rec plantextbox = fun (Formulaplan (_, b, _)) -> b
let rec planinfo = fun (Formulaplan (_, _, i)) -> i
let rec plantextsize p = tbSize (plantextbox p)
let rec planlisttextsize ps =
  nj_fold ( +-+ ) (List.map plantextsize ps) nulltextsize
let viewBox = Japeserver.getProofPane
let rec clearView () = Japeserver.clearProofPane ()
let highlight = Japeserver.highlight
let drawBox = Japeserver.drawRect
let rec linethickness leading =
  (* width of the lines (box, selection) we draw *)
  let r = max ((leading + 2) / 3) (1) in(* consolereport["leading ", string_of_int leading, "; thickness ", string_of_int r]; *)
   r
let setproofparams = Japeserver.setproofparams
let rec measuretext ta t = Text.M.measuretext Japeserver.measurestring ta t
(* note fixed alignment, so don't use for folded/multiline texts *)
let text2textinfo = measuretext FirstLine
let rec mktextinfo f = text2textinfo <*> f
let rec string2textinfo f = mktextinfo (string2text f)
let rec element2textinfo elementstring =
  mktextinfo (element2text elementstring)
let rec term2textinfo termstring = mktextinfo (term2text termstring)
let reason2textinfo = mktextinfo reason2text
let rec procrustean_reason2textinfo w r =
  let (rf, rs) = reason2fontNstring r in
  string2textinfo rf (Japeserver.procrustes w " ..." rf rs)
let rec textinfo2plan (size, layout) info p =
  Formulaplan (layout, textbox (p, size), info)
let rec string2plan font s info p =
  textinfo2plan (string2textinfo font s) info p
let rec element2plan ef element info p =
  textinfo2plan (element2textinfo ef element) info p
(* I'd much rather say val element2plan = textinfo2plan o element2textinfo, but
 * I seem to remember that SML 109 can't cope.
 *)
 
(* to make a comma-separated list without heap churning ... can't be hard, surely?
 * But I have made a right meal of it in the past.
 *
 * Ok, start again ... since we have to take the element and make a plan out of it,
 * it's best to give an element function which does the whole thing.  Users can
 * make it out of element2plan easily.
 *
 * Then we need commatx and commainf, and we have the whole thing, I think, with
 * just a sort of foldr.  Surely.  We need a zero _function_ rather than a zero
 * _value_ because other people need to take the final position and draw the rest
 * of the sequent, for example
 *
 * But if we use foldr there is heap churning because all the conses take place
 * afterwards -- in SML, anyway.  Never mind: this works, at least.
 *)

let rec plancons =
  fun (Formulaplan (_, b1, _) as plan) moref ->
    let (plans, b2) = moref (rightby (tbPos b1, tsW (tbSize b1))) in
    plan :: plans, ( +|-|+ ) (b1, b2)
let rec plan2plans = fun (Formulaplan (_, b, _) as plan) -> [plan], b
(* I bet this churns like buggery! Still, it never reverses :-) *)
let rec things2plans thingf sepf moref things zp =
  let rec f a1 a2 =
    match a1, a2 with
      [], p -> moref p
    | [el], p -> plancons (thingf el p) moref
    | el :: things, p ->
        plancons (thingf el p) (fun p' -> plancons (sepf p') (f things))
  in
  f things zp
let rec sequent2plans hs hf cs cf commaf stilef sp =
  let rec rhs rp =
    things2plans cf commaf (fun _ -> [], emptytextbox) cs rp
  in
  things2plans hf commaf (fun p' -> plancons (stilef p') rhs) hs sp
(*   (* the plan comes out of mktlp in reverse order; it's regularised in 
    * mkelementlistplan and mksequentplan (this is necessary for plan-linearisers 
    * and for the minimum-waste split algorithm).  It isn't done here because it would 
    * cause too much hoohah - see, for example, mksequentplan
    *)
   (* the 'plans' argument to mktlp has to be backwards as well, so that the 
    * whole thing gets built backwards ...
    *)
   fun mktlp elf els comminf commaclass startinf =
     let fun addcomma (p, plans, size) =
           let val (p', txplan, commasize) = textinfo2plan comminf commaclass p
           in
               (p', txplan::plans, size+-+commasize)
           end
         fun addelement element (p, plans, size) =
           let val (p', elementplan, elementsize) = elf element p
           in
               (p', elementplan::plans, size+-+elementsize)
           end
         fun S []        z = z
         |   S [el]      z = addelement el z
         |   S (el::els) z = S els (addcomma (addelement el z)) (* isn't this just foldl? *)
         
     in
         S els startinf (* don't reverse it: trust your users *)
     end

   fun s2p mkp hs hclass cs cclass stileinf stileclass comminf commaclass (p,plans,size) =
     let val (p,hplans,hsize) = mktlp (mkp hclass) hs comminf commaclass (p,List.rev plans,size)
         val (p',stileplan, stilesize) = textinfo2plan stileinf stileclass p
         val (p'',allplans,allsize) = 
           mktlp (mkp cclass) cs comminf commaclass (p',stileplan::hplans,hsize+-+stilesize)
     in
         (p'',List.rev allplans,allsize)
     end
   ;
                     
   val sequent2plans = 
     let fun mkp elf el p = element2plan el elf p in s2p mkp end

(*    val sequentntexts2plans = let fun mkp class eli p = 
     elementinfo2plan (elnoside eli) class p in s2p mkp end
   ;
*)                                                 
*)

let rec planOffset =
  fun (Formulaplan (layout, box, thing)) pos ->
    Formulaplan (layout, tbOffset box pos, thing)
let rec drawplan f p =
  fun (Formulaplan (Textlayout tl, b, info)) ->
    Japeserver.drawmeasuredtext (f info) tl (( +->+ ) (p, tbPos b))
let rec findfirstplanhit p =
  findfirst
    (fun pl -> if withintb (p, plantextbox pl) then Some pl else None)
let textinfostring = pairstring textsizestring textlayoutstring ","

