/* 
    $Id$

    Copyright © 2003 Richard Bornat & Bernard Sufrin
     
        richard@bornat.me.uk
        sufrin@comlab.ox.ac.uk

    This file is part of japeserver, which is part of jape.

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
    
*/

public interface ProtocolConstants {
    /*  from displayfont.ml:
        (* Useful translation for Japeserver marshalling.
        * Current C/Java/Tk interfaces believe in these integers.
        *
        *  TermFont = 0
        *  ReasonFont = 1
        *  ProvisoFont = 2
        *
        *)
     */
    
    public final static byte TermFontNum = 0,  ReasonFontNum = 1,  ProvisoFontNum = 2;
    
    /*  from japeserver.ml:
            match m with
              "subformula" -> 0
            | "token"      -> 1
            | _ -> raise (Catastrophe_ ...
     */

    public static final byte SubformulaSelectionMode = 0,
                             TokenSelectionMode      = 1;

    /*	from japeserver.ml:
            let displaystyle2int d =
              match d with
                  BoxStyle  -> 0
                | TreeStyle -> 1
    */

    public final static byte BoxStyle  = 0,
                             TreeStyle = 1;

    /* from displayclass.ml/mli:
        (* Useful translation for Japeserver marshalling.
         * Current C/Java/Tk interfaces believe in these integers.
         *
         *   DisplayPunct  0
         *   DisplayConc   1
         *   DisplayHyp    2
         *   DisplayReason 3
         *   DisplayAmbig  4
         *
         *)
     */

    public static final byte PunctTextItem  = 0,
                             ConcTextItem   = 1,
                             HypTextItem    = 2,
                             ReasonTextItem = 3,
                             AmbigTextItem  = 4;

    /* from displayfont.mli:
        (* Useful translation for Japeserver marshalling.
         *
         *  ProofPane = 0
         *  DisproofPane = 1
         *
         *)
        */

    public static final byte ProofPaneNum = 0, DisproofPaneNum = 1;

    /* from japeserver.ml:
        match panelkind with
        TacticPanelkind     -> "0"
        | ConjecturePanelkind -> "1"
        | GivenPanelkind      -> "2"
    */

    public static final byte	TacticPanelKind     = 0,
                                ConjecturePanelKind = 1,
                                GivenPanelKind      = 2;
        
    
}
