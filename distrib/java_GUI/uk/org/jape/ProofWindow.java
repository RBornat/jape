/* 
    $Id$

    Copyright © 2002 Richard Bornat & Bernard Sufrin
     
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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuBar;
import javax.swing.JScrollPane;
import java.awt.Point;
import java.awt.Rectangle;

public class ProofWindow extends JapeWindow {
    int proofnum;
    protected static ProofWindow focussedproof = null;

    protected ProofCanvas proofCanvas; 
    protected AnchoredScrollPane proofpane;

    protected JapeCanvas focussedCanvas;
    
    public ProofWindow(String title, int proofnum) {
        super(title);
        this.proofnum = proofnum;

        getContentPane().setLayout(new BorderLayout()); 
        proofpane = new AnchoredScrollPane();

        getContentPane().add(proofpane, BorderLayout.CENTER);
        setBar(); 
        pack();
        setVisible(true);
        focussedproof = this;
    }
    
    public static ProofWindow spawn(String title, int proofnum) throws ProtocolError {
        if (findWindow(title)!=null)
            throw new ProtocolError("already a window with that title");
        else
            return new ProofWindow(title, proofnum);
    }
    
    public static ProofWindow focussedProofWindow() {
        // for now, I only deal with one proof window
        return focussedproof;
    }

    private static void checkFocussedProof() throws ProtocolError {
        if (focussedproof==null)
            throw new ProtocolError("no focussed proof");
    }

    private static void checkFocussedProofCanvas() throws ProtocolError {
        checkFocussedProof();
        if (focussedproof.proofCanvas==null)
            throw new ProtocolError("no focussed proof panel - clearProofPane missing?");
    }

    private static void checkFocussedCanvas() throws ProtocolError {
        checkFocussedProof();
        if (focussedproof.focussedCanvas==null)
            throw new ProtocolError("no focussed panel - drawInPane missing?");
    }
        
    public static Rectangle getViewportBounds() throws ProtocolError {
        checkFocussedProof();
        return focussedproof.proofpane.getViewportBounds();
    }

    /*	from japeserver.ml:
          let displaystyle2int d =
            match d with
              BoxStyle  -> 0
            | TreeStyle -> 1
     */

    public final static byte BoxStyle  = 0,
                             TreeStyle = 1;
    protected byte style;
    private int linethickness=1;
    
    public static void setProofParams(byte style, int linethickness) throws ProtocolError {
        checkFocussedProof();
        if (style<0 || style>1)
            throw (new ProtocolError(style+" should be a proof display style: either 0 or 1"));
        focussedproof.style = style;
        focussedproof.linethickness = linethickness;
    }

    public static void clearProofPane() throws ProtocolError {
        checkFocussedProof();
        focussedproof.newProofCanvas();
    }

    private void newProofCanvas() {
        proofCanvas = new ProofCanvas();
        proofpane.add(proofCanvas);
        switch(style) {
            case BoxStyle:
                proofpane.setanchor(proofpane.ANCHOR_SOUTHWEST); break;
            case TreeStyle:
                proofpane.setanchor(proofpane.ANCHOR_SOUTH); break;
            default:
                Alert.abort("ProofWindow.newProofCanvas style="+style);
        } 
        proofpane.validate(); proofpane.repaint();
        focussedCanvas = proofCanvas;
    }

    public static void drawInPane(String s) throws ProtocolError {
        if (s.equals("proof")) {
            checkFocussedProofCanvas();
            focussedproof.focussedCanvas = focussedproof.proofCanvas;
        }
        else
            throw (new ProtocolError("drawInPane "+s));
    }

    public static void setGivens(String[] gs) throws ProtocolError {
        checkFocussedProof();
        focussedproof.newGivens(gs);
    }

    private void newGivens(String[] gs) throws ProtocolError {
        if (gs.length!=0)
            throw (new ProtocolError("can't (yet) set givens "+gs));
    }

    public static void clearProvisoView() throws ProtocolError {
        // do nothing for the time being
    }

    public static void drawstring(int x, int y, byte fontnum, byte kind,
                                  String annottext, String printtext) throws ProtocolError {
        checkFocussedCanvas();
        JapeFont.checkInterfaceFontnum(fontnum);
        focussedproof.focussedCanvas.add(
            kind==TextItem.PunctKind ?
                new TextItem(focussedproof.focussedCanvas, x, y, fontnum, annottext, printtext) :
                new SelectableTextItem(focussedproof.focussedCanvas, x, y, fontnum, kind,
                                       annottext, printtext,
                                       focussedproof.style, focussedproof.linethickness));
    }
}
