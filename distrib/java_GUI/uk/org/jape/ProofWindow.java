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

    protected KPanel proofpanel; 
    protected JapeScrollPane proofpane;

    protected KPanel focussedpanel;
    
    public ProofWindow(String title, int proofnum) {
        super(title);
        this.proofnum = proofnum;

        getContentPane().setLayout(new BorderLayout()); 
        proofpane = new JapeScrollPane();

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

    private static void checkFocussedProofPanel() throws ProtocolError {
        checkFocussedProof();
        if (focussedproof.proofpanel==null)
            throw new ProtocolError("no focussed proof panel - clearProofPane missing?");
    }

    private static void checkFocussedPanel() throws ProtocolError {
        checkFocussedProof();
        if (focussedproof.focussedpanel==null)
            throw new ProtocolError("no focussed panel - drawInPane missing?");
    }
        
    public static Rectangle getViewportBounds() throws ProtocolError {
        checkFocussedProof();
        return focussedproof.proofpane.getViewportBounds();
    }

    private String style;
    private int linethickness;
    
    public static void setProofParams(String style, int linethickness) throws ProtocolError {
        checkFocussedProof();
        focussedproof.style = style;
        focussedproof.linethickness = linethickness;
    }

    public static void clearProofPane() throws ProtocolError {
        checkFocussedProof();
        focussedproof.newProofPanel();
    }

    private void newProofPanel() {
        proofpanel = new KPanel();
        proofpane.add(proofpanel);
        if (style!=null) {
            if (style.equals("box"))
                proofpane.setanchor(proofpane.ANCHOR_SOUTHWEST);
            else
            if (style.equals("tree"))
                proofpane.setanchor(proofpane.ANCHOR_SOUTH);
        } 
        proofpane.validate(); proofpane.repaint();
        focussedpanel = proofpanel;
    }

    public static void drawInPane(String s) throws ProtocolError {
        if (s.equals("proof")) {
            checkFocussedProofPanel();
            focussedproof.focussedpanel = focussedproof.proofpanel;
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

    public static void drawstring(int x, int y, int font, int kind, String annottext, String printtext) throws ProtocolError {
        checkFocussedPanel();
        // ignore kind for the moment; ditto annottext
        JLabel l = new JLabel(printtext);
        l.setFont(JapeFont.getFont(font));
        TextDimension td = JapeFont.measure(l, printtext);
        l.setBounds(x,y-td.ascent,td.width, td.ascent+td.descent);
        focussedproof.focussedpanel.add(l);
    }
}
