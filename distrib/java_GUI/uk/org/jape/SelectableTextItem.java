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
import java.awt.Color;
import java.awt.Graphics;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.Rectangle;

public class SelectableTextItem extends TextItem {
    public Color selectionColour = Color.red;

    protected final byte kind;
    protected byte selected = ProofCanvas.NoSel;
    protected byte proofstyle;
    protected int selectionthickness;

    // for drawing selections
    private final int left = 0, top = 0, right, bottom;
    
    public SelectableTextItem(JapeCanvas canvas, int x, int y, byte fontnum, byte kind,
                              String annottext, String printtext,
                              byte proofstyle, int selectionthickness) {
        super(canvas,x,y,fontnum,annottext,printtext);
        this.kind=kind; this.proofstyle=proofstyle; this.selectionthickness=selectionthickness;
        // make room for selection rectangle
        inset = 2*selectionthickness;
        Rectangle r = getBounds();
        r.grow(inset,inset);
        setBounds(r);
        right = r.width-selectionthickness; bottom = r.height-selectionthickness;

        addMouseListener(new MouseAdapter() {
            public void mouseClicked(MouseEvent e) {
                click(e);
            }
        });
    }

    protected void click(MouseEvent e) {
        byte selected;
        if (this.selected==ProofCanvas.NoSel) {
            // determine selection type
            switch (kind) {
                case ConcKind:
                    selected = ProofCanvas.ConcSel; break;
                case HypKind:
                    selected = ProofCanvas.HypSel; break;
                case ReasonKind:
                   selected = ProofCanvas.ReasonSel; break;
                case AmbigKind:
                    selected = e.getY()<getHeight()/2 ? ProofCanvas.ConcSel : ProofCanvas.HypSel;
                    break;
                default:
                    Alert.abort("SelectableTextItem.click kind="+kind);
                    selected = ProofCanvas.NoSel; // shut up compiler
            }
            canvas.selectionMade(this, e, selected);
        }
        else
            System.err.println("no SelectableTextItem support for double clicks yet");
    }

    public void select(byte selkind) {
        selected = selkind;
        repaint();
    }

    protected void paintBox(Graphics g, Color c) {
        g.setColor(c); g.drawRect(left,top, right,bottom);
    }

    protected void paintSides(Graphics g) {
        g.setColor(selectionColour);
        g.drawLine(left,top, left,bottom); g.drawLine(right,top, right,bottom);
    }

    protected void paintHoriz(Graphics g, int y) {
        g.setColor(selectionColour); g.drawLine(left,y, right,y);
    }

    protected void paintDotted(Graphics g, int y) {
        int dashlength = 3*selectionthickness;
        Color background = getBackground();
        for (int i=0; i<right; i+=dashlength) {
            g.setColor((i/3)%2==0 ? selectionColour : background);
            g.drawLine(i,y, Math.min(right, i+dashlength-1),y);
        }
    }

    protected void paintHooks(Graphics g, int y) {
        int hooklength = 4*selectionthickness;
        int lefthook = hooklength-1, righthook = right-hooklength+1;
        g.setColor(selectionColour); g.drawLine(left,y, lefthook,y);
        if (lefthook+1<=righthook-1) {
            g.setColor(getBackground()); g.drawLine(lefthook+1,y, righthook-1,y);
        }
        g.setColor(selectionColour); g.drawLine(righthook,y, right,y);
    }

    /*  At present we have two selection styles.  Reasons, and all formulae in tree style,
        are selected by surrounding them with a box.  In box style hyps get a selection
        open at the bottom, concs get a selection open at the top.  Ambigs behave differently
        when clicked in different places: near the top you get a conc-style selection, near
        the bottom a hyp-style selection, but in each case the closed end of the box is a dotted line.
     */

    public void paint(Graphics g) {
        Color background = getBackground();
        // do the selection stuff
        switch (selected) {
            case JapeCanvas.NoSel:
                paintBox(g, background); break;
            case ProofCanvas.ReasonSel:
                paintBox(g, selectionColour); break;
            case ProofCanvas.HypSel:
                if (proofstyle!=ProofCanvas.BoxStyle)
                    paintBox(g, selectionColour);
                else
                if (kind==AmbigKind) {
                    paintSides(g); paintDotted(g, top); paintHooks(g, bottom);
                }
                else {
                    paintSides(g); paintHoriz(g, top); paintHooks(g, bottom);
                }
                break;
            case ProofCanvas.ConcSel:
                if (proofstyle!=ProofCanvas.BoxStyle)
                    paintBox(g, selectionColour);
                else
                if (kind==AmbigKind) {
                    paintSides(g); paintDotted(g, bottom); paintHooks(g, top);
                }
                else {
                    paintSides(g); paintHoriz(g, bottom); paintHooks(g, top);
                }
                break;
            default:
                Alert.abort("SelectableTextItem.repaint selected="+selected);
        }
        // then draw the rest of it
        super.paint(g);
    }
}
