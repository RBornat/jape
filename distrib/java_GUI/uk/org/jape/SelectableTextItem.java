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
import java.awt.Graphics2D;
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

    /*
        Bernard's original ProofCanvas included this comment:
    
            This next spasm copes with the fact that Macs have 1-button
            mice, and that (in order to cope with this) Java AWT
            reports button 2 and button 3 presses AS IF they were
            presses with one of Meta or Alt pressed. I simply don't know
            whether the getButton field of an event on the Mac always
            says 1, or whether the lowlevel AWTery reports virtual
            buttons. Until I find out, I'm assuming the former, and using
            the Alt and Meta fields to give an indication of the button
            that was actually pressed (on nonMacs) or the (virtual) button
            that a MacIsta indicated that she wanted to press.
        
            Beyond here we're simply pretending we have a 3-button mouse.
    
            ---------------
    
        and this code:
        
            // Assigns the right virtual button for all but a move
            lastButton = 1;
            if (e.isAltDown())  lastButton=2;
            else
            if (e.isMetaDown()) lastButton=3;
    
            -----------------
    
        from which I hope we may deduce the translation of keys and buttons.
    
        From this point on I'm using isAltDown and isMetaDown :-).
     */

    /*
        It would be nice if click meant select me (as it does), and press-and-drag
        meant text-select me (as it doesn't for us, but it does in every editor).
    
        The drawbacks, apart from incompatibility with Actually Existing Jape, would
        be (a) impossible to select a token with a single click; (b) a modifier key /
        alternative button needed for drag-n-drop, when I come to it.
    
        Ho hum, decisions, decisions.  I'm going to try press-and-drag for text selection
        till I find out how it feels.
     */

    // you get a click event if you press the mouse at a particular point, move it and then
    // move back to the same point!  Well blow me down: we're not having that.
    
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

    // Java doesn't support wide lines, says the 1.1 docs ...
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
        paint((Graphics2D) g);
    }
    
    public void paint(Graphics2D g) {
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
