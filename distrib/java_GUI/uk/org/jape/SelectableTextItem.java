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
import java.awt.Component;
import java.util.Enumeration;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.Rectangle;
import java.util.Vector;

public class SelectableTextItem extends TextItem implements SelectionConstants {
    public Color selectionColour     = Color.red,
                 textselectionColour = Color.yellow;

    protected final byte kind;
    protected byte proofstyle;
    protected int selectionthickness;
    protected SelectionRect selectionRect = null;

    public SelectableTextItem(JapeCanvas canvas, int x, int y, byte fontnum, byte kind,
                              String annottext, String printtext,
                              byte proofstyle, int selectionthickness) {
        super(canvas,x,y,fontnum,annottext,printtext);
        this.kind=kind; this.proofstyle=proofstyle; this.selectionthickness=selectionthickness;
        // make room for selection rectangle
        

        addMouseListener(new MouseAdapter() {
            /*
                void mouseClicked(MouseEvent e)
                    Invoked when the mouse has been clicked on a component.
                void mouseDragged(MouseEvent e)
                    Invoked when a mouse button is pressed on a component and then dragged.
                void mouseEntered(MouseEvent e)
                    Invoked when the mouse enters a component.
                void mouseExited(MouseEvent e)
                    Invoked when the mouse exits a component.
                void mouseMoved(MouseEvent e)
                    Invoked when the mouse button has been moved on a component (with no buttons no down).
                void mousePressed(MouseEvent e)
                    Invoked when a mouse button has been pressed on a component.
                void mouseReleased(MouseEvent e)
                    Invoked when a mouse button has been released on a component.

                All reasonable, except that (experimentally) mouseClicked seems to mean mouseReleased
                in the same place as mousePressed ...
             */
            public void mousePressed(MouseEvent e) {
                pressed(LocalSettings.mouseDownKind(e), e);
            }
            public void mouseDragged(MouseEvent e) {
                dragged(e);
            }
            public void mouseReleased(MouseEvent e) {
                released(e);
            }
        });
    }

    protected class SelectionRect extends Component {
        public final byte selkind;
        private final int left = 0, top = 0, right, bottom;

        SelectionRect(byte selkind, Rectangle bounds) {
            super();
            this.selkind = selkind;
            int inset = 2*selectionthickness;
            bounds.grow(inset,inset);
            setBounds(bounds);
            right = bounds.width-selectionthickness; bottom = bounds.height-selectionthickness;
            canvas.add(this);
        }

        // Java doesn't support wide lines, says the 1.1 docs ...
        protected void paintBox(Graphics g) {
            g.drawRect(left,top, right,bottom);
        }

        protected void paintSides(Graphics g) {
            g.drawLine(left,top, left,bottom); g.drawLine(right,top, right,bottom);
        }

        protected void paintHoriz(Graphics g, int y) {
            g.drawLine(left,y, right,y);
        }

        protected void paintDotted(Graphics g, int y) {
            int dashlength = 3*selectionthickness;
            for (int i=0; i<right; i+=dashlength) {
                if ((i/3)%2==0) g.drawLine(i,y, Math.min(right, i+dashlength-1),y);
            }
        }

        protected void paintHooks(Graphics g, int y) {
            int hooklength = 4*selectionthickness;
            int lefthook = hooklength-1, righthook = right-hooklength+1;
            g.drawLine(left,y, lefthook,y);
            g.drawLine(righthook,y, right,y);
        }

        public void paint(Graphics g) {
            /*  At present we have two selection styles.  Reasons, and all formulae in tree style,
                are selected by surrounding them with a box.  In box style hyps get a selection
                open at the bottom, concs get a selection open at the top.  Ambigs behave differently
                when clicked in different places: near the top you get a conc-style selection, near
                the bottom a hyp-style selection, but in each case the closed end of the box is a
                dotted line.
            */

            g.setColor(selectionColour);

            switch (selkind) {
                case ProofCanvas.ReasonSel:
                    paintBox(g); break;
                case ProofCanvas.HypSel:
                    if (proofstyle!=ProofCanvas.BoxStyle)
                        paintBox(g);
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
                        paintBox(g);
                    else
                        if (kind==AmbigKind) {
                            paintSides(g); paintDotted(g, bottom); paintHooks(g, top);
                        }
                        else {
                            paintSides(g); paintHoriz(g, bottom); paintHooks(g, top);
                        }
                        break;
                default:
                    Alert.abort("SelectableTextItem.SelectionRect selkind="+selkind);
            }
        }
    }

    protected FormulaTree formulae;
    
    protected class FormulaTree {
        public final int start, end; // start<=i<end -> i is in this subformula
        public final int pxstart, pxend;
        public FormulaTree parent;
        public final FormulaTree[] children;

        public FormulaTree(int start, int end, FormulaTree[] children) {
            this.start = start; this.end = end;
            this.pxstart = JapeFont.charsWidth(printchars, 0, start, fontnum);
            this.pxend = JapeFont.charsWidth(printchars, 0, end, fontnum);
            this.children = children;
            for (int i=0; i<children.length; i++)
                children[i].parent = this;
        }

        public String toString() {
            String s = "FT["+start+","+end+" "+pxstart+","+pxend+" [";
            for (int i=0; i<children.length; i++)
                s = s+children[i]+(i==children.length-1 ? "" : ",");
            return s+"]]";
        }
    }

    // annoti, printi, annotlen are variables in TextItem
    protected FormulaTree computeFormulaTree(char expectedket) {
        int i0 = printi;
        Vector cs = new Vector();
        char c;
        while (annoti<annotlen) {
            c = annottext.charAt(annoti++);
            if (invisbra(c))
                cs.add(computeFormulaTree(bra2ket(c)));
            else
            if (invisket(c)) {
                if (c==expectedket)
                    return computeFormulaTreeResult(i0, cs);
                else
                    Alert.abort("computeFormulaTree saw "+(int)c+", expected "+(int)expectedket);
            }
            else
                printi++;
        }
        
        if (expectedket!=0)
            Alert.abort(this+": computeFormulaTree exhausted text, "+
                        ", expected "+(int)expectedket);

        if (printi!=printchars.length)
            Alert.abort(this+": text is "+printchars.length+
                        " chars, but computeFormulaTree thinks it's "+printi);

        return computeFormulaTreeResult(i0, cs);
    }

    protected FormulaTree computeFormulaTreeResult(int i0, Vector cs) {
        if (cs.size()==1) {
            FormulaTree child = (FormulaTree)cs.get(0);
            if (child.start==i0 && child.end==printi)
                return child;
        }

        return new FormulaTree(i0, printi, (FormulaTree[])cs.toArray(new FormulaTree[cs.size()]));
    }

    protected FormulaTree pixel2Subformula(FormulaTree f, int px) {
        if (px<f.pxstart || px>f.pxend)
            return null;
        else {
            FormulaTree f1;
            for (int i=0; i<f.children.length; i++)
                if ((f1 = pixel2Subformula(f.children[i],px))!=null)
                    return f1;
            return f;
        }
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
    
        I'm using LocalSettings to get this right on different machines.
     */

    /*
        It might be nice if click meant select me (as it does), and press-and-drag
        meant text-select me (as it doesn't for us, but it does in every editor).
    
        The drawbacks, apart from incompatibility with Actually Existing Jape, would
        be (a) impossible to select a token with a single click; (b) a modifier key /
        alternative button needed for drag-n-drop, when I come to it.
    
        I've thought about it.  Essentially normal click (select, move when it's implemented)
        works on the whole object, text click (select ranges of text) works within the object.
        They are two different things, and I shouldn't confuse them.  No doubt that was the
        reason for the original design ...
     */

    // you get a click event if you press the mouse at a particular point, move it and then
    // move back to the same point!  Well blow me down: we're not having that.

    protected boolean mousemotionseen = false;
    protected byte eventKind;

    public class TextSel {
        public final int start, end, pxstart, pxend;
        public TextSel(FormulaTree f) {
            start=f.start; end=f.end; pxstart=f.pxstart; pxend=f.pxend;
        }
        public TextSel(int start, int end) {
            this.start = start; this.end = end;
            pxstart = JapeFont.charsWidth(printchars, 0, start, fontnum);
            pxend = JapeFont.charsWidth(printchars, 0, end, fontnum);
        }
        public void repaint() { repaintFromTextSel(pxstart, 0, pxend-pxstart, getHeight()); }
        public void paint(Graphics g) {
            g.setColor(textselectionColour);
            g.fillRect(pxstart, 0, pxend-pxstart, getHeight());
        }
    }

    private void repaintFromTextSel(int x, int y, int width, int height) {
        repaint(x, y, width, height);
    }

    public Vector textsels;
    
    private void ensureTextSelectionVars() {
        if (formulae==null) {
            annoti = printi = 0; annotlen = annottext.length();
            formulae = computeFormulaTree((char)0);
            textsels = new Vector(1);
        }
    }
    
    protected void pressed(byte eventKind, MouseEvent e) {
        this.eventKind = eventKind;
        mousemotionseen = false;
        switch (eventKind) {
            case TextSelection:
                canvas.killTextSelections(null); // i.e. kill the lot
                ensureTextSelectionVars();
                TextSel t = new TextSel(pixel2Subformula(formulae, e.getX()));
                textsels.add(t);
                t.repaint();
                break;
            case Selection:
                break; // nothing till release
            default:
                System.err.println("SelectableTextItem.pressed eventKind="+eventKind);
        }
    }

    protected void dragged(MouseEvent e) {
        mousemotionseen = true;
        switch (eventKind) {
            case TextSelection:
        }
    }
    
    protected void released(MouseEvent e) {
        switch (eventKind) {
            case TextSelection:
                break;
            case Selection:
                if (!mousemotionseen) {
                    byte selected;
                    if (selectionRect==null) {
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
                        canvas.declareSelection(this, e, selected);
                    }
                    else
                        System.err.println("no SelectableTextItem support for double clicks yet");
                }
                break;
            default:
                System.err.println("SelectableTextItem.release eventKind="+eventKind);
                break;
        }
    }

    public void select(byte selkind) {
        if (selkind==ProofCanvas.NoSel) {
            if (selectionRect!=null) {
                selectionRect.repaint();
                canvas.remove(selectionRect);
                selectionRect = null;
            }
        }
        else {
            selectionRect = new SelectionRect(selkind, getBounds());
            selectionRect.repaint();
        }
    }

    public void paint(Graphics g) {
        paint((Graphics2D) g);
    }
    
    public void paint(Graphics2D g) {
        if (textsels!=null) {
            for (Enumeration e = textsels.elements(); e.hasMoreElements(); ) {
                ((TextSel)e.nextElement()).paint(g);
            }
        }

        /* canvas.fonts[fontnum].setGraphics(g);
        // Background painting
        int[]   sel    = marked.runs();
        boolean normal = true;
        int     here   = position.x+canvas.textInset.width;
        int     there;

        g.setColor(Color.green);
        for (int i=0; i<sel.length; i++) {
            there = boundaries[sel[i]];
            if (!normal) g.fillRect(here, position.y, there-here, bounds.height);
            normal = !normal;
            here = there;
        }

        g.setColor(greyed?canvas.getGreyedColour():canvas.getNormalColour());
        */

        // then draw the rest of it
        super.paint(g);
    }
}
