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
import java.awt.event.MouseMotionAdapter;
import java.awt.event.MouseEvent;
import java.awt.Rectangle;
import java.util.Vector;

public class SelectableTextItem extends TextItem
                                implements ProofConstants, SelectionConstants {
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
                case ReasonSel:
                    paintBox(g); break;
                case HypSel:
                    if (proofstyle!=BoxStyle)
                        paintBox(g);
                    else
                        if (kind==AmbigTextItem) {
                            paintSides(g); paintDotted(g, top); paintHooks(g, bottom);
                        }
                        else {
                            paintSides(g); paintHoriz(g, top); paintHooks(g, bottom);
                        }
                        break;
                case ConcSel:
                    if (proofstyle!=BoxStyle)
                        paintBox(g);
                    else
                        if (kind==AmbigTextItem) {
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

    protected FormulaTree enclosingSubformula(FormulaTree a, FormulaTree b) {
        while (a.parent!=null && !(a.start<=b.start && b.end<=a.end)) {
            a = a.parent;
        }
        return a;
    }

    protected FormulaTree findSubformula(FormulaTree f, int start, int end) {
        if (f==null || f.start==start && f.end==end)
            return f;
        for (int i=0; i<f.children.length; i++) {
            FormulaTree f1;
            if ((f1=findSubformula(f.children[i], start, end))!=null)
                return f1;
        }
        return null;
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
        public int start, end, pxstart, pxend;
        
        public TextSel(FormulaTree f) { init(f); }
        
        public void reset(FormulaTree f) { repaint(); init(f); }
        
        private void init(FormulaTree f) {
            start=f.start; end=f.end; pxstart=f.pxstart; pxend=f.pxend;
            repaint();
        }
        
        public TextSel(int start, int end) { init(start, end); }
        
        public void reset(int start, int end) { repaint(); init(start, end); }
        
        private void init(int start, int end) {
            this.start = start; this.end = end;
            pxstart = JapeFont.charsWidth(printchars, 0, start, fontnum);
            pxend = JapeFont.charsWidth(printchars, 0, end, fontnum);
            repaint();
        }
        
        public void repaint() {
            SelectableTextItem.this.repaint(pxstart, 0, pxend-pxstart, getHeight());
        }
        
        public void paint(Graphics g) {
            g.setColor(textselectionColour);
            g.fillRect(pxstart, 0, pxend-pxstart, getHeight());
        }

        public boolean overlaps(TextSel t) {
            return t!=null && t!=this &&
                   (t.start<=this.start && this.start<=t.start ||
                    this.start<=t.start && t.start<=this.start);
        }
    }

    public Vector textsels;
    private int currenttextselindex = -1;
    FormulaTree anchor, current;

    public void removeTextSelections() {
        if (textsels!=null)
            while (textsels.size()!=0) {
                getTextSel(0).repaint();
                textsels.remove(0);
            }
    }

    public String getTextSelectionStrings() {
        if (textsels==null || textsels.size()==0)
            return null;
        else {
            StringBuffer b = new StringBuffer(printchars.length+2*textsels.size());
            int i = 0;
            for (int j=0; j<textsels.size(); j++) {
                TextSel t = getTextSel(j);
                b.append(printchars, i, t.start-i);
                b.append(Reply.stringSep);
                b.append(printchars, t.start, t.end-t.start);
                b.append(Reply.stringSep);
                i = t.end;
            }
            b.append(printchars, i, printchars.length-i);
            return b.toString();
        }
    }

    private void ensureTextSelectionVars() {
        if (formulae==null) {
            annoti = printi = 0; annotlen = annottext.length();
            formulae = computeFormulaTree((char)0);
            textsels = new Vector(1);
        }
    }

    protected TextSel getTextSel(int i) {
        return i<0 || i>=textsels.size() ? null : (TextSel)textsels.get(i);
    }

    protected void newTextSel(MouseEvent e) {
        ensureTextSelectionVars();
        anchor = current = pixel2Subformula(formulae, e.getX());
        int i;
        for (i=0; i<textsels.size(); i++)
            if (anchor.start<=getTextSel(i).start)
                break;
        textsels.add(i, new TextSel(anchor));
        currenttextselindex = i;
    }
        
    protected void pressed(byte eventKind, MouseEvent e) {
        this.eventKind = eventKind;
        mousemotionseen = false;
        switch (eventKind) {
            case TextSelection:
                canvas.killTextSelections(null); // kill everybody's, including mine
            case DisjointTextSelection: // don't kill any text selections?
                newTextSel(e);
                break;
                
            case ExtendedTextSelection:
                canvas.killTextSelections(this); // kill everybody else's
            case ExtendedDisjointTextSelection:
                ensureTextSelectionVars();
                if (textsels.size()==0)
                    newTextSel(e);
                else {
                    int i, px=e.getX();
                    for (i=0; i<textsels.size(); i++) {
                        if (px<getTextSel(i).pxend) {
                            // are we nearer to this one than the one before?
                            if (i==0 || getTextSel(i).pxstart-px<px-getTextSel(i-1).pxend)
                                i++;
                            break;
                        }
                    }
                    // we are nearest to textsel[i]
                    currenttextselindex = i;
                    anchor = findSubformula(formulae, getTextSel(i).start, getTextSel(i).end);
                    current = enclosingSubformula(anchor, pixel2Subformula(formulae, px));
                    getTextSel(i).reset(current);
                }
                break;

            case Selection:
            case ExtendedSelection:
            case DisjointSelection:
            case ExtendedDisjointSelection:
                break; // nothing happens till release, and then only sometimes
            default:
                System.err.println("SelectableTextItem.pressed eventKind="+eventKind);
        }
    }

    protected void dragged(MouseEvent e) {
        mousemotionseen = true;
        switch (eventKind) {
            case TextSelection:
            case ExtendedTextSelection:
            case DisjointTextSelection:
            case ExtendedDisjointTextSelection:
                FormulaTree sel = enclosingSubformula(anchor, pixel2Subformula(formulae, e.getX()));
                if (sel!=current) {
                    getTextSel(currenttextselindex).reset(sel);
                    current = sel;
                }
                break;
            case Selection:
            case ExtendedSelection:
            case DisjointSelection:
            case ExtendedDisjointSelection:
                break;
            default:
                System.err.println("SelectableTextItem.dragged eventKind="+eventKind);
        }
    }
    
    protected void released(MouseEvent e) {
        switch (eventKind) {
            case TextSelection:
            case ExtendedTextSelection:
            case DisjointTextSelection:
            case ExtendedDisjointTextSelection:
                {   TextSel current = getTextSel(currenttextselindex);
                    for (int i=0; i<textsels.size(); ) {
                        TextSel t = getTextSel(i);
                        if (t.overlaps(current))
                            textsels.remove(i);
                        else
                            i++;
                    }
                }
                currenttextselindex = -1;
                break;
            case Selection:
            case ExtendedSelection:
            case DisjointSelection:
            case ExtendedDisjointSelection:
                if (!mousemotionseen) {
                    byte selected;
                    if (e.getClickCount()==1) {
                        // determine selection type
                        switch (kind) {
                            case ConcTextItem:
                                selected = ConcSel; break;
                            case HypTextItem:
                                selected = HypSel; break;
                            case ReasonTextItem:
                                selected = ReasonSel; break;
                            case AmbigTextItem:
                                selected = e.getY()<getHeight()/2 ? ConcSel : HypSel;
                                break;
                            default:
                                Alert.abort("SelectableTextItem.click kind="+kind);
                                selected = NoSel; // shut up compiler
                        }
                        canvas.declareSelection(eventKind, selected);
                        select(selected);
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
        if (selectionRect!=null) {
            selectionRect.repaint();
            canvas.remove(selectionRect);
            selectionRect = null;
        }
        
        if (selkind!=NoSel) {
            selectionRect = new SelectionRect(selkind, getBounds());
            selectionRect.repaint();
        }
    }

    public void paint(Graphics g) {
        paint((Graphics2D) g);
    }
    
    public void paint(Graphics2D g) {
        if (textsels!=null) {
            TextSel current = getTextSel(currenttextselindex);
            for (int i = 0; i<textsels.size(); i++) {
                TextSel t = getTextSel(i);
                if (!t.overlaps(current))
                    t.paint(g);
            }
        }
        super.paint(g);
    }
}
