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

public class TextSelectableItem extends TextItem {
    public Color textselectionColour = Color.yellow;

    public TextSelectableItem(JapeCanvas canvas, int x, int y, byte fontnum, 
                              String annottext, String printtext) {
        super(canvas,x,y,fontnum,annottext,printtext);
        addMouseInteractionListener(new MouseInteractionAdapter() {
            public void textpressed(byte eventKind, MouseEvent e) {
                TextSelectableItem.this.textpressed(eventKind, e);
            }
            public void textdragged(byte eventKind, MouseEvent e) {
                TextSelectableItem.this.textdragged(eventKind, e);
            }
            public void textreleased(byte eventKind, boolean isClick, MouseEvent e) {
                TextSelectableItem.this.textreleased(eventKind, isClick, e);
            }
        });
        selectionHalo = canvas.getSelectionHalo();
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
            TextSelectableItem.this.repaint(pxstart, 0, pxend-pxstart, getHeight());
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

    public void deTextSelect() {
        if (textsels!=null)
            while (textsels.size()!=0) {
                getTextSel(0).repaint();
                textsels.remove(0);
            }
    }

    // should be abstract: this is the ProofCanvas version
    public String getTextSelections() {
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
        anchor = current = pixel2Subformula(formulae, internalX(e.getX()));
        int i;
        for (i=0; i<textsels.size(); i++)
            if (anchor.start<=getTextSel(i).start)
                break;
        textsels.add(i, new TextSel(anchor));
        currenttextselindex = i;
    }
        
    protected void textpressed(byte eventKind, MouseEvent e) {
        switch (eventKind) {
            case SelectionConstants.TextSelection:
                canvas.killTextSelections(null); // kill everybody's, including mine
            case SelectionConstants.DisjointTextSelection: // don't kill any text selections?
                newTextSel(e);
                break;
                
            case SelectionConstants.ExtendedTextSelection:
                canvas.killTextSelections(this); // kill everybody else's
            case SelectionConstants.ExtendedDisjointTextSelection:
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

            default:
                System.err.println("TextSelectableItem.textpressed eventKind="+eventKind);
        }
    }

    protected void textdragged(byte eventKind, MouseEvent e) {
        FormulaTree sel = enclosingSubformula(anchor, pixel2Subformula(formulae,internalX(e.getX())));
        if (sel!=current) {
            getTextSel(currenttextselindex).reset(sel);
            current = sel;
        }
    }
    
    protected void textreleased(byte eventKind, boolean isClick, MouseEvent e) {
        TextSel current = getTextSel(currenttextselindex);
        for (int i=0; i<textsels.size(); ) {
            TextSel t = getTextSel(i);
            if (t.overlaps(current))
                textsels.remove(i);
            else
                i++;
        }
        currenttextselindex = -1;
    }

    public void paint(Graphics g) {
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
