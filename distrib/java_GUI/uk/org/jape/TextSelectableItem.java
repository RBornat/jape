/*
    $Id$
    
    Copyright © 2003-4 Richard Bornat & Bernard Sufrin
    
    richard@bornat.me.uk
    sufrin@comlab.ox.ac.uk
    
    This file is part of the Jape GUI, which is part of Jape.
    
    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.
    
    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with Jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).
    
*/

package uk.org.jape;

import java.awt.Component;
import java.util.Enumeration;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseMotionAdapter;
import java.awt.event.MouseEvent;
import java.awt.Rectangle;
import java.util.Vector;

public class TextSelectableItem extends TextItem implements SelectionConstants {

    protected final AnnotatedTextComponent[] components;

    // globals for computing ColourTree, FormulaTree
    protected int	   annoti, printi; 
    protected int	   annotlen;

    public TextSelectableItem(JapeCanvas canvas, int x, int y, byte fontnum, String annottext) {
	this(canvas, x, y, 
	     new AnnotatedTextComponent[]{new AnnotatedTextComponent(0, 0, fontnum, annottext)});
    }
    
    public TextSelectableItem(JapeCanvas canvas, int x, int y, AnnotatedTextComponent[] components) {
	super(canvas,x,y,components);
	this.components = components;
	addJapeMouseListener(new JapeMouseTextAdapter() {
	    public void textpressed(byte eventKind, MouseEvent e) {
		TextSelectableItem.this.textpressed(eventKind, e);
	    }
	    public void textdragged(byte eventKind, MouseEvent e) {
		TextSelectableItem.this.textdragged(eventKind, e);
	    }
	    public void textreleased(byte eventKind, boolean isClick, MouseEvent e) {
		TextSelectableItem.this.canvas.claimFocus();
		TextSelectableItem.this.textreleased(eventKind, isClick, e);
	    }
	});
    }

    protected FormulaTree formulae;
    
    protected class FormulaTree {
	public final int start, end; // printtext indices; start<=i<end -> i is in this subformula
	public final int pxstart, pxend;
	public FormulaTree parent;
	public final FormulaTree[] children;

	public FormulaTree(int start, int end, FormulaTree[] children) {
	    this.start = start; this.end = end;
	    this.pxstart = pxval(start);
	    this.pxend = pxval(end);
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

    protected int pxval(int i) {
	int width = 0;
	for (int j=0; j<components.length; j++) {
	    AnnotatedTextComponent c = components[j];
	    if (i>c.printlen) {
		width += c.dimension.width; i -= c.printlen;
	    }
	    else
		return width+JapeFont.charsWidth(c.printchars, 0, i, c.fontnum);
	}
	Alert.abort("TextSelectableItem.pxval runs out of characters");
	return 0; // shut up compiler
    }
    
    // annoti, printi, annotlen are variables in TextItem
    protected FormulaTree computeFormulaTree(String annottext, char expectedket, int printlen) {
	int i0 = printi;
	Vector cs = new Vector();
	char c;
	while (annoti<annotlen) {
	    c = annottext.charAt(annoti++);
	    if (AnnotatedTextComponent.invisbra(c))
		cs.add(computeFormulaTree(annottext, AnnotatedTextComponent.bra2ket(c), printlen));
	    else
	    if (AnnotatedTextComponent.invisket(c)) {
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

	if (printi!=printlen)
	    Alert.abort(this+": text is "+printlen+
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

    protected FormulaTree findSubformula(FormulaTree f, TextSel t) {
	return t==null ? null : findSubformula(f, t.start, t.end);
    }

    public class TextSel {
	public int start, end, pxstart, pxend;
	private int startgap = 0, endgap = 0;
	
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
	    pxstart = pxval(start);
	    pxend = pxval(end);
	    repaint();
	}
	
	/*  repaint, paint must scan across an array of components. What an obvious
	    candidate for a function argument ... 
	    But no, we must use a ****ing iterator.
	 */
	public void repaint() {
	    initRects();
	    while (hasRect()) {
		TextSelectableItem.this.repaint(rX, rY, rW, rH);
		nextRect();
	    }
	}
	
	public void paint(Graphics g) {
	    if (isEnabled()) {
		if (paint_tracing)
		    Logger.log.println("painting text selection "+start+","+end);
		g.setColor(Preferences.TextSelectionColour);
		initRects();
		while (hasRect()) {
		    g.fillRect(rX, rY, rW, rH);
		    nextRect();
		}
	    }
	}
	
	private int rX, rY, rW, rH, rStart, rEnd, rI;
	
	private void initRects() {
	    rStart = pxstart; rEnd = pxend; rI=0;
	    while (rStart>=components[rI].dimension.width) {
		rStart -= components[rI].dimension.width; 
		rEnd -= components[rI].dimension.width;
		rI++;
	    }
	    rStart += startgap;
	}
	
	private boolean hasRect() {
	    if(rStart<rEnd) {
		rX = components[rI].offX+rStart;
		rY = ascent+components[rI].offY-components[rI].dimension.ascent;
		rW = (rEnd>components[rI].dimension.width ? components[rI].dimension.width
							  : rEnd-endgap)
		    -rStart;
		rH = components[rI].dimension.ascent+components[rI].dimension.descent;
		return true;
	    }
	    else
		return false;
	}
	
	private void nextRect() {
	    if (rStart<rEnd) {
		if (rEnd>components[rI].dimension.width) {
		    rStart = 0;
		    rEnd -= components[rI].dimension.width;
		    rI++;
		}
		else
		    rStart = rEnd;
	    }
	}
	
	/* Overlap doesn't include touching */
	public boolean overlaps(TextSel t) {
	    return t!=null && t!=this && t.end>this.start && this.end>t.start;
	}

	public String toString() {
	    return "TextSel[start="+start+"; end="+end+"; pxstart="+pxstart+"; pxend="+pxend+"]";
	}
    }

    public Vector textsels;
    private int currenttextselindex = -1;
    FormulaTree anchor, current;

    public void deTextSelect() {
	if (textsels!=null && textsels.size()!=0) {
	    while (textsels.size()!=0) {
		getTextSel(0).repaint();
		textsels.remove(0);
	    }
	    canvas.getProofWindow().enableCopy();
	}
    }
    
    // this is slow -- don't use too often
    protected char[] getprintchars() {
	StringBuffer printtext = new StringBuffer();
	for (int i=0; i<components.length; i++) {
	    printtext.append(components[i].printtext);
	}
	return printtext.toString().toCharArray();
    }

    public String getContextualisedTextSelections() {
	if (textsels==null || textsels.size()==0)
	    return null;
	else {
	    char[] printchars = getprintchars();
	    StringBuffer b = new StringBuffer(printchars.length+2*textsels.size());
	    int i = 0;
	    for (int j=0; j<textsels.size(); j++) {
		TextSel t = getTextSel(j);
		try {
		    b.append(printchars, i, t.start-i);
		    b.append(Reply.stringSep);
		    b.append(printchars, t.start, t.end-t.start);
		    b.append(Reply.stringSep);
		    i = t.end;
		} catch (Exception e) {
		    Logger.log.println("exception "+e+" in getContextualisedTextSelections\n"+this);
		}
	    }
	    b.append(printchars, i, printchars.length-i);
	    return b.toString();
	}
    }

    public String getTextSelections() {
	if (textsels==null || textsels.size()==0)
	    return null;
	else {
	    char[] printchars = getprintchars();
	    StringBuffer b = new StringBuffer(printchars.length+2*textsels.size());
	    for (int j=0; j<textsels.size(); j++) {
		TextSel t = getTextSel(j);
		if (j!=0)
		    b.append(Reply.stringSep);
		b.append(printchars, t.start, t.end-t.start);
	    }
	    return b.toString();
	}
    }

    public String getSingleTextSelection() {
	return textsels==null || textsels.size()!=1 ?  null : getTextSelections();
    }
    
    public int getTextSelectionCount() {
	return textsels==null ? 0 : textsels.size();
    }

    // don't do this very often
    protected String getannottext() {
	StringBuffer annottext = new StringBuffer();
	annotlen = 0;
	for (int i=0; i<components.length; i++) {
	    annottext.append(components[i].annottext);
	    annotlen += components[i].annotlen;
	}
	return annottext.toString();
    }

    // nor this
    protected int getprintlen() {
	int printlen = 0;
	for (int i=0; i<components.length; i++)
	    printlen = printlen+components[i].printlen;
	return printlen;
    }
    
    private void ensureTextSelectionVars() {
	if (formulae==null) {
	    annoti = printi = 0;
	    formulae = computeFormulaTree(getannottext(), (char)0, getprintlen());
	    textsels = new Vector(1);
	    canvas.getProofWindow().enableCopy();
	}
    }

    protected TextSel getTextSel(int i) {
	return i<0 || i>=textsels.size() ? null : (TextSel)textsels.get(i);
    }

    protected int getTextPoint(MouseEvent e) {
	int ci=0, px=internalX(e.getX()), py=internalY(e.getY());
	// Logger.log.print("getTextPoint("+e+") ("+px+","+py+")");
	while (ci<components.length && 
	       py>ascent+components[ci].offY+components[ci].dimension.descent) ci++;
	if (ci==components.length ||
	    (ci!=0 && (py-(ascent+components[ci-1].offY+components[ci-1].dimension.descent)<
			ascent+components[ci].offY-components[ci].dimension.ascent-py))) 
	    ci--;
	for (int i=0; i<ci; i++)
	    px += components[i].dimension.width;
	// Logger.log.println(" => "+px);
	return px;
    }
    
    protected void newTextSel(MouseEvent e) {
	ensureTextSelectionVars();
	anchor = current = pixel2Subformula(formulae, getTextPoint(e));
	int i;
	for (i=0; i<textsels.size(); i++)
	    if (anchor.start<=getTextSel(i).start)
		break;
	textsels.add(i, new TextSel(anchor));
	currenttextselindex = i;
	canvas.getProofWindow().enableCopy();
    }

    private int pixel2TextSelIndex(int x) {
	ensureTextSelectionVars();
	for (int i=0; i<textsels.size(); i++) {
	    TextSel t = getTextSel(i);
	    // Logger.log.println("enclosingTextSelIndex("+x+") looking at "+t);
	    if (t.pxstart<=x && x<t.pxend)
		return i;
	}
	return -1;
    }

    public void setTextSels(String[] sels) throws ProtocolError { // used in disproof sequent
	ensureTextSelectionVars();
	int i=0, start=0, end;
	while (true) {
	    if (sels.length-i<3)
		throw (new ProtocolError("must be 3, 5, 7, ... number of strings"));
	    start += sels[i].length();
	    end = start+sels[i+1].length();
	    FormulaTree f = findSubformula(formulae, start, end);
	    if (f==null)
		throw (new ProtocolError("can't find selection "+((i+1)/2)));
	    textsels.add(new TextSel(f));
	    start = end;
	    i+=2;
	    if (sels.length-i==1)
		break;
	}
	repaint();
    }

    private boolean oldsel;
    
    protected void textpressed(byte eventKind, MouseEvent e) {
	// Logger.log.println("textpressed eventKind="+eventKind);
	switch (eventKind) {
	    case TextSelection:
		canvas.killTextSelections(null); // kill everybody's, including mine
		newTextSel(e);
		break;

	    case DisjointTextSelection: { // don't kill any text selections
		    int x = getTextPoint(e);
		    currenttextselindex = pixel2TextSelIndex(x);
		    // Logger.log.println("textpressed DisjointTextSelection x="+x
		    //			  +"; currenttextselindex="+currenttextselindex);
		    if (currenttextselindex==-1) {
			newTextSel(e); oldsel=false;
		    }
		    else {
			TextSel t = getTextSel(currenttextselindex);
			anchor = findSubformula(formulae, t);
			current = enclosingSubformula(anchor, pixel2Subformula(formulae, x));
			t.reset(current);
			oldsel = true;
		    }
		}
		break;
		
	    case ExtendedTextSelection:
		canvas.killTextSelections(this); // kill everybody else's
	    case ExtendedDisjointTextSelection:
		ensureTextSelectionVars();
		if (textsels.size()==0)
		    newTextSel(e);
		else {
		    int i, px=getTextPoint(e);
		    for (i=0; i<textsels.size(); i++) {
			if (px<getTextSel(i).pxend) {
			    // are we nearer to this one or the one before?
			    if (i!=0 && getTextSel(i).pxstart-px>px-getTextSel(i-1).pxend)
				i--;
			    break;
			}
		    }
		    // we are nearest to textsel[i], if there is one
		    currenttextselindex = Math.min(i, textsels.size()-1);
		    anchor = findSubformula(formulae, getTextSel(currenttextselindex));
		    current = enclosingSubformula(anchor, pixel2Subformula(formulae, internalX(px)));
		    getTextSel(currenttextselindex).reset(current);
		}
		break;

	    default:
		Logger.log.println("?? TextSelectableItem.textpressed eventKind="+eventKind);
	}
    }

    protected void textdragged(byte eventKind, MouseEvent e) {
	FormulaTree sel = enclosingSubformula(anchor, pixel2Subformula(formulae,getTextPoint(e)));
	if (sel!=current) {
	    getTextSel(currenttextselindex).reset(sel);
	    current = sel;
	}
    }
    
    protected void textreleased(byte eventKind, boolean isClick, MouseEvent e) {
	TextSel current = getTextSel(currenttextselindex);
	// Logger.log.println("textreleased isClick="+isClick+"; eventKind="+eventKind+"; oldsel="+oldsel);
	if (isClick && eventKind==DisjointTextSelection && oldsel)
	    textsels.remove(currenttextselindex);
	else {
	    for (int i=0; i<textsels.size(); ) {
		TextSel t = getTextSel(i);
		if (t.overlaps(current))
		    textsels.remove(i);
		else
		    i++;
	    }
	}
	for (int i=0; i<textsels.size()-1; i++) {
	    TextSel t1 = getTextSel(i), t2 = getTextSel(i+1);
	    t1.endgap = t2.startgap = (t1.end==t2.start ? 1 : 0);
	}
	if (textsels.size()!=0)
	    getTextSel(0).startgap = getTextSel(textsels.size()-1).endgap = 0;
	repaint();
	currenttextselindex = -1;
	canvas.getProofWindow().enableCopy();
	canvas.notifyTextSelectionChange(this);
    }

    protected void paintTextSels(Graphics g) {
	if (textsels!=null) {
	    TextSel current = getTextSel(currenttextselindex);
	    for (int i = 0; i<textsels.size(); i++) {
		TextSel t = getTextSel(i);
		if (!t.overlaps(current))
		    t.paint(g);
	    }
	}
    }
    
    public void paint(Graphics g) {
	if (isEnabled()) {
	    if (paint_tracing)
		Logger.log.println("painting textselectable item at "+getX()+","+getY());
	    paintTextSels(g);		  
	}
	super.paint(g);
    }

    public String toString() {
	String s = "TextSelectableItem["+
	"formulae="+formulae+
	", currenttextselindex="+currenttextselindex+
	", textsels=";
	if (textsels==null)
	    s = s+"null";
	else {
	    s = s+"[";
	    for (int i=0; i<textsels.size(); i++) {
		s = s+getTextSel(i);
		if (i+1<textsels.size())
		    s = s+", ";
	    }
	    s = s+"]";
	}
	return s+", "+super.toString()+"]";
    }
    
    public void greyen() {
	deTextSelect();
	super.greyen();
    }
}
