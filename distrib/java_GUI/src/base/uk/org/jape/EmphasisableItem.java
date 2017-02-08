/* 
    Copyright © 2003-17 Richard Bornat & Bernard Sufrin
	
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

import java.awt.Color;
import java.awt.Graphics;

import java.util.Vector;

@SuppressWarnings("serial")
public class EmphasisableItem extends TextSelectableItem {

    protected EmphasisLine emphasisLine;

    protected final DisproofCanvas canvas;

    public EmphasisableItem(DisproofCanvas canvas, int x, int y, 
			    AnnotatedTextComponent[] components) {
	super(canvas,x,y,components);
	this.canvas = canvas;
	emphasisLine = new EmphasisLine(false, getX(),
			    getY()+getHeight()+canvas.getSelectionGap()+canvas.linethickness,
			    getWidth());
	canvas.add(emphasisLine);
	addSelectionIndicator(new RectSelection(this));
	
	annoti = printi = 0; 
	Vector<ColourSeg> cs = new Vector<ColourSeg>();
	computeColourSegs(getannottext(), (char)0, JapePrefs.TextColour, false, cs, getprintlen());
	coloursegs = (ColourSeg[])cs.toArray(new ColourSeg[cs.size()]);
	if (colourseg_tracing) {
	    String annottext = getannottext();
	    char[] printchars = getprintchars();
	    String annotttext = getannottext();
	    Logger.log.print("text="+JapeUtils.enQuote(new String(printchars))+"; annottext=\"");
	    for (int i=0; i<annottext.length(); i++) {
		char c = annottext.charAt(i);
		Logger.log.print(AnnotatedTextComponent.annotatedString_of_char(c));
	    }
	    Logger.log.print("\"; coloursegs=[");
	    for (int i=0; i<coloursegs.length; i++) {
		Logger.log.print(coloursegs[i]);
		if (i+1<coloursegs.length)
		    Logger.log.print(", ");
	    }
	    Logger.log.println("]");
	}
    }

    protected class ColourSeg {
	public final Color colour;
	public final int start, pxstart;
	public int end;     // not final -- this is odd ...

	public ColourSeg(Color colour, int start, int end) {
	    this.colour=colour;
	    this.start=start; this.end=end;
	    this.pxstart=pxval(start);
	}

	public void paint(Graphics g) {
	    int start = this.start, pxstart = this.pxstart, end = this.end;
	    if (DebugVars.paint_tracing)
		Logger.log.println("painting colour segment "+start+","+end);
	    int ci = 0;
	    while (start>components[ci].printlen) {
		start -= components[ci].printlen;
		end -= components[ci].printlen;
		pxstart -= components[ci].dimension.width;
		ci++;
	    }
	    g.setColor(colour);
	    while (start<end) {
		AnnotatedTextComponent c = components[ci];
		g.setFont(c.font);
		int px = c.offX+pxstart, py = c.offY+ascent;
		if (end<=c.printlen) {
		    g.drawChars(c.printchars, start, end-start, px, py);
		    start = end;
		}
		else {
		    g.drawChars(c.printchars, start, c.printlen-start, px, py);
		    start = 0; end -= c.printlen;
		    pxstart = 0;
		    ci++;
		}
	    }
	}

	public String toString() {
	    return "ColourSeg[colour="+
	    (colour==JapePrefs.OutColour    ? "OutColour"	 :
	     colour==JapePrefs.ForcedColour ? "ForcedColour"	 :
	     colour==JapePrefs.TextColour   ? "Off/TextColour" :
	     "??"+colour       )+
	    ", start="+start+
	    ", end="+end+
	    ", pxstart="+pxstart+
	    ", chars="+JapeUtils.enQuote((new String(getprintchars(), start, end-start)))+
	    "]";
	}
    }

    protected ColourSeg[] coloursegs;

    static Color bra2TextColour(char c) {
	return  c==AnnotatedTextComponent.onbra	 ?  JapePrefs.ForcedColour    :
		c==AnnotatedTextComponent.offbra ?  JapePrefs.TextColour      :
		/* c==outbra assumed */		    JapePrefs.OutColour       ;
    }

    private void extendColourSeg(Vector<ColourSeg> cs, Color colour, int start, int end) {
	if (cs.size()!=0) {
	    ColourSeg cseg = (ColourSeg)cs.lastElement();
	    if (cseg.colour.equals(colour) && cseg.end==start) {
		cseg.end=end; return;
	    }
	}
	if (start!=end)
	    cs.add(new ColourSeg(colour, start, end));
    }

    protected void computeColourSegs(String annottext, char expectedket, 
				     Color colour, boolean locked, Vector<ColourSeg> cs,
				     int printlen) {
	int i0 = printi;
	char c;
	while (annoti<annotlen) {
	    c = annottext.charAt(annoti++);
	    if (AnnotatedTextComponent.invisbra(c)) {
		extendColourSeg(cs, colour, i0, printi);
		boolean newlocked = locked || c==AnnotatedTextComponent.lockbra;
		Color newcolour = newlocked ? colour : bra2TextColour(c);
		char newket = AnnotatedTextComponent.bra2ket(c);
		computeColourSegs(annottext, newket, newcolour, newlocked, cs, printlen);
		i0 = printi;
	    }
	    else
		if (AnnotatedTextComponent.invisket(c)) {
		    if (c==expectedket) {
			extendColourSeg(cs, colour, i0, printi); return;
		    }
		    else
			Alert.abort("TextItem.computeColourSegs saw "+(int)c+
				    ", expected "+(int)expectedket);
		}
	    else
		printi++;
	}
	if (expectedket!=0)
	    Alert.abort(this+": computeColourSegs exhausted text, "+
			", expected "+(int)expectedket);

	if (printi!=printlen)
	    Alert.abort(this+": text is "+printlen+
			" chars, but computeColourSegs thinks it's "+printi);

	extendColourSeg(cs, colour, i0, printi);
	return;
    }

    public void paint(Graphics g) {
	if (isEnabled()) {
	    if (DebugVars.paint_tracing)
		Logger.log.println("painting EmphasisableItem at "+getX()+","+getY());
	    paintTextSels(g);
	    int len = coloursegs.length;
	    for (int i=0; i<len; i++)
		coloursegs[i].paint(g);
	}
	else
	    super.paint(g);
    }

    protected class EmphasisLine extends LineItem {

	private boolean emphasised;

	EmphasisLine(boolean emphasised, int x1, int y1, int width) {
	    super(EmphasisableItem.this.canvas, x1, y1, x1+width, y1);
	    setForeground(JapePrefs.ForcedColour);
	    this.emphasised = emphasised;
	}

	public void setlinethickness(int linethickness) {
	    super.setlinethickness(linethickness);
	    EmphasisableItem.this.canvas.computeBounds(); // because it might have been 0
	}

	public void paint(Graphics g) {
	    if (DebugVars.paint_tracing)
		Logger.log.println("painting emphasis at "+getX()+","+getY()+"; "+emphasised);
	    if (emphasised)
		super.paint(g);
	}

	public void emphasise(boolean emphasised) {
	    this.emphasised = emphasised; repaint();
	}
    }

    public void emphasise(boolean emphasised) {
	emphasisLine.emphasise(emphasised);
    }

    public String toString() {
	String s = "EmphasisableItem["+super.toString()+"; coloursegs=[";
	for (int i=0; i<coloursegs.length; i++) {
	    s=s+coloursegs[i];
	    if (i+1<coloursegs.length)
		s=s+",";
	}
	return s+"]]";
    }
}
