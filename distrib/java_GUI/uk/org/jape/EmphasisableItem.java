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

import java.awt.Color;
import java.awt.Graphics;

import java.util.Vector;

public class EmphasisableItem extends TextSelectableItem {

    protected EmphasisLine emphasisLine;

    protected final DisproofCanvas canvas;

    public EmphasisableItem(DisproofCanvas canvas, int x, int y, byte fontnum, String annottext,
			    byte selectionKind) {
	super(canvas,x,y,fontnum,annottext);
	this.canvas = canvas;
	emphasisLine = new EmphasisLine(false, getX(),
			    getY()+getHeight()+canvas.getSelectionGap()+canvas.linethickness,
			    getWidth());
	canvas.add(emphasisLine);
	addSelectionIndicator(new RectSelection(this));
	
	annoti = printi = 0; 
	Vector cs = new Vector();
	computeColourSegs((char)0, Preferences.TextColour, false, cs);
	coloursegs = (ColourSeg[])cs.toArray(new ColourSeg[cs.size()]);
	if (colourseg_tracing) {
	    Logger.log.print("text=\""+text+"; annottext=\"");
	    for (int i=0; i<annottext.length(); i++) {
		char c = annottext.charAt(i);
		Logger.log.print(c==onbra   ? "*ON("  :
				 c==onket   ? ")NO*"  :
				 c==offbra  ? "*OFF("  :
				 c==offket  ? ")FFO*"  :
				 c==outbra  ? "*OUT("  :
				 c==outket  ? ")TUO*"  :
				 c==lockbra ? "*LOCK(" :
				 c==lockket ? ")KCOL*" :
				 String.valueOf(c));
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
	public int end;

	public ColourSeg(Color colour, int start, int end) {
	    this.colour=colour;
	    this.start=start; this.end=end;
	    this.pxstart=JapeFont.charsWidth(printchars, 0, start, fontnum);
	}

	public void paint(Graphics g) {
	    if (paint_tracing)
		Logger.log.println("painting colour segment "+start+","+end);
	    g.setColor(colour);
	    g.drawChars(printchars, start, end-start, pxstart, dimension.ascent);
	}

	public String toString() {
	    return "ColourSeg[colour="+
	    (colour==Preferences.OutColour    ? "OutColour"	 :
	     colour==Preferences.ForcedColour ? "ForcedColour"	 :
	     colour==Preferences.TextColour   ? "Off/TextColour" :
	     "??"+colour       )+
	    ", start="+start+
	    ", end="+end+
	    ", pxstart="+pxstart+
	    ", chars=\""+(new String(printchars, start, end-start))+"\""+
	    "]";
	}
    }

    protected ColourSeg[] coloursegs;

    static Color bra2TextColour(char c) {
	return c==onbra	 ? Preferences.ForcedColour :
	c==offbra ? Preferences.TextColour :
	/* c==outbra assumed */ Preferences.OutColour;
    }

    private void extendColourSeg(Vector cs, Color colour, int start, int end) {
	if (cs.size()!=0) {
	    ColourSeg cseg = (ColourSeg)cs.lastElement();
	    if (cseg.colour.equals(colour) && cseg.end==start) {
		cseg.end=end; return;
	    }
	}
	if (start!=end)
	    cs.add(new ColourSeg(colour, start, end));
    }

    protected void computeColourSegs(char expectedket, Color colour, boolean locked, Vector cs) {
	int i0 = printi;
	char c;
	while (annoti<annotlen) {
	    c = annottext.charAt(annoti++);
	    if (invisbra(c)) {
		extendColourSeg(cs, colour, i0, printi);
		boolean newlocked = locked || c==lockbra;
		Color newcolour = newlocked ? colour : bra2TextColour(c);
		char newket = bra2ket(c);
		computeColourSegs(newket, newcolour, newlocked, cs);
		i0 = printi;
	    }
	    else
		if (invisket(c)) {
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

	if (printi!=printchars.length)
	    Alert.abort(this+": text is "+printchars.length+
			" chars, but computeColourSegs thinks it's "+printi);

	extendColourSeg(cs, colour, i0, printi);
	return;
    }

    public void paint(Graphics g) {
	if (isEnabled()) {
	    if (paint_tracing)
		Logger.log.println("painting EmphasisableItem at "+getX()+","+getY());
	    paintTextSels(g);
	    g.setFont(font);
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
	    setForeground(Preferences.ForcedColour);
	    this.emphasised = emphasised;
	}

	public void setlinethickness(int linethickness) {
	    super.setlinethickness(linethickness);
	    EmphasisableItem.this.canvas.computeBounds(); // because it might have been 0
	}

	public void paint(Graphics g) {
	    if (paint_tracing)
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
