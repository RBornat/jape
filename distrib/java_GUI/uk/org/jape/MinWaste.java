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
import java.awt.FontMetrics;
import java.util.Vector;

public class MinWaste {
    static class Fold {
	private String s;
	private FontMetrics m;
	private char[] cs;
	private int spacewidth;
	private Vector ws;

	private class W {
	    int i; int j; int width; int spacing;
	    W(int i, int j, int width, int spacing) {
		this.i=i; this.j=j; this.width=width; this.spacing=spacing;
	    }
	    public String toString() {
		return "W[i="+i+",j="+j+",width="+width+",spacing="+spacing+"]";
	    }
	}

	private F fold;
	int constraint;

	Fold(FontMetrics m, String s, int constraint) {
	    this.s = s;
	    cs = s.toCharArray();
	    spacewidth = m.charWidth(' ');
	    ws=new Vector();
	    // break on spaces, commas, semicolons or otherwise on tab-sized chunks
	    for (int i=0; i<cs.length; ) {
		int j;
		for (j=i; j<cs.length; j++)  {
		    char c = cs[j];
		    if (c==' ') break;
		    if (c==',' || c==';') { j++; break; }
		}
		int w = m.charsWidth(cs,i,j-i);
		if (w>constraint) {
		    // binary chop to find a nice spot -- we go for eight spaces
		    int constraint2 = Math.min(constraint, spacewidth*8);
		    int i1=i;
		    while (i1+1!=j) {
			int k = (i1+j)/2;
			if ((w=m.charsWidth(cs,i,j-i))<constraint2)
			    i1 = k;
			else
			    j = k;
		    }
		}
		// consume following spaces
		int j0=j;
		for ( ; j<cs.length && cs[j]==' '; j++);
		// Logger.log.print(ws.size()+": word \""+new String(cs,i,j0-i)+"\" ");
		ws.add(new W(i, j0, w, m.charsWidth(cs,j0,j-j0)));
		// Logger.log.println(ws.get(ws.size()-1));
		i=j;
	    }
	    fold = new F(0, ws.size(), null);
	    report();
	}

	public int height, width;

	private void report() {
	    height = fold.height; width = fold.maxwidth;
	}

	public void split() {
	    fold = fold.split(); report();
	}

	public String[] reportSplit() {
	    Vector v = new Vector();
	    fold.reportSplit(v);
	    return ((String[])v.toArray(new String[v.size()]));
	}

	private class F implements Cloneable {
	    private int i, j; // words from i..j-1, of course
	    private int height, width, maxwidth, waste;
	    private F next;

	    public String toString() {
		return	super.toString()+": F[i="+i+",j="+j+",height="+height+",width="+width+",maxwidth="+maxwidth+",waste="+waste+",next="+next+"]";
	    }

	    F(int i, int j, F next) {
		this.i=i; this.j=j; this.next=next;
		measurewidth(); measurewaste(); smooth();
	    }

	    // a deep copy, to make in-place modifications possible
	    protected F clone(F f) {
		return f==null ? null : (F)f.clone();
	    }

	    public Object clone() {
		F f = null; // shut up compiler
		try {
		    f = (F)super.clone();
		} catch (Exception e) {
		    Logger.crash("can't clone "+this, 2);
		}
		f.next=clone(f.next);
		return f;
	    }

	    private void measurewidth() {
		width=0;
		for (int i1=i; i1<j; i1++) {
		    width+=((W)ws.get(i1)).width;
		    if (i1+1<j)
			width+=((W)ws.get(i1)).spacing;
		}
	    }

	    private void measurewaste() {
		if (next==null) {
		    height=1; maxwidth=width; waste=0;
		}
		else {
		    waste = width<next.maxwidth ? Math.max(next.waste,next.maxwidth-width) :
						  next.waste+width-next.maxwidth;
		    maxwidth=Math.max(width,next.maxwidth);
		    height = next.height+1;
		}
	    }

	    private void take() {
		// int ww = ((W)ws.get(j)).width;
		next.i++; next.measurewidth(); next.measurewaste(); next.smooth();
		j++; measurewidth(); measurewaste();
		// Logger.log.println("after take "+this);
	    }

	    private void give() {
		// int ww = ((W)ws.get(j-1)).width;
		next.i--; next.measurewidth(); next.measurewaste(); next.smooth();
		j--; measurewidth(); measurewaste();
		// Logger.log.println("after give "+this);
	    }

	    // Oh I wish I could think of a fast undo ...
	    private void smooth() {
		if (next!=null) {
		    if (width<next.maxwidth)
			while (width<next.maxwidth && next!=null && next.i+1<next.j) {
			    int oldwaste = waste;
			    take();
			    if (waste>oldwaste) {
				give(); break;
			    }
			}
		    else // sort of repeated -- sorry
			while (width>next.maxwidth && next!=null && i+1<j) {
			    int oldwaste = waste;
			    give();
			    if (waste>oldwaste) {
				take(); break;
			    }
			}
		    // Logger.log.println("smooth done");
		}
	    }

		// the split algorithm puts some of the onus on the garbage collector ...
		public F split() {
		    F f = i+1<j	     ? new F(i,i+1,new F(i+1,j,clone(next))) :
			  next!=null ? new F(i,j,next.split()) :
				       this;
		    return f.maxwidth<maxwidth ? f : this;
		}

		public void reportSplit(Vector v) {
		    int start = ((W)ws.get(i)).i, end = ((W)ws.get(j-1)).j;
		    // Logger.log.println(v.size()+": from "+i+"("+start+") to "+(j-1)+"("+end+")"+" width="+width+", maxwidth="+maxwidth+", waste="+waste);
		    v.add(new String(cs,start,end-start));
		    if (next!=null)
			next.reportSplit(v);
		}
	}
    }

	public static String[] minwaste(Component c, String s, int width) {
	    Fold f = new Fold (c.getFontMetrics(c.getFont()), s, width);
	    while (f.width>width) {
		int oldwidth = f.width;
		f.split();
		if (f.width==oldwidth)
		    break;
	    }
	    return f.reportSplit();
	}

	
}
