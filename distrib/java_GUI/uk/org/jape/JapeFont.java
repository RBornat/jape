/* 
    $Id$

    Copyright � 2002 Richard Bornat & Bernard Sufrin
     
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

import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.util.HashMap;
import javax.swing.JLabel;
import java.util.Vector;

public class JapeFont {
    public static TextDimension measure(JLabel l, String s) {
        FontMetrics m = l.getFontMetrics(l.getFont());
        return new TextDimension(m.stringWidth(s), m.getMaxAscent(), m.getMaxDescent());
    }
    
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
            // it's good enough to use word lengths
            for (int i=0; i<cs.length; ) {
                int j;
                for (j=i; j<cs.length && cs[j]!=' '; j++);
                int w = m.charsWidth(cs,i,j-i);
                if (w>constraint) {
                    // look for places to split a long word
                    for (int i1=i; i1<j; ) {
                        int j1;
                        for (j1=i1; j1<j && j1<i1+constraint && cs[j1]!=',' && cs[j1]!=';'; j1++);
                        if (j1<j) {
                            j1++;
                            // System.err.print(ws.size()+": part-word \""+new String(cs,i1,j1-i1)+"\" ");
                            ws.add(new W(i1, j1, m.charsWidth(cs,i1,j1-i1), 0));
                            // System.err.println(ws.get(ws.size()-1));
                        }
                        else {
                            w=m.charsWidth(cs,i1,j1-i1);
                            i=i1; // add spacing to the last bit
                        }
                        i1=j1;
                    }
                }
                int j0=j;
                for ( ; j<cs.length && cs[j]==' '; j++);
                // System.err.print(ws.size()+": word \""+new String(cs,i,j0-i)+"\" ");
                ws.add(new W(i, j0, w, m.charsWidth(cs,j0,j-j0)));
                // System.err.println(ws.get(ws.size()-1));
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
                return  super.toString()+": F[i="+i+",j="+j+",height="+height+",width="+width+",maxwidth="+maxwidth+",waste="+waste+",next="+next+"]";
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
                    System.err.println("can't clone "+this); 
                    System.exit(2); 
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
                int ww = ((W)ws.get(j)).width;
                next.i++; next.measurewidth(); next.measurewaste(); next.smooth();
                j++; measurewidth(); measurewaste();
                // System.err.println("after take "+this);
            }
            
            private void give() {
                int ww = ((W)ws.get(j-1)).width;
                next.i--; next.measurewidth(); next.measurewaste(); next.smooth();
                j--; measurewidth(); measurewaste();
                // System.err.println("after give "+this);
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
                    // System.err.println("smooth done");
                }
            }
            
            // the split algorithm puts some of the onus on the garbage collector ...
            public F split() {
                F f = i+1<j	  ? new F(i,i+1,new F(i+1,j,clone(next))) :
                      next!=null  ? new F(i,j,next.split()) : 
                                    this;
                return f.maxwidth<maxwidth ? f : this;
            }
            
            public void reportSplit(Vector v) {
                int start = ((W)ws.get(i)).i, end = ((W)ws.get(j-1)).j;
                // System.err.println(v.size()+": from "+i+"("+start+") to "+(j-1)+"("+end+")"+" width="+width+", maxwidth="+maxwidth+", waste="+waste);
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
    
    /* ************************
       Ascii -> Unicode translation, for fonts (such as Konstanz) which don't 
       have a proper Unicode encoding.
       
       also font substitution technology, because at present MacOS X Java doesn't 
       do proper glyph substitution.
       ************************
     */
     
    private static PosIntHashMap toUni=null, toAsc=null;
    
    private static Font substituteFont=null;
    private static HashMap substitutes = new HashMap(50,(float)0.5);
    
    private static class P {
        private int style, size;
        public int hashCode() {
            return size*10+style;
        }
        public boolean equals(Object o) {
            return o instanceof P && ((P)o).style==style && ((P)o).size==size;
        }
        public P(int style, int size) {
            this.style=style; this.size=size;
        }
    }
    
    private static Font findsubstitute(int style, int size) {
        P p = new P(style,size);
        Object o = (Font)substitutes.get(p);
        if (o==null) {
            Font f = substituteFont.deriveFont(style, (float)size);
            substitutes.put(p,f);
            return f;
        }
        else
            return (Font)o;
    }

    public static final int MENUENTRY = 0;
    public static final int DIALOGLABEL = 1;
    public static final int PROOFPANEL = 2;
    public static final int PANELENTRY = 3;
    public static final int BUTTON = 4;

    public static void setComponentFont(int kind, Component c) {
        switch (kind) {
            case MENUENTRY:
            case DIALOGLABEL:
            case PANELENTRY:
            case BUTTON:
                if (substituteFont!=null)
                    mimicFont(c);
                // else do nothing
                break;
            case PROOFPANEL:
                // don't know yet
                break;
            default:
                Alert.showErrorAlert("setComponentFont("+kind+","+c);
        }
    }

    private static void mimicFont(Component c) {
        // use size info from component itself
        Font f = c.getFont();
        Font f1 = findsubstitute(f.getStyle(), f.getSize());
        c.setFont(f1);
    }

    private static boolean codecDone = false;
    
    public static String toUnicode(String s) {
        return substituteFont==null ? tran(toUni, s) : s;
    }
    
    public static String toAscii(String s) {
        return substituteFont==null ? tran(toAsc, s) : s;
    }
    
    // we can't set the font of window titles, and I don't know what else ...
    public static String toUnicodeTitle(String s) {
        return tran(toUni, s);
    }
    
    // we can't set the font of window titles, and I don't know what else ...
    public static String toAsciiAlways(String s) {
        return tran(toAsc, s);
    }
    
    private static String tran(PosIntHashMap table, String s) {
        /* return "\u21d2\u22b8\u2297\u2ae0\u2227\u297e\u2295\u2192\u27db\u2ADF\u0393\u2261\u22A2\u2200\u22A7\u2194\u2228\u039B\u223C\u22A9\u222A\u00D7\u2135\u22C3\u2286\u2AE2\u25C1\u21dd\u2907\u21CC\u21a6\u2292\u22a5\u25a1\u25aa\u2283\u03bb\u225c\u03a4\u214b\u2203\u2234\u27e6\u2208\u27da\u27e7\u2229"; */
/* */
        codecDone = true;
        if (table==null) 
            return s;
        else {
            StringBuffer t = null;
            int len = s.length();
            for (int i=0; i<len; i++) {
                int c = table.get((int)s.charAt(i));
                if (c>=0) {
                    if (t==null)
                        t = new StringBuffer(s);
                    t.setCharAt(i,(char)c);
                }
            }
            return t==null ? s : t.toString();
        }
/* */
    }
    
    private static void setmap(int asc, int uni) {
        toUni.set(asc,uni); toAsc.set(uni,asc);
    }
    
    private static void setKonstanzMap() {
        int minsize = 100; // about 50 chars to translate
        
        // System.err.println("setting up map Konstanz <-> Unicode");
        
        toUni = new PosIntHashMap(minsize);
        toAsc = new PosIntHashMap(minsize);
        
        // this list defined by looking at what MacOS X thinks Konstanz means.
        // If it isn't universally accepted, we shall have to translate back into bytes,
        // and then into Unicode again.  But I hope not.
        
        // note the ones for which I haven't found a perfect match (look for ***)
        
        setmap(0x00A4, 0x21d2); // double arrow right
        setmap(0x00A5, 0x22b8); // lollipop
        setmap(0x00AA, 0x2297); // circled times
        setmap(0x00AF, 0x2AE0); // short up tack .. best I can find
        setmap(0x00B6, 0x2227); // logical and
        setmap(0x00B7, 0x297D); // right fishhook (RIGHT FISH TAIL), form of logical implication
        setmap(0x00BA, 0x2295); // circled plus
	setmap(0x00C1, 0x2192); // single arrow right
        setmap(0x00C2, 0x27db); // left and right turnstile
        setmap(0x00C4, 0x2ADF); // short down tack .. best I can find
        setmap(0x00C7, 0x0393); // Gamma
        setmap(0x00C8, 0x2261); // equivalent
        setmap(0x00CA, 0x22A2); // turnstile
        setmap(0x00CB, 0x2200); // for all
        setmap(0x00CC, 0x22A7); // models
        setmap(0x00CD, 0x2194); // left-right single arrow
        setmap(0x00CE, 0x2228); // logical or
        setmap(0x00CF, 0x039B); // Lambda
        setmap(0x00D2, 0x223C); // tilde operator
        setmap(0x00D3, 0x22A9); // forces
        setmap(0x00D4, 0x222A); // union
        setmap(0x00D9, 0x00D7); // multiplication (and it is D7, believe it or not ...)
        setmap(0x00DA, 0x2135); // aleph (alef?)
        setmap(0x00DC, 0x22C3); // n-ary union
        setmap(0x00DF, 0x2286); // subset or equal
        setmap(0x00E4, 0x2AE2); // triple-bar turnstile
        setmap(0x00EB, 0x25C1); // white left-pointing triangle
        setmap(0x00EF, 0x21dd); // rightwards squiggle arrow (*** should be turnstile with tilde as horizontal)
        setmap(0x00F6, 0x2907); // rightwards double arrow from bar
        setmap(0x00FC, 0x21CC); // RIGHTWARDS HARPOON OVER LEFTWARDS HARPOON
        setmap(0x00ff, 0x21a6); // rightwards single arrow from bar (maps to)
        setmap(0x0131, 0x2292); // square greater or equal (*** should have a line above)
        setmap(0x0178, 0x22a5); // bottom (as UP TACK)
        setmap(0x02C6, 0x25a1); // white square (25ab is small white square, if more appropriate)
        setmap(0x02D8, 0x25aa); // small black square (25a0 is black square, if more appropriate)
        setmap(0x02d9, 0x2283); // superset
        setmap(0x02da, 0x03bb); // lambda (more lambdas in the unicode 3 space, but let's not go there)
        setmap(0x02dc, 0x225c); // equal by definition to (as DELTA EQUAL TO; 225d is nicer in print but not on screen)
        setmap(0x2020, 0x03a4); // Tau (I think)
        setmap(0x2021, 0x214b); // turned (upside-down) ampersand
        setmap(0x2030, 0x2203); // exists
        setmap(0x2039, 0x2234); // proportion (looks like double colon)
        setmap(0x203a, 0x27e6); // white left square bracket (semantic bra) (301a may be preferable)
        setmap(0x2044, 0x2208); // element of
        setmap(0xf8ff, 0x27da); // left and right forces
        setmap(0xfb01, 0x27e7); // white right square bracket (semantic ket) (301b may be preferable)
        setmap(0xfb02, 0x2229); // intersection
    }
    
    public static void setfont(String name) throws ProtocolError {
        if (codecDone)
            throw (new ProtocolError("too late!"));
        else
        if (name.equals("Konstanz")) {
            if (japeserver.onMacOS) {
                substituteFont = new Font("Konstanz", Font.PLAIN, 1);
                if (substituteFont==null)
                    throw (new ProtocolError("can't open Konstanz Plain 1.0"));
            }
            setKonstanzMap();
        }
        else
            Alert.showErrorAlert("SETFONTS doesn't understand encoding "+name);
    }
    
    public static void unsetfont() {
        codecDone = false;
    }
}
