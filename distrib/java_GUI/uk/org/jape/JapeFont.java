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

import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.util.HashMap;
import javax.swing.JLabel;

public class JapeFont {

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
    
    private static Font deriveFont(Font f, int style, int size) {
        if (substituteFont==null)
            return f.deriveFont(style, (float)size);
        else {
            P p = new P(style,size);
            Object o = (Font)substitutes.get(p);
            if (o==null) {
                Font f1 = substituteFont.deriveFont(style, (float)size);
                substitutes.put(p,f1);
                return f1;
            }
            else
                return (Font)o;
        }
    }

    public static final int MENUENTRY   = 0;
    public static final int DIALOGLABEL = 1;
    public static final int PROOFPANEL  = 2;
    public static final int PANELENTRY  = 3;
    public static final int PANELBUTTON = 4;

    public static void setComponentFont(int kind, Component c) {
        switch (kind) {
            case MENUENTRY  : 
            case DIALOGLABEL:
                mimicFont(c); break;
            case PANELENTRY :
                mimicFont(c, LocalSettings.PanelEntryFontSize); break;
            case PANELBUTTON:
                mimicFont(c, LocalSettings.PanelButtonFontSize); break;
            case PROOFPANEL:
                // don't know yet
                break;
            default:
                Alert.showErrorAlert("setComponentFont("+kind+","+c);
        }
    }

    private static void mimicFont(Component c) {
        // use size info from component itself
        mimicFont(c, c.getFont().getSize());
    }

    private static void mimicFont(Component c, int size) {
        Font f = c.getFont();
        c.setFont(deriveFont(f, f.getStyle(), size));
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
        setmap(0x00E3, 0x27E8); // sequence bra
        setmap(0x00E4, 0x2AE2); // triple-bar turnstile
        setmap(0x00EB, 0x25C1); // white left-pointing triangle
        setmap(0x00EF, 0x21DD); // rightwards squiggle arrow (*** should be turnstile with tilde as horizontal)
        setmap(0x00F1, 0x27E9); // sequence ket
        setmap(0x00F5, 0x21D0); // leftwards double arrow
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

    /* from the jape code (displayfont.ml)
       (* Useful translation for Japeserver marshalling.
        * Current C/Java/Tk interfaces believe in these integers.
        *
        *  TermFont = 0
        *  ReasonFont = 1
        *  ProvisoFont = 2
        *
        *)
     */

    public final static int termFontNum = 0,  reasonFontNum = 1,  provisoFontNum = 2;
    public static byte[] interfaceFontSizes =
        new byte[]{ LocalSettings.FormulaFontSize, LocalSettings.ReasonFontSize, LocalSettings.ProvisoFontSize };
    private static Font[] interfaceFonts;

    public static void checkInterfaceFontnum(int fontnum) throws ProtocolError {
        if (fontnum<termFontNum || fontnum>provisoFontNum)
            throw new ProtocolError("fontnum "+fontnum+" out of range");
    }
    
    private static void initInterfaceFonts() {
        if (interfaceFonts==null) {
            codecDone = true;
            Font base = new Font("sanserif", Font.PLAIN, 1);
            interfaceFonts = new Font[3];
            for (int i=termFontNum; i<=provisoFontNum; i++)
                interfaceFonts[i] = deriveFont(base, Font.PLAIN, interfaceFontSizes[i]);
        }
    }

    private static FontMetrics[] interfaceMetrics;

    private static void initInterfaceMetrics() {
        if (interfaceMetrics==null) {
            initInterfaceFonts();
            interfaceMetrics = new FontMetrics[3];
            JLabel l = new JLabel();
            for (int i=termFontNum; i<=provisoFontNum; i++) {
                interfaceMetrics[i] = l.getFontMetrics(interfaceFonts[i]);
            }
        }
    }

    public static TextDimension measure(JLabel l, String s) {
        FontMetrics m = l.getFontMetrics(l.getFont());
        return new TextDimension(m.stringWidth(s), m.getMaxAscent(), m.getMaxDescent());
    }

    public static TextDimension measure(String s, byte fontnum) {
        initInterfaceMetrics();
        return new TextDimension(interfaceMetrics[fontnum].stringWidth(s),
                                 interfaceMetrics[fontnum].getMaxAscent(),
                                 interfaceMetrics[fontnum].getMaxDescent());
    }

    public static int charsWidth(char[] cs, int off, int len, byte fontnum) {
        initInterfaceMetrics();
        return interfaceMetrics[fontnum].charsWidth(cs, off, len);
    }

    public static TextDimension checkedMeasure(String s, byte fontnum) throws ProtocolError {
        checkInterfaceFontnum(fontnum);
        return measure(s, fontnum);
    }

    public static FontMetrics getFontMetrics(byte fontnum) {
        initInterfaceMetrics();
        return interfaceMetrics[fontnum];
    }

    public static FontMetrics checkedFontMetrics(byte fontnum) throws ProtocolError {
        checkInterfaceFontnum(fontnum);
        return getFontMetrics(fontnum);
    }

    public static Font getFont(byte fontnum) {
        initInterfaceFonts();
        return interfaceFonts[fontnum];
    }
    public static void setSubstituteFont(String name) throws ProtocolError {
        if (codecDone)
            throw new ProtocolError("too late!");
        else
        if (name.equals("Konstanz")) {
            if (japeserver.onMacOS) {
                substituteFont = new Font("Konstanz", Font.PLAIN, 1);
                if (substituteFont==null)
                    throw new ProtocolError("can't open Konstanz Plain 1.0");
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
