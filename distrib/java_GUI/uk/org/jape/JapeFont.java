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

import java.awt.Canvas;
import java.awt.Component;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.util.HashMap;
import javax.swing.JLabel;

public class JapeFont implements DebugConstants, ProtocolConstants {

    /* ************************
       Ascii -> Unicode translation, for fonts (such as Konstanz) which don't 
       have a proper Unicode encoding.
       
       also font substitution technology, because at present MacOS X Java doesn't 
       do proper glyph substitution.
       ************************
     */
     
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

    public static final int MENUENTRY   = 100,
                            DIALOGLABEL = 101,
                            PROOFPANEL  = 102,
                            PANELENTRY  = 103,
                            PANELBUTTON = 104;

    public static void setComponentFont(Component c, int kind) {
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

    public static byte[] interfaceFontSizes =
        new byte[]{ LocalSettings.FormulaFontSize, LocalSettings.ReasonFontSize, LocalSettings.ProvisoFontSize };

    private static Font[] interfaceFonts;

    public static String getFontNames(String sep) {
        initInterfaceFonts();
        String s = null;
        for (int i=TermFontNum; i<=ProvisoFontNum; i++) {
            Font f = interfaceFonts[i];
            s = (s==null ? "" : s+sep)+f.getFontName()+","+f.getStyle()+","+f.getSize();
        }
        return s;
    }
    
    public static void checkInterfaceFontnum(int fontnum) throws ProtocolError {
        if (fontnum<TermFontNum || fontnum>ProvisoFontNum)
            throw new ProtocolError("fontnum "+fontnum+" out of range");
    }

    private static void setInterfaceFonts(Font base) {
        interfaceFonts = new Font[3];
        for (int i=TermFontNum; i<=ProvisoFontNum; i++)
            interfaceFonts[i] = deriveFont(base, Font.PLAIN, interfaceFontSizes[i]);
    }

    private static boolean codecDone;
    
    private static void initInterfaceFonts() {
        if (interfaceFonts==null) {
            codecDone = true;
            setInterfaceFonts(new Font("sanserif", Font.PLAIN, 1));
        }
    }

    private static FontMetrics[] interfaceMetrics;

    private static void initInterfaceMetrics() {
        if (interfaceMetrics==null) {
            initInterfaceFonts();
            interfaceMetrics = new FontMetrics[3];
            JLabel l = new JLabel();
            for (int i=TermFontNum; i<=ProvisoFontNum; i++) {
                interfaceMetrics[i] = l.getFontMetrics(interfaceFonts[i]);
            }
        }
    }

    private static final Component dummyComponent = new Canvas();
    
    public static TextDimension measure(Font f, String s) {
        dummyComponent.setFont(f);
        return measure(dummyComponent, s);
    }

    public static TextDimension measure(Component c, String s) {
        FontMetrics m = c.getFontMetrics(c.getFont());
        return new TextDimension(m.stringWidth(s), m.getMaxAscent(), m.getMaxDescent());
    }

    public static TextDimension measure(String s, byte fontnum) {
        initInterfaceMetrics();
        if (measure_debug)
            System.err.println("measuring \""+s+"\"; ("+
                           interfaceMetrics[fontnum].stringWidth(s)+","+
                           interfaceMetrics[fontnum].getMaxAscent()+"["+
                           interfaceMetrics[fontnum].getAscent()+"],"+
                           interfaceMetrics[fontnum].getMaxDescent()+"["+
                           interfaceMetrics[fontnum].getDescent()+"]); "+
                           interfaceMetrics[fontnum].getLeading());
        return new TextDimension(interfaceMetrics[fontnum].stringWidth(s),
                                 interfaceMetrics[fontnum].getMaxAscent(),
                                 interfaceMetrics[fontnum].getMaxDescent());
    }

    public static int charsWidth(char[] cs, int off, int len, byte fontnum) {
        initInterfaceMetrics();
        return interfaceMetrics[fontnum].charsWidth(cs, off, len);
    }

    public static int stringWidth(Font f, String s) {
        dummyComponent.setFont(f);
        return stringWidth(dummyComponent, s);
    }

    public static int stringWidth(Component c, String s) {
        FontMetrics m = c.getFontMetrics(c.getFont());
        return m.stringWidth(s);
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

    private static String encodingName;
    
    public static void setSubstituteFont(String name) throws ProtocolError {
        if (encodingName==null || !encodingName.equals(name)) {
            if (codecDone)
                throw new ProtocolError("too late!");
            else
                if (name.equals("Konstanz")) {
                    if (japeserver.onMacOS) {
                        substituteFont = new Font("Konstanz", Font.PLAIN, 1);
                        if (substituteFont==null)
                            throw new ProtocolError("can't open Konstanz Plain 1.0");
                        setInterfaceFonts(substituteFont);
                        Reply.sendCOMMAND("setfonts \""+getFontNames("\" \"")+"\"");
                    }
                }
            else
                throw new ProtocolError("japeserver doesn't understand encoding "+name);
            encodingName = name;
        }
    }
    
    public static void unsetfont() {
        codecDone = false;
    }
}
