//
//  JapeFont.java
//  japeserver
//
//  Created by Richard Bornat on Tue Sep 10 2002.
//  Copyright (c) 2002 __MyCompanyName__. All rights reserved.
//

import java.awt.Font;
import java.util.HashMap;

public class JapeFont {
    private static PosIntHashMap toUni=null, toAsc=null;
    
    private static Font substituteFont=null, menuFont, formulaFont;
    
    private static boolean codecDone = false;
    
    public static String toUnicode(String s) {
        return tran(toUni, s);
    }
    
    public static String toAscii(String s) {
        return tran(toAsc, s);
    }
    
    private static String tran(PosIntHashMap table, String s) {
        codecDone = true;
        if (table==null) // some other mechanism is being used ...
            return s;
        else {
            StringBuffer t = new StringBuffer(s);
            int len = t.length();
            for (int i=0; i<len; i++) {
                int c = table.get((int)t.charAt(i));
                if (c>=0)
                    t.setCharAt(i,(char)c);
            }
            return t.toString();
        }
    }
    
    private static void setmap(int asc, int uni) {
        toUni.set(asc,uni); toAsc.set(uni,asc);
    }
    
    private static void setKonstanzMap() {
        // this list defined by looking at what MacOS X thinks Konstanz means.
        // If it isn't universally accepted, we shall have to translate back into bytes,
        // and then into Unicode again.  But I hope not.
        int minsize = 100; // about 50 chars to translate
        
        toUni = new PosIntHashMap(minsize);
        toAsc = new PosIntHashMap(minsize);
        
        setmap(0xA4, 0x21d2); // double arrow right
        setmap(0xA5, 0x22b8); // lollipop
        setmap(0xAA, 0x2297); // circled times
        setmap(0xAF, 0x2AE0); // short up tack .. best I can find
        setmap(0xB6, 0x2227); // logical and
        // setmap(0xB7, 0x); // **** fishhook, form of logical implication
        setmap(0xBA, 0x2295); // circled plus
	setmap(0xC1, 0x2192); // single arrow right
        setmap(0xC2, 0x27db); // left and right turnstile
        setmap(0xC4, 0x2ADF); // short down tack .. best I can find
        setmap(0xC7, 0x0393); // Gamma
        setmap(0xC8, 0x2261); // equivalent
        setmap(0xCA, 0x22A2); // turnstile
        setmap(0xCB, 0x2200); // for all
        setmap(0xCC, 0x22A7); // models
        setmap(0xCD, 0x2194); // left-right single arrow
        setmap(0xCE, 0x2228); // logical or
        setmap(0xCF, 0x039B); // Lambda
        setmap(0xD2, 0x223C); // tilde operator
        setmap(0xD3, 0x22A9); // forces
        setmap(0xD4, 0x222A); // union
        setmap(0xD9, 0x00D7); // multiplication (and it is D7, believe it or not ...)
        setmap(0xDA, 0x2135); // aleph (alef?)
        setmap(0xDC, 0x22C3); // n-ary union
        setmap(0xDF, 0x2286); // subset or equal
        setmap(0xE4, 0x2AE2); // triple-bar turnstile
        setmap(0xEB, 0x25C1); // white left-pointing triangle
        // setmap(0xEF, 0x); // *** turnstile with tilde as horizontal
        // setmap(0xF6, 0x); // *** turnstile with double arrow as horizontal
        setmap(0xFC, 0x21CC); // RIGHTWARDS HARPOON OVER LEFTWARDS HARPOON
        // setmap(0xff, 0x); // *** turnstile with single arrow as horizontal
        // setmap(0x131, ); // *** like 2292 with a line above
        setmap(0x178, 0x22a5); // bottom (as UP TACK)
        setmap(0x2C6, 0x25a1); // white square (25ab is small white square, if more appropriate)
        setmap(0x2D8, 0x25aa); // small black square (25a0 is black square, if more appropriate)
        setmap(0x2d9, 0x2283); // superset
        setmap(0x2da, 0x03bb); // lambda (more lambdas in the unicode 3 space, but let's not go there)
        setmap(0x2dc, 0x225c); // equal by definition to (as DELTA EQUAL TO; 225d is nicer in print but not on screen)
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
            throw (new ProtocolError("SETFONTS must be sent when there are no open windows and nothing in the menus or panels"));
        else
        if (name.equals("KONSTANZ"))
            setKonstanzMap();
        else
            System.err.println("SETFONTS doesn't understand encoding "+name);
    }
    
    public static void unsetfont() {
        codecDone = false;
    }
}
