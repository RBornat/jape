/* 
    Copyright © 2003-19 Richard Bornat & Bernard Sufrin
     
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

public class AnnotatedTextComponent extends TextComponent {
    public final char[] printchars;
    public final String annottext;
    public final int annotlen;
    public AnnotatedTextComponent(int x, int y, byte fontnum, String annottext) {
	super(x, y, fontnum, printtext(annottext));
	this.annottext = annottext;
	this.annotlen = annottext.length();
	this.printchars = printtext.toCharArray();
    }
    static String printtext(String annottext) {
	int annotlength = annottext.length();
	StringBuffer printbuf = new StringBuffer(annotlength);
	for (int i=0; i<annotlength; i++) {
	    char c = annottext.charAt(i);
	    if (!invisible(c))
		printbuf.append(c);
	}
	return printbuf.toString();
    }    
    
    protected static char onbra, onket, offbra, offket, outbra, outket, lockbra, lockket;
    
    public static void setinvischars(char _onbra, char _onket, char _offbra, char _offket,
				     char _outbra, char _outket, char _lockbra, char _lockket) {
	onbra=_onbra; onket=_onket;
	offbra=_offbra; offket=_offket;
	outbra=_outbra; outket=_outket;
	lockbra=_lockbra; lockket=_lockket;
    }
    
    static boolean invisbra(char c) {
	return c==onbra || c==offbra || c==outbra || c==lockbra;
    }
    
    static boolean invisket(char c) {
	return c==onket || c==offket || c==outket || c==lockket;
    }
    
    static boolean invisible(char c) {
	return invisbra(c) || invisket(c);
    }
    
    static char bra2ket(char c) {
	return c==onbra	 ? onket :
	c==offbra ? offket :
	c==outbra ? outket :
	/* c==lockbra assumed */ lockket;
    }
    
    static char ket2bra(char c) {
	return c==onket	 ? onbra :
	c==offket ? offbra :
	c==outket ? outbra :
	/* c==lockket assumed */ lockbra;
    }
    
    static String annotatedString_of_char(char c) {
	return  c==onbra   ? "*ON("  :
		c==onket   ? ")NO*"  :
		c==offbra  ? "*OFF("  :
		c==offket  ? ")FFO*"  :
		c==outbra  ? "*OUT("  :
		c==outket  ? ")TUO*"  :
		c==lockbra ? "*LOCK(" :
		c==lockket ? ")KCOL*" :
		c=='"'     ? "\\\""   :
		String.valueOf(c);
    }
    
    public String toString() {
	String s = "AnnotatedTextComponent["+annotlen+":\"";
	for (int i=0; i<annottext.length(); i++)
	    s = s+annotatedString_of_char(annottext.charAt(i));
	return s+"\", "+super.toString()+"]";
    }
}
