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

import java.awt.Component;
import java.awt.Container;
import java.awt.Point;
import java.awt.Rectangle;

public class JapeUtils {
    public static String enQuote(Object o) {
	if (o==null)
	    return "null";
	else {
	    StringBuffer b = new StringBuffer();
	    b.append('"');
	    String s = o.toString();
	    for (int i=0; i<s.length(); i++) {
		char c = s.charAt(i);
		if (c=='"')
		    b.append("\\\"");
		else
		    b.append(c);
	    }
	    b.append('"');
	    return b.toString();
	}
    }

    /* not a very complete attempt, yet */
    public static boolean isQuoted(String s) {
        if (s==null || s.length()==1)
            return false;
        else
            return s.startsWith("\"") && s.endsWith("\"");
    }
    
    public static String stringOfArray(Object[] a, String sep, boolean enQuote) {
	String s = "{";
	for (int i=0; i<a.length; i++) {
	    s = s+(enQuote ? enQuote(a[i].toString()) : a[i].toString());
	    if (i+1<a.length)
		s=s+sep;
	}
	return s+"}";
    }

    public static String shortStringOfRectangle(Rectangle r) {
	return r.x+","+r.y+" "+r.width+"x"+r.height;
    }

    public static void showContainer(Container pane, String prefix) {
	for (int i=0; i<pane.getComponentCount(); i++) {
	    Component c = pane.getComponent(i);
	    String label = prefix==null ? ""+i : prefix+"."+i;
	    Logger.log.println(label+": "+c);
	    if (c instanceof Container)
		showContainer((Container)c, label);
	}
    }

    public static void showContainer(Container pane) {
	Logger.log.println(pane);
	showContainer(pane, null);
    }

    public static void showShortContainer(Container pane, String prefix) {
	for (int i=0; i<pane.getComponentCount(); i++) {
	    Component c = pane.getComponent(i);
	    String label = prefix==null ? ""+i : prefix+"."+i;
	    Logger.log.println(label+": "+shortStringOfRectangle(c.getBounds()));
	    if (c instanceof Container)
		showShortContainer((Container)c, label);
	}
    }

    public static void showShortContainer(Container pane) {
	    Logger.log.println(shortStringOfRectangle(pane.getBounds()));
	    showShortContainer(pane,null);
    }

    public static boolean isIn(Object thing, Object[] things) {
	for (int i=0; i<things.length; i++)
	    if (things[i].equals(thing))
		return true;
	return false;
    }
    
    // some vector geometry (copied from web, sigh!)
    
    public static Point lineVector(Point a, Point b) { // make a vector
        return new Point(b.x-a.x, b.y-a.y);
    }
    public static double distance(Point a, Point b) {
        double d1 = a.x - b.x;
        double d2 = a.y - b.y;
        return Math.sqrt(d1*d1+d2*d2);
    }
    
    public static double dotProduct(Point ab, Point cd) { 
        return (double)(ab.x * cd.x) + (double)(ab.y * cd.y);
   }

    public static double crossProduct(Point ab, Point cd) { 
        return (double)(ab.x * cd.y) - (double)(cd.x * ab.y);
   }

    public static double pointToLineDistance(Point c, Point a, Point b) {
        // line is (segment) ab, point is c
        Point ab = lineVector(a,b), bc = lineVector(b,c);
        double dist = crossProduct(ab, bc) / distance(a,b);
        double dot1 = dotProduct(ab, bc);
        if(dot1 > 0)
            return distance(b,c);
        else {
            Point ba = lineVector(b,a), ac = lineVector(a,c);
            double dot2 = dotProduct(ba, ac);
            if (dot2 > 0)
                return distance(a,c);
            else
                return Math.abs(dist);
        }    
    }
}
