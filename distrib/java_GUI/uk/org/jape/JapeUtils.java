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
import java.awt.Container;

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

    public static Component findTargetAt(Class target, Component c, int x, int y) {
	if (c.isVisible() && c.contains(x,y)) {
	    if (c instanceof Container) {
		Container c1 = (Container) c;
		int ncs = c1.getComponentCount();
		for (int i=0; i<ncs; i++) {
		    Component c2 = c1.getComponent(i);
		    if ((c2=findTargetAt(target, c2, x-c2.getX(), y-c2.getY()))!=null)
			return c2;
		}
	    }
	    // no child fits: will we do?
	    if (target.isInstance(c))
		return c;
	}

	return null; // all else has failed
    }
}
