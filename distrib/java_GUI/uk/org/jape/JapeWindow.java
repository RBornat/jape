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

import java.util.Enumeration;
import java.awt.Font;
import java.util.Iterator;
import javax.swing.JFrame;
import javax.swing.JMenuBar;
import java.awt.Point;
import java.util.Vector;

public class JapeWindow extends JFrame {

    String title;
    protected static Vector windowv = new Vector();
    
    public JapeWindow(String title) {
        super(title);
        this.title=title; // ignoring whatever else may happen outside, this is a uid
        windowv.add(this);
    }
    
    public static JapeWindow findWindow(String title) {
        int len = windowv.size();
        for (int i=0; i<len; i++) {
            JapeWindow w = (JapeWindow)windowv.get(i);
            if (w.title.equals(title))
                return w;
        }
        return null;
    }
    
    public static Enumeration windows() {
        return windowv.elements();
    }
    
    protected void setBar() {
        if (this instanceof ProofWindow)
            JapeMenu.setProofWindowBar(this);
        else
        if ((this instanceof PanelWindowData.PanelWindow && LocalSettings.panelWindowMenus) ||
            this instanceof SurrogateWindow)
            JapeMenu.setNonProofWindowBar(this);
    }
    
    public static void updateMenuBars() {
        for (Enumeration e = windowv.elements(); e.hasMoreElements(); ) {
            JapeWindow w = (JapeWindow)e.nextElement();
            w.setBar();
        }
    }

    public static void ensureMenusAvailable() {
        SurrogateWindow sw = null;
        for (int i=0; i<windowv.size(); i++) {
            JapeWindow w = (JapeWindow)windowv.get(i);
            if (w instanceof ProofWindow)
                return;
            if (w instanceof PanelWindowData.PanelWindow && LocalSettings.panelWindowMenus)
                return;
            if (w instanceof SurrogateWindow)
                sw = (SurrogateWindow) w;
        }
        // no useable menus anywhere ... get a surrogate
        if (sw==null)
            sw = new SurrogateWindow();
        sw.setVisible(true);
    }
    
    private static Point firstPos = new Point(0,0);
    private static Point lastPos  = null;

    protected Point nextPos() {
        if (lastPos==null) {
            lastPos = firstPos;
        }
        else {
            lastPos.x += LocalSettings.PosIncr;
            lastPos.y += LocalSettings.PosIncr;

            if (lastPos.y+getHeight()>japeserver.screenBounds.height ||
                lastPos.x+getWidth()>japeserver.screenBounds.width) {
                lastPos.y = 0;
                firstPos.x += LocalSettings.PosIncr;
                if (firstPos.x+getWidth()>japeserver.screenBounds.width)
                    firstPos.x = 0;
                lastPos.x = firstPos.x;
            }
        }

        return lastPos;
    }
}
