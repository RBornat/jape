/* 
    $Id$

    Copyright © 2003 Richard Bornat & Bernard Sufrin
     
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

import java.awt.Font;
import java.awt.Point;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import java.util.Enumeration;
import java.util.Vector;

import javax.swing.JFrame;
import javax.swing.JMenuBar;

public abstract class JapeWindow extends JFrame {

    final String title;
    private static Vector windowv = new Vector();

    public JapeWindow(String title) {
        this(title, -1);
    }

    public JapeWindow(final String title, int proofnum) {
        
        super((LocalSettings.UnicodeWindowTitles || proofnum<0) ?
                     (japeserver.onMacOS ? JapeCharEncoding.trueUnicode(title) : title) :
                     ("Proof #"+proofnum));
        this.title=title; // ignoring whatever else may happen outside, this is a uid
        addTowindowv();
        if (windowv_tracing)
            Logger.log.println("JapeWindow.init \""+title+"\", "+proofnum+", "+stringOfwindowv());
        windowListener = new WindowAdapter() {
            public void windowActivated(WindowEvent e) {
                if (windowListener!=null) {
                    setTopWindow();
                    JapeMenu.windowActivated(titleForMenu(), JapeWindow.this);
                    if (LocalSettings.hideSurrogateWindow && servesAsControl())
                        hideSurrogate();
                }
                else
                    Logger.log.println("JapeWindow.windowListener late windowActivated \""+
                                       title +"\"; "+e);
            }
        };
        addWindowListener(windowListener);
        JapeMenu.windowAdded(titleForMenu(proofnum), this);
    }

    protected WindowListener windowListener;

    private final static boolean windowv_tracing = false;
    
    private static String stringOfwindowv() {
        String s = "[";
        for (int i=0; i<windowv.size(); i++) {
            s=s+"\""+((JapeWindow)windowv.get(i)).title+"\"";
            if (i+1<windowv.size())
                s=s+"; ";
        }
        return s+"]";
    }

    private synchronized void addTowindowv() {
        windowv.add(this);
    }
    
    private synchronized void setTopWindow() {
        if (windowv_tracing)
            Logger.log.print("JapeWindow.setTopWindow before \""+title+"\", "+stringOfwindowv());
        int i = windowv.indexOf(JapeWindow.this);
        if (i==-1)
            Alert.abort("untoppable window "+this.title);
        else {
            windowv.remove(i);
            windowv.insertElementAt(this,0);
        }
        if (windowv_tracing)
            Logger.log.println(" after "+stringOfwindowv());
    }

    private synchronized void removeFromwindowv() {
        if (windowv_tracing)
            Logger.log.print("JapeWindow.removeFromwindowv before \""+title+"\", "+stringOfwindowv());
        int i = windowv.indexOf(this);
        if (i==-1)
            Alert.abort("unremovable window "+JapeWindow.this.title);
        else
            windowv.remove(i);
        if (windowv_tracing)
            Logger.log.println(" after "+stringOfwindowv());
    }

    public void closeWindow() {
        if (windowv_tracing)
            Logger.log.println("JapeWindow.closeWindow before \""+title+"\", "+stringOfwindowv());
        removeFromwindowv();
        // Linux gives us spurious events after the window has gone, so kill the listener
        removeWindowListener(windowListener); windowListener = null;
        setVisible(false); dispose();
        JapeMenu.windowRemoved(titleForMenu(), this);
    }

    protected synchronized void hideSurrogate() {
        for (int i=0; i<windowv.size(); i++)
            if (windowv.get(i) instanceof SurrogateWindow)
                ((SurrogateWindow)windowv.get(i)).closeWindow();
    }

    private String titleForMenu(int proofnum) {
        return LocalSettings.UnicodeWindowTitles || proofnum<0 ? title : proofnum+":"+title;
    }

    private String titleForMenu() {
        return titleForMenu(this instanceof ProofWindow ? ((ProofWindow)this).proofnum : -1);
    }

    private int menustamp = 0;
    
    protected final void setBar() {
        menustamp = JapeMenu.setBar(this, menustamp);
    }

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

    protected abstract boolean servesAsControl();

    /*******************************************************************************************

        static interface for Dispatcher, etc.
        
    *******************************************************************************************/
    
    public static JapeWindow findWindow(String title) {
        if (windowv_tracing)
            Logger.log.println("JapeWindow.findWindow \""+title+"\", "+stringOfwindowv());
        int len = windowv.size();
        for (int i=0; i<len; i++) {
            JapeWindow w = (JapeWindow)windowv.get(i);
            if (w.title.equals(title))
                return w;
        }
        return null;
    }

    public static JapeWindow getTopWindow() {
        return windowv.size()==0 ? null : (JapeWindow)windowv.get(0);
    }

    public static Enumeration windows() {
        return windowv.elements();
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
            if (w instanceof SurrogateWindow)
                sw = (SurrogateWindow) w;
            else
            if (w.isVisible() && w.servesAsControl())
                return;
        }
        // no useable menus anywhere ... get a surrogate
        if (sw==null)
            sw = LocalSettings.hideSurrogateWindow ? ((SurrogateWindow)new MenuHolder()) :
                                                     ((SurrogateWindow)new ControlWindow());
        sw.setVisible(true);
    }
    
    private static Point firstPos = new Point(0,0);
    private static Point lastPos  = null;

    public static void resetNextPos() {
        firstPos = new Point(0,0);
        lastPos = null;
    }
}
