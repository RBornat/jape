/* 
    $Id$

    Copyright © 2003 Richard Bornat & Bernard Sufrin
     
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

    public JapeWindow(String title) {
        this(title, -1);
    }

    public JapeWindow(final String title, int proofnum) {
        super((LocalSettings.UnicodeWindowTitles || proofnum<0) ?
                     JapeUtils.trueTitle(title) :
                     ("Proof #"+proofnum));
        this.title=title; // ignoring whatever else may happen outside, this is a uid
        windowList.addToWindowv(this);
        windowListener = new WindowAdapter() {
            public void windowActivated(WindowEvent e) {
                if (windowListener!=null) {
                    windowList.setTopWindow(JapeWindow.this);
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

    public abstract static class WindowAction {
        public abstract void action(JapeWindow w);
    }
    
    private static class WindowList {
        private static Vector windowv = new Vector();
        
        public synchronized String stringOfWindowv() {
            String s = "[";
            for (int i=0; i<windowv.size(); i++) {
                s=s+"\""+((JapeWindow)windowv.get(i)).title+"\"";
                if (i+1<windowv.size())
                    s=s+"; ";
            }
            return s+"]";
        }

        public synchronized void addToWindowv(JapeWindow w) {
            windowv.add(w);
            if (windowv_tracing)
                Logger.log.println("JapeWindow.WindowList "+JapeUtils.enQuote(w.title)+
                                   ", "+stringOfWindowv());
            
        }

        public synchronized void setTopWindow(JapeWindow w) {
            if (windowv_tracing)
                Logger.log.print("JapeWindow.setTopWindow before "+JapeUtils.enQuote(w.title)+
                                 ", "+stringOfWindowv());
            int i = windowv.indexOf(w);
            if (i==-1)
                Alert.abort("untoppable window "+w.title);
            else {
                windowv.remove(i);
                windowv.insertElementAt(w,0);
            }
            if (windowv_tracing)
                Logger.log.println(" after "+stringOfWindowv());
        }

        public synchronized void removeFromWindowv(JapeWindow w) {
            if (windowv_tracing)
                Logger.log.print("JapeWindow.removeFromWindowv before "+JapeUtils.enQuote(w.title)+
                                 ", "+stringOfWindowv());
            int i = windowv.indexOf(w);
            if (i==-1)
                Alert.abort("unremovable window "+w.title);
            else
                windowv.remove(i);
            if (windowv_tracing)
                Logger.log.println(" after "+stringOfWindowv());
        }

        protected synchronized SurrogateWindow findSurrogate() {
            for (int i=0; i<windowv.size(); i++)
                if (windowv.get(i) instanceof SurrogateWindow)
                    return (SurrogateWindow)windowv.get(i);
            return null;
        }

        public synchronized JapeWindow findControlWindow() {
            for (int i=0; i<windowv.size(); i++) {
                JapeWindow w = (JapeWindow)windowv.get(i);
                if (w.isVisible() && w.servesAsControl())
                    return w;
            }
            return null;
        }
        
        protected synchronized JapeWindow findWindow(String title) {
            if (windowv_tracing)
                Logger.log.println("JapeWindow.findWindow \""+title+"\", "+stringOfWindowv());
            int len = windowv.size();
            for (int i=0; i<len; i++) {
                JapeWindow w = (JapeWindow)windowv.get(i);
                if (w.title.equals(title))
                    return w;
            }
            return null;
        }

        protected synchronized ProofWindow findProofWindow(int proofnum) {
            int len = windowv.size();
            for (int i=0; i<len; i++) {
                Object o = windowv.get(i);
                if (o instanceof ProofWindow && ((ProofWindow)o).proofnum==proofnum)
                    return (ProofWindow) o;
            }
            return null;
        }

        protected synchronized JapeWindow getTopWindow() {
            return windowv.size()==0 ? null : (JapeWindow)windowv.get(0);
        }

        protected synchronized void updateMenuBars() {
            for (Enumeration e = windowv.elements(); e.hasMoreElements(); ) {
                JapeWindow w = (JapeWindow)e.nextElement();
                w.setBar();
            }
        }

        protected synchronized void iter(WindowAction a) {
            for (Enumeration e = windowv.elements(); e.hasMoreElements(); ) {
                a.action((JapeWindow)e.nextElement());
            }
        }
    }

    protected static final WindowList windowList = new WindowList();

    public static void windowIter(WindowAction a) {
        windowList.iter(a);
    }
    
    protected WindowListener windowListener;

    private final static boolean windowv_tracing = false;
    
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

            if (lastPos.y+getHeight()>Jape.screenBounds.height ||
                lastPos.x+getWidth()>Jape.screenBounds.width) {
                lastPos.y = 0;
                firstPos.x += LocalSettings.PosIncr;
                if (firstPos.x+getWidth()>Jape.screenBounds.width)
                    firstPos.x = 0;
                lastPos.x = firstPos.x;
            }
        }

        return lastPos;
    }

    protected abstract boolean servesAsControl();

    public void closeWindow() {
        if (windowv_tracing)
            Logger.log.println("JapeWindow.closeWindow before "+JapeUtils.enQuote(title)+
                               ", "+windowList.stringOfWindowv());
        windowList.removeFromWindowv(this);
        // Linux gives us spurious events after the window has gone, so kill the listener
        removeWindowListener(windowListener); windowListener = null;
        setVisible(false); dispose();
        JapeMenu.windowRemoved(titleForMenu(), this);
    }

    /*******************************************************************************************

        static interface for Dispatcher, etc.
        
    *******************************************************************************************/
    
    public static JapeWindow findWindow(String title) {
        return windowList.findWindow(title);
    }

    public static JapeWindow getTopWindow() {
        return windowList.getTopWindow();
    }

    public static void updateMenuBars() {
        windowList.updateMenuBars();
    }

    public static void hideSurrogate() {
        SurrogateWindow w;
        while ((w=windowList.findSurrogate())!=null)
            w.closeWindow();
    }
    
    public static void ensureMenusAvailable() {
        if (windowList.findControlWindow()==null) {
            SurrogateWindow sw = windowList.findSurrogate();
            if (sw==null)
                sw = LocalSettings.hideSurrogateWindow ? ((SurrogateWindow)new MenuHolder()) :
                                                         ((SurrogateWindow)new ControlWindow());
            sw.setVisible(true);
        }
    }
    
    private static Point firstPos = new Point(0,0);
    private static Point lastPos  = null;

    public static void resetNextPos() {
        firstPos = new Point(0,0);
        lastPos = null;
    }
}
