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

import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

public class MouseInteractionAdapter implements MouseListener, MouseMotionListener {
    byte eventKind;
    boolean dragSeen;

    /*
        void mouseClicked(MouseEvent e)
            Invoked when the mouse has been clicked on a component.
        void mouseEntered(MouseEvent e)
            Invoked when the mouse enters a component.
        void mouseExited(MouseEvent e)
            Invoked when the mouse exits a component.
        void mousePressed(MouseEvent e)
            Invoked when a mouse button has been pressed on a component.
        void mouseReleased(MouseEvent e)
            Invoked when a mouse button has been released on a component.

        All reasonable, except that (experimentally) mouseClicked seems to
        mean mouseReleased in the same place as mousePressed ...
     */

    public final void mouseClicked(MouseEvent e) { }
    public final void mouseEntered(MouseEvent e) { }
    public final void mouseExited(MouseEvent e) { }
    
    public final void mousePressed(MouseEvent e) {
        eventKind = LocalSettings.mouseDownKind(e);
        if ((eventKind&SelectionConstants.TextSelMask)!=0)
            textpressed(e);
        else
            pressed(e);
    }
    
    public final void mouseReleased(MouseEvent e) {
        if ((eventKind&SelectionConstants.TextSelMask)!=0)
            textreleased(e);
        else
        if (dragSeen)
            released(e);
        else
        if (e.getClickCount()==2)
            doubleclicked(e);
        else
            clicked(e);
    }

    /*
        void mouseDragged(MouseEvent e)
            Invoked when a mouse button is pressed on a component and then dragged.
        void mouseMoved(MouseEvent e)
            Invoked when the mouse button has been moved on a component
            (with no buttons no down).
        */

    public final void mouseDragged(MouseEvent e) {
        dragSeen = true;
        if ((eventKind&SelectionConstants.TextSelMask)!=0)
            textdragged(e);
        else
            dragged(e);
    }

    public final void mouseMoved(MouseEvent e) { }

    public void textpressed(MouseEvent e) { }
    public void pressed(MouseEvent e) { }

    public void textdragged(MouseEvent e) { }
    public void dragged(MouseEvent e) { }

    public void textreleased(MouseEvent e) { }
    public void clicked(MouseEvent e) { }
    public void released(MouseEvent e) { }
    public void doubleclicked(MouseEvent e) { }
}
