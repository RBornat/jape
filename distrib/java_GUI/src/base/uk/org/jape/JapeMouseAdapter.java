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

import java.awt.event.MouseEvent;

import javax.swing.event.MouseInputListener;

public class JapeMouseAdapter implements JapeMouseListener, MouseInputListener,
                                         SelectionConstants {
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

    // you get a click event if you press the mouse at a particular point, move it and then
    // move back to the same point!  Well blow me down: we're not having that.

    private int x, y, wobble;
    private boolean significantWobble() { return wobble>3; } // a bit of wobble allowed, esp. for laptops
    private boolean recording=false;
    private static final boolean chasing_drag_events=false;

    @SuppressWarnings("unused")
    public final void mouseClicked(MouseEvent e) { 
        if (chasing_drag_events && recording)
            Logger.log.println((Object)this+" click missed"); 
    }
    
    public void mouseEntered(MouseEvent e) { }
    public void mouseExited(MouseEvent e) { }
    
    public final void mousePressed(MouseEvent e) {
        if (chasing_drag_events) {
            if (!recording)
                Logger.log.println((Object)this+" pressed seen"); 
            else
                Logger.log.println((Object)this+" pressed again while recording");

            recording = true;
        }
        x=e.getX(); y=e.getY(); wobble=0;
        pressed(e);
    }
    
    public final void mouseReleased(MouseEvent e) {
        if (chasing_drag_events) {
            if (recording)
                Logger.log.println((Object)this+" released seen wobble "+wobble+" click count "+e.getClickCount());
            else
                Logger.log.println((Object)this+" released seen without press");
            recording = false;
        }
        // we have a released event -- but is it mine?
        RegisteredDrag dragee = Jape.getRegisteredDrag();
        if (dragee!=null)
            dragee.released(e); // which will go to me if it's me that's being dragged.
        else
        if (significantWobble())
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
        wobble = Math.max(wobble, Math.abs(e.getX()-x)+Math.abs(e.getY()-y));
        if (chasing_drag_events) {
            if (recording)
                Logger.log.println((Object)this+" dragged seen wobble "+wobble);
            else
                Logger.log.println((Object)this+" dragged without recording");
        }
        dragged(significantWobble(), e);
    }

    @SuppressWarnings("unused")
    public final void mouseMoved(MouseEvent e) { 
        if (chasing_drag_events && recording)
            Logger.log.println((Object)this+" move missed"); 
    }

    public void pressed(MouseEvent e) { }
    public void dragged(boolean significantWobble, MouseEvent e) { }
    public void released(MouseEvent e) { }
    public void clicked(MouseEvent e) { }
    public void doubleclicked(MouseEvent e) { }
}

