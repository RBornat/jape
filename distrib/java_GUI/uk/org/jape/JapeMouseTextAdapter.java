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

/*
    Bernard's original ProofCanvas included this comment:
    
        This next spasm copes with the fact that Macs have 1-button
        mice, and that (in order to cope with this) Java AWT
        reports button 2 and button 3 presses AS IF they were
        presses with one of Meta or Alt pressed. I simply don't know
        whether the getButton field of an event on the Mac always
        says 1, or whether the lowlevel AWTery reports virtual
        buttons. Until I find out, I'm assuming the former, and using
        the Alt and Meta fields to give an indication of the button
        that was actually pressed (on nonMacs) or the (virtual) button
        that a MacIsta indicated that she wanted to press.
        
        Beyond here we're simply pretending we have a 3-button mouse.
    
    and this code:
    
        // Assigns the right virtual button for all but a move
        lastButton = 1;
        if (e.isAltDown())  lastButton=2;
        else
        if (e.isMetaDown()) lastButton=3;
    
    I'm using LocalSettings to get this right on different machines.
 */

/*
    It might be nice if click meant select me (as it does), and press-and-drag
    meant text-select me (as it doesn't for us, but it does in every editor).
    
    The drawbacks, apart from incompatibility with Actually Existing Jape, would
    be (a) impossible to select a token with a single click; (b) a modifier key /
    alternative button needed for drag-n-drop, when I come to it.
    
    But it doesn't work.  Essentially normal click (select, move when it's implemented)
    works on the whole object, text click (select ranges of text) works within the object.
    They are two different actions, and I shouldn't confuse them.  No doubt that was the
    reason for the original design ...
 
 */

public class JapeMouseTextAdapter extends    JapeMouseAdapter
                                  implements JapeMouseListener,
                                             SelectionConstants {
    private byte eventKind;

    public final void pressed(MouseEvent e) {
        eventKind = LocalSettings.mouseDownTextItemMeans(e);
        if ((eventKind&TextSelMask)!=0)
            textpressed(eventKind, e);
        else
            pressed(eventKind, e);
    }
    
    public final void dragged(MouseEvent e) {
        if ((eventKind&TextSelMask)!=0)
            textdragged(eventKind, e);
        else
            dragged(eventKind, e);
    }

    public final void slightlydragged(MouseEvent e) {
        if ((eventKind&TextSelMask)!=0)
            textdragged(eventKind, e);
    }

    public final void released(MouseEvent e) {
        if ((eventKind&TextSelMask)!=0)
            textreleased(eventKind, !wobbly(), e);
        else
            released(eventKind, e);
    }

    public final void clicked(MouseEvent e) {
        if ((eventKind&TextSelMask)!=0)
            textreleased(eventKind, true, e);
        else
            clicked(eventKind, e);
    }

    public final void doubleclicked(MouseEvent e) {
        if ((eventKind&TextSelMask)!=0)
            textreleased(eventKind, true, e);
        else
            doubleclicked(eventKind, e);
    }

    public void textpressed(byte eventKind, MouseEvent e) { }
    public void pressed(byte eventKind, MouseEvent e) { }

    public void textdragged(byte eventKind, MouseEvent e) { }
    public void dragged(byte eventKind, MouseEvent e) { }

    public void textreleased(byte eventKind, boolean isClick, MouseEvent e) { }
    public void released(byte eventKind, MouseEvent e) { }
    public void clicked(byte eventKind, MouseEvent e) { }
    public void doubleclicked(byte eventKind, MouseEvent e) { }
}
