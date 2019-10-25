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

@SuppressWarnings("serial")
public class ControlWindow extends SurrogateWindow {
    public ControlWindow() {
	super("Jape Control"); // let's give the user a clue when the window is closed
	setLocation(Jape.screenBounds.width/2-getWidth()/2,
		    Jape.screenBounds.height/2-getHeight()/2);
    }

    protected void windowCloser() {
	if (Alert.askOKCancel(ControlWindow.this, "Quit Jape?")==Alert.OK)
	    Jape.handleQuit();
    }
}

