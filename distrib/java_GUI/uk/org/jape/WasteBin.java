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

/* The wastebin_*.jpg files included with Jape derive initially from a clipping (trash_can_02)
   served up by Apple's AppleWorks in 2002. So far as I could tell they were in
   the public domain. I tweaked them a little (dimensions, aspect ratio, colour) 
   to include them in Jape. I intend the results to be covered by the GPL.
   
   Richard Bornat
 */
 
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Toolkit;

import java.awt.image.ImageObserver;

public class WasteBin extends Component implements DebugConstants,
						   LineTarget, LabelTarget, WorldTarget {
    private static Image enabled_image, selected_image, disabled_image,
			 enabled_scaled, selected_scaled, disabled_scaled;
    private Image current_image;

    private DisproofPane disproofPane;
    
    public WasteBin(DisproofPane disproofPane) {
	super();
	this.disproofPane = disproofPane;
	if (enabled_image==null) {
	    enabled_image  = Images.getImage("wastebin_enabled.jpg");
	    selected_image = Images.getImage("wastebin_selected.jpg");
	    disabled_image = Images.getImage("wastebin_disabled.jpg");
	    current_image = null;
	    // trigger imageUpdate
	    if (image_tracing)
		Logger.log.println("dimension request (enabled_image)");
	    enabled_image.getWidth(this); enabled_image.getHeight(this);
	}
	else
	    current_image = enabled_scaled;

	if (current_image!=null) {
	    // possible concurrency nasty: see comment in notifyHeight
	    if (image_tracing)
		Logger.log.println("dimension request (current_image)");
	    int width = current_image.getWidth(this), height = current_image.getHeight(this);
	    setSize(width, height);
	}
	else
	    setSize(-1, -1);
    }

    private boolean selected = false, enabled = true;

    private void setImage() {
	current_image = enabled ? (selected ? selected_scaled : enabled_scaled) :
	disabled_scaled;
    }
    
    private void setSelected(boolean selected) {
	this.selected = selected; setImage(); repaint();
    }

    public void setEnabled(boolean enabled) {
	this.enabled = enabled; setImage(); repaint();
    }

    public boolean scaled() { return current_image!=null; }

    public void paint(Graphics g) {
	if (current_image!=null) {
	    if (image_tracing)
		Logger.log.println("painting "+getSize());
	    g.drawImage(current_image, 0, 0, this);
	}
    }

    public void notifyHeight(int height) {
	if (image_tracing)
	    Logger.log.println("notifyHeight("+height+")");
	if (current_image==null) {
	    if (image_tracing)
		Logger.log.println("dimension request (enabled_image) in notifyHeight");
	    int imageheight = enabled_image.getHeight(this),
		imagewidth = enabled_image.getWidth(this);
	    if (imageheight!=-1 && imagewidth!=-1) {
		float scale = ((float)height)/((float)imageheight);

		int scaledwidth = (int)(((float)imagewidth)*scale),
		    scaledheight = (int)(((float)imageheight)*scale);

		enabled_scaled =
		    enabled_image.getScaledInstance(scaledwidth, scaledheight, Image.SCALE_SMOOTH);
		selected_scaled =
		    selected_image.getScaledInstance(scaledwidth, scaledheight, Image.SCALE_SMOOTH);
		disabled_scaled =
		    disabled_image.getScaledInstance(scaledwidth, scaledheight, Image.SCALE_SMOOTH);

		if (image_tracing)
		    Logger.log.println("image to be "+
				       (((float)imagewidth)*scale)+" x "+
				       (((float)imageheight)*scale));

		setImage();

		// there's a nasty bit of concurrency here ... be careful.
		// I have to getWidth and getHeight and then believe the local answers;
		// if either is -1 then I rely on imageUpdate;
		// if neither is -1 then I believe them.
		// (Well maybe there isn't, but it it's better to be sure.)
		if (image_tracing)
		    Logger.log.println("dimension request (current_image) in notifyHeight");
		int currwidth = current_image.getWidth(this),
		    currheight = current_image.getHeight(this);
		if (currwidth!=-1 && currheight!=-1) {
		    setSize(currwidth, currheight); repaint();
		    if (image_tracing) {
			Logger.log.println("immediate repaint "+getBounds());
		    }
		}
	    }
	    else
		if (image_tracing)
		    Logger.log.println("not yet: "+imageheight+"x"+imagewidth);
	}
	else {
	    if (image_tracing)
		Logger.log.println("we are already set");
	}
    }

    // This method should return true if further updates are needed
    // or false if the required information has been acquired
    private boolean MORE_PLEASE = true, THATS_ENOUGH = false;

    public boolean imageUpdate(Image img, int infoflags, int x, int y, int width, int height) {
	if (image_tracing)
	    Logger.log.println("imageUpdate("+img+",0x"+Integer.toHexString(infoflags)+
			       ","+x+","+y+","+width+","+height+")"+
			       "; enabled_image="+enabled_image+
			       "; enabled_scaled="+enabled_scaled+
			       "; current_image="+current_image+
			       "; WIDTH=0x"+Integer.toHexString(ImageObserver.WIDTH)+
			       "; HEIGHT=0x"+Integer.toHexString(ImageObserver.HEIGHT)+
			       "; ALLBITS=0x"+Integer.toHexString(ImageObserver.ALLBITS));
	if ((infoflags & (ImageObserver.WIDTH | ImageObserver.HEIGHT)) ==
	    (ImageObserver.WIDTH | ImageObserver.HEIGHT)) {
	    if (img==enabled_image) {
		if (image_tracing)
		    Logger.log.println("it's the first time");
		if (disproofPane.layout_set()) {
		    disproofPane.getLayout().layoutContainer(disproofPane);
		    if (image_tracing)
			Logger.log.println("triggered a layout");
		}
		return THATS_ENOUGH;
	    }
	    else {
		if (getWidth()==-1 || getHeight()==-1)
		    setSize(width, height);
		repaint();
		if (image_tracing) {
		    Logger.log.println("triggered first repaint "+getBounds());
		}
		return MORE_PLEASE;
	    }
	}
	else
	    if ((infoflags & ImageObserver.ALLBITS) == ImageObserver.ALLBITS) {
		if (image_tracing)
		    Logger.log.println("triggered second repaint "+getBounds());
		repaint(); return THATS_ENOUGH;
	    }
	else
	    return MORE_PLEASE; // you get all sorts of intermediate information
    }

    /* ******************************** drag target ******************************** */

    private boolean dragEnter() {
	if (enabled) {
	    setSelected(true); return true;
	}
	else
	    return false;
    }

    private void dragExit() {
	setSelected(false);
    }

    // WorldTarget
    public boolean dragEnter(byte dragKind, WorldItem w) { return dragEnter(); }
    public void dragExit(byte dragKind, WorldItem w) { dragExit(); }
    public void drop(byte dragKind, WorldItem w, int x, int y) {
	if (selected) {
	    if (dragKind==SelectionConstants.MoveWorldDrag)
		Reply.sendCOMMAND("deleteworld "+w.idX+" "+w.idY);
	    setSelected(false);
	}
	else
	    Alert.abort("world drop into waste bin when not selected");
    }
    
    // LabelTarget
    public boolean dragEnter(WorldItem w, String label) { return dragEnter(); }
    public void dragExit(WorldItem w, String label) { dragExit(); }
    public void drop(WorldItem w, String label) {
	if (selected) {
	    Reply.sendCOMMAND("deleteworldlabel "+w.idX+" "+w.idY+" \""+label+"\"");
	    setSelected(false);
	}
	else
	    Alert.abort("label drop into waste bin when not selected");
    }
    
    // LineTarget
    public boolean dragEnter(WorldConnector l) { return dragEnter(); }
    public void dragExit(WorldConnector l) { dragExit(); }
    public void drop(WorldConnector l) {
	if (selected) {
	    Reply.sendCOMMAND("deleteworldlink "+l.from.idX+" "+l.from.idY+" "+
			      l.to.idX+" "+l.to.idY);
	    setSelected(false);
	}
	else
	    Alert.abort("line drop into waste bin when not selected");
    }
}
