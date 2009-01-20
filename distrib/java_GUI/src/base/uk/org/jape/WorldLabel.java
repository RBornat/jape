/* 
    $Id$

    Copyright © 2003-8 Richard Bornat & Bernard Sufrin
     
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
import java.awt.Graphics;
import java.awt.Point;

import java.awt.event.MouseEvent;

import javax.swing.JFrame;
import javax.swing.JLayeredPane;
import javax.swing.SwingUtilities;

@SuppressWarnings("serial")
public class WorldLabel extends TextItem implements MiscellaneousConstants {
    protected final WorldCanvas canvas;
    protected final WorldItem world;
    protected final boolean forced;
    protected final String text;
    protected final JLayeredPane layeredPane;
    protected final Container contentPane;

    public WorldLabel(WorldCanvas canvas, JFrame window, WorldItem world,
		      int x, int y, boolean forced, String text) {
	super(canvas, x, y, ProtocolConstants.ProvisoFontNum, text);
	this.canvas = canvas;
	this.layeredPane = window.getLayeredPane();
	this.contentPane = window.getContentPane();
	this.world = world;
	this.forced = forced;
	this.text = text;

	addMouseInputListener(new JapeMouseAdapter() {
	    private boolean noticeDrag;
	    private byte dragKind;
	    public void pressed(MouseEvent e) {
		dragKind = LocalSettings.mousePressWorldLabelMeans(e);
		WorldLabel.this.canvas.claimFocus();
		noticeDrag = !(/* e.isAltDown() || */ e.isShiftDown() ||
			       e.isMetaDown() || e.isControlDown());
		if (noticeDrag)
		    WorldLabel.this.pressed(dragKind, e);
	    }
	    public void dragged(boolean wobbly, MouseEvent e) {
		if (wobbly && noticeDrag)
		    WorldLabel.this.dragged(dragKind, e); // don't take notice of small movements
	    }
	    public void released(MouseEvent e) {
		if (noticeDrag)
		    WorldLabel.this.released(dragKind, e);
	    }
	});
    }

    @SuppressWarnings("serial")
    class LabelImage extends DragImage {
	public LabelImage() {
	    super(Transparent);
	    include(WorldLabel.this); fixImage();
	}
    }

    private int startx, starty, lastx, lasty;
    private boolean firstDrag;
    private LabelImage labelImage;
    private LabelTarget over;
    
    private void pressed(byte dragKind, MouseEvent e) {
	startx = e.getX(); starty = e.getY(); firstDrag = true;
    }

    protected void dragged(byte dragKind, MouseEvent e) {
        if (firstDrag) {
            firstDrag = false;
            over = null;
            if (labelImage==null)
                labelImage = new LabelImage();
            layeredPane.add(labelImage, JLayeredPane.DRAG_LAYER);
            labelImage.setLocation(SwingUtilities.convertPoint(this, e.getX()-startx,
                    e.getY()-starty, layeredPane));
            if (DebugVars.drag_tracing)
                Logger.log.println("; dragged label at "+labelImage.getX()+","+labelImage.getY());
            labelImage.repaint();
            if (dragKind==MoveLabelDrag) {
                setVisible(false); canvas.forcerepaint();
            }
            else
                canvas.wasteBin.setEnabled(false);
        }
        else {
            if (DebugVars.drag_tracing)
                Logger.log.print("mouse dragged to "+e.getX()+","+e.getY());
            labelImage.moveBy(e.getX()-lastx, e.getY()-lasty);
            if (DebugVars.drag_tracing)
                Logger.log.println("; dragged label now at "+labelImage.getX()+","+labelImage.getY());
            Point p = SwingUtilities.convertPoint(this, e.getX(), e.getY(), contentPane);
            /* if I knew how to make this polymorphic in Java, I would */
            Component target = contentPane.findComponentAt(p);
            if (target instanceof ContainerWithOrigin.Child)
                target = target.getParent();
            if (target!=null && target instanceof LabelTarget) {
                LabelTarget ltarget = (LabelTarget)target;
                if (ltarget!=over) {
                    if (over!=null) {
                        over.dragExit(world, text); over=null;
                    }
                    if (ltarget!=null && ltarget.dragEnter(world, text))
                        over = ltarget;
                }   
            }
            else
            if (over!=null) {
                over.dragExit(world, text); over=null;
            }
        }
        lastx = e.getX(); lasty = e.getY();
    }

    protected void released(final byte dragKind, MouseEvent e) {
	if (DebugVars.drag_tracing)
	    Logger.log.println("mouse released at "+e.getX()+","+e.getY()+
			       "; dragged label at "+labelImage.getX()+","+labelImage.getY());
	if (over==null)
	    new Flyback(labelImage, labelImage.getLocation(),
			SwingUtilities.convertPoint(this, 0, 0, layeredPane)) {
		protected void finishFlyback() { finishDrag(dragKind, false); }
	    };
	else {
	    over.drop(dragKind, world, text);
	    finishDrag(dragKind, true);
	}
    }

    protected void finishDrag(byte dragKind, boolean success) {
	layeredPane.remove(labelImage);
	layeredPane.repaint();
	canvas.wasteBin.setEnabled(true);
	if (dragKind==MoveLabelDrag && !success) {
	    setVisible(true); repaint();
	}
    }

    public void paint(Graphics g) {
	setForeground(forced ? JapePrefs.ForcedColour : JapePrefs.TextColour);
	super.paint(g);
    }

}
