/* 
        Copyright © 2003-17 Richard Bornat & Bernard Sufrin
     
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
import java.awt.Point;
import java.awt.event.MouseEvent;

import javax.swing.BorderFactory;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;

@SuppressWarnings("serial")
public class Tile extends JLabel implements DebugConstants, MiscellaneousConstants {
    final String text;
    private Container layeredPane;
    private Container contentPane;
						
    static final int spacing = LocalSettings.TileSpacing;
    
    static final Border padding = BorderFactory.createEmptyBorder(spacing/2,spacing,spacing/2,spacing),
			raisedbevel = BorderFactory.createRaisedBevelBorder(),
			loweredbevel = BorderFactory.createLoweredBevelBorder(),
			compoundbevel = BorderFactory.createCompoundBorder(raisedbevel, loweredbevel),
			border = BorderFactory.createCompoundBorder(compoundbevel, padding);

    private final WasteBin wasteBin;
    
    public Tile(JFrame window, WasteBin wasteBin, final String text) {
	super(text);
	this.layeredPane = window.getLayeredPane(); this.contentPane = window.getContentPane();
	this.wasteBin = wasteBin; this.text = text;

	setFont(JapeFont.getFont(ProtocolConstants.TermFontNum));
	setBorder(border);
	setSize(getPreferredSize());

	JapeMouseListener mil = new JapeMouseAdapter() {
	    public void doubleclicked(MouseEvent e) {
		Tile.this.getProofWindow().claimDisproofFocus();
		Reply.sendCOMMAND("tileact", text);
	    }
	    public void pressed(MouseEvent e) {
		Tile.this.getProofWindow().claimDisproofFocus();
		Tile.this.pressed(e);
	    }
	    public void dragged(boolean wobbly, MouseEvent e) {
		if (wobbly) Tile.this.dragged(e);
	    }
	    public void released(MouseEvent e) {
		Tile.this.released(e);
	    }
	};
        addMouseListener(mil);
        addMouseMotionListener(mil);
    }

    public ProofWindow getProofWindow() {
	Container c = getParent();
	while (c!=null && !(c instanceof ProofWindow))
	    c = c.getParent();
	return (ProofWindow)c; // null if we ain't in a ProofWindow, obviously -- but we always are
    }
    
    protected class TileImage extends DragImage {
	public TileImage() {
	    super(Transparent); include(Tile.this); fixImage();
	}
    }
    
    protected int startx, starty, lastx, lasty;
    private boolean firstDrag;
    private TileImage tileImage;
    
    protected void pressed(MouseEvent e) { // doesn't matter what keys are pressed
	if (DebugVars.drag_tracing)
	    Logger.log.print("mouse pressed on tile "+text+" at "+e.getX()+","+e.getY()+
			     " insets="+getInsets());
	startx = lastx = e.getX(); starty = lasty = e.getY(); firstDrag = true; // in case of drag
    }

    private TileTarget over;
    private RegisteredDrag dragee = null;
    
    protected void dragged(MouseEvent e) {
	if (firstDrag) {
	    firstDrag = false;
	    dragee = new RegisteredDrag(){
	        public void released(MouseEvent re) {
	            Tile.this.released(re);
	        }
	    };
	    Jape.registerDrag(dragee);
	    over = null; 
	    if (tileImage==null)
		tileImage = new TileImage();
	    layeredPane.add(tileImage, JLayeredPane.DRAG_LAYER);
	    tileImage.setLocation(SwingUtilities.convertPoint(this, e.getX()-startx,
							      e.getY()-starty, layeredPane));
	    if (DebugVars.drag_tracing)
		Logger.log.println("; dragged tile at "+tileImage.getX()+","+tileImage.getY());
	    tileImage.repaint();
	    wasteBin.setEnabled(false);
	}
	else {
	    if (DebugVars.drag_tracing)
		Logger.log.print("mouse dragged to "+e.getX()+","+e.getY());
	    tileImage.moveBy(e.getX()-lastx, e.getY()-lasty);
	    if (DebugVars.drag_tracing)
		Logger.log.println("; dragged tile now at "+tileImage.getX()+","+tileImage.getY());
	    Point p = SwingUtilities.convertPoint(this, e.getX(), e.getY(), contentPane);
	    /* if I knew how to make this polymorphic in Java, I would */
	    Component target = contentPane.findComponentAt(p);
	    if (target instanceof ContainerWithOrigin.Child)
	        target = target.getParent();
	    if (target!=null && target instanceof TileTarget) {
	        TileTarget ttarget = (TileTarget)target;
	        if (ttarget!=over) {
	            if (over!=null) {
	                over.dragExit(Tile.this); 
	                over=null;
	            }
	            if (ttarget!=null && ttarget.dragEnter(Tile.this))
	                over = ttarget;
	        }   
	    }
	    else 
	    if (over!=null) {
	        over.dragExit(Tile.this); over=null;
	    }
	}
	lastx = e.getX(); lasty = e.getY();
    }

    protected void released(MouseEvent e) {
        Jape.deregisterDrag(dragee); 
	if (DebugVars.drag_tracing)
	    Logger.log.println("mouse released at "+e.getX()+","+e.getY()+
			       "; dragged tile at "+tileImage.getX()+","+tileImage.getY());
	if (over==null)
	    new Flyback(tileImage, tileImage.getLocation(),
			SwingUtilities.convertPoint(this, 0, 0, layeredPane)) {
		protected void finishFlyback() { finishDrag(); }
	    };
	else {
	    over.drop(this);
	    finishDrag();
	}
    }

    protected void finishDrag() {
	layeredPane.remove(tileImage);
	layeredPane.repaint();
	wasteBin.setEnabled(true);
    }
}
