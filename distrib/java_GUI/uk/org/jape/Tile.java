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

import java.awt.Component;
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Point;

import java.awt.event.MouseEvent;

import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.JFrame;
import javax.swing.SwingUtilities;

import javax.swing.border.Border;
import javax.swing.BorderFactory;

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
                Reply.sendCOMMAND("tileact \""+text+"\"");
            }
            public void pressed(MouseEvent e) {
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

    protected class TileImage extends DragImage {
        public TileImage() {
            super(Transparent); include(Tile.this); fixImage();
        }
    }
    
    protected int startx, starty, lastx, lasty;
    private boolean firstDrag;
    private TileImage tileImage;
    private Class targetClass;

    protected void pressed(MouseEvent e) { // doesn't matter what keys are pressed
        if (drag_tracing)
            System.err.print("mouse pressed on tile "+text+" at "+e.getX()+","+e.getY()+
                             " insets="+getInsets());
        startx = lastx = e.getX(); starty = lasty = e.getY(); firstDrag = true; // in case of drag
    }

    private TileTarget over;

    protected void dragged(MouseEvent e) {
        if (firstDrag) {
            firstDrag = false;
            try {
                targetClass = Class.forName("TileTarget");
            } catch (ClassNotFoundException exn) {
                Alert.abort("can't make TileTarget a Class");
            }
            over = null;
            if (tileImage==null)
                tileImage = new TileImage();
            layeredPane.add(tileImage, JLayeredPane.DRAG_LAYER);
            tileImage.setLocation(SwingUtilities.convertPoint(this, e.getX()-startx,
                                                              e.getY()-starty, layeredPane));
            if (drag_tracing)
                System.err.println("; dragged tile at "+tileImage.getX()+","+tileImage.getY());
            tileImage.repaint();
            wasteBin.setEnabled(false);
        }
        else {
            if (drag_tracing)
                System.err.print("mouse dragged to "+e.getX()+","+e.getY());
            tileImage.moveBy(e.getX()-lastx, e.getY()-lasty);
            if (drag_tracing)
                System.err.println("; dragged tile now at "+tileImage.getX()+","+tileImage.getY());
            Point p = SwingUtilities.convertPoint(this, e.getX(), e.getY(), contentPane);
            TileTarget target = (TileTarget)japeserver.findTargetAt(targetClass, contentPane, p.x, p.y);
            if (target!=over) {
                if (over!=null) {
                    over.dragExit(); over=null;
                }
                if (target!=null && target.dragEnter(Tile.this))
                    over = target;
            }
        }
        lastx = e.getX(); lasty = e.getY();
    }

    protected void released(MouseEvent e) {
        if (drag_tracing)
            System.err.println("mouse released at "+e.getX()+","+e.getY()+
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
