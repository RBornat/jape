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

import java.awt.datatransfer.StringSelection;

import java.awt.Component;
import java.awt.Container;
import java.awt.Point;

import java.awt.dnd.DnDConstants;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragSource;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DragSourceListener;

import java.awt.event.MouseEvent;

import javax.swing.JLayeredPane;
import javax.swing.SwingUtilities;

public class StationaryTile extends Tile implements DragSourceListener, DragGestureListener {

    Container layeredPane;
    DragSource dragSource;
    
    public StationaryTile(Container layeredPane, final String text) {
        super(text);
        this.layeredPane = layeredPane;

        dragSource = new DragSource();
        dragSource.createDefaultDragGestureRecognizer( this, DnDConstants.ACTION_MOVE, this);
        
        MouseInteractionListener mil = new MouseInteractionAdapter() {
            public void doubleclicked(byte eventKind, MouseEvent e) {
                Reply.sendCOMMAND("tileact \""+JapeFont.toAscii(text)+"\"");
            }
            /*public void pressed(byte eventKind, MouseEvent e) {
                StationaryTile.this.pressed(e);
            }
            public void dragged(byte eventKind, MouseEvent e) {
                StationaryTile.this.dragged(e);
            }
            public void clicked(byte eventKind, MouseEvent e) {
                StationaryTile.this.released(e);
            }
            public void released(byte eventKind, MouseEvent e) {
                StationaryTile.this.released(e);
            }*/
        };
        addMouseListener(mil);
        addMouseMotionListener(mil);
    }

    /*protected int startx, starty, lastx, lasty;
    protected Tile draggedTile;
    
    protected void pressed(MouseEvent e) { // doesn't matter what keys are pressed
        if (drag_tracing)
            System.err.print("mouse pressed on tile "+text+" at "+e.getX()+","+e.getY()+
                             " insets="+getInsets());
        startx = lastx = e.getX(); starty = lasty = e.getY();
        layeredPane.add(draggedTile = new Tile(text), JLayeredPane.DRAG_LAYER);
        draggedTile.setLocation(SwingUtilities.convertPoint(this, 0, 0, layeredPane));
        if (drag_tracing)
            System.err.println("; dragged tile at "+draggedTile.getX()+","+draggedTile.getY());
        draggedTile.repaint();
    }

    protected void dragged(MouseEvent e) {
        if (drag_tracing)
            System.err.print("mouse dragged to "+e.getX()+","+e.getY());
        draggedTile.repaint();
        draggedTile.setLocation(draggedTile.getX()+(e.getX()-lastx),
                                draggedTile.getY()+(e.getY()-lasty));
        lastx = e.getX(); lasty = e.getY();
        if (drag_tracing)
            System.err.println("; dragged tile now at "+draggedTile.getX()+","+draggedTile.getY());
        draggedTile.repaint();
    }

    protected void released(MouseEvent e) {
        if (drag_tracing)
            System.err.println("mouse released at "+e.getX()+","+e.getY()+
                               "; dragged tile at "+draggedTile.getX()+","+draggedTile.getY());
        layeredPane.remove(draggedTile);
        layeredPane.repaint();
    } */

    public void dragGestureRecognized( DragGestureEvent event) {
        System.err.println("[dragSource] dragGestureRecognized");
        StringSelection selection = new StringSelection(text);
        dragSource.startDrag (event, DragSource.DefaultMoveDrop, selection, this);
    }

    public void dragEnter (DragSourceDragEvent event) {
        System.err.println("[dragSource] dragEnter");
    }

    public void dragExit (DragSourceEvent event) {
        System.err.println("[dragSource] dragExit");
    }

    public void dragOver (DragSourceDragEvent event) {
        System.err.println("[dragSource] dragOver");
    }

    public void dropActionChanged ( DragSourceDragEvent event) {
        System.err.println("[dragSource] dropActionChanged");
    }

    public void dragDropEnd (DragSourceDropEvent event) {
        System.err.println("[dragSource] dragDropEnd "+event.getDropSuccess());
    }
}
