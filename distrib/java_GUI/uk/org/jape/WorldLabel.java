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

import java.awt.Container;
import java.awt.Point;

import java.awt.event.MouseEvent;

import javax.swing.JFrame;
import javax.swing.JLayeredPane;
import javax.swing.SwingUtilities;

public class WorldLabel extends TextItem implements MiscellaneousConstants {
    protected final WorldCanvas canvas;
    protected final WorldItem world;
    protected final String text;
    protected final JLayeredPane layeredPane;
    protected final Container contentPane;

    public WorldLabel(WorldCanvas canvas, JFrame window, WorldItem world,
                      int x, int y, String text) {
        super(canvas, x, y, ProtocolConstants.ProvisoFontNum, text);
        this.canvas = canvas;
        this.layeredPane = window.getLayeredPane();
        this.contentPane = window.getContentPane();
        this.world = world;
        this.text = text;

        addJapeMouseListener(new JapeMouseAdapter() {
            private boolean noticeDrag;
            public void pressed(MouseEvent e) {
                noticeDrag = !(e.isAltDown() || e.isShiftDown() ||
                               e.isMetaDown() || e.isControlDown());
                if (noticeDrag)
                    WorldLabel.this.pressed(e);
            }
            public void dragged(boolean wobbly, MouseEvent e) {
                if (wobbly && noticeDrag)
                    WorldLabel.this.dragged(e); // don't take notice of small movements
            }
            public void released(MouseEvent e) {
                if (noticeDrag)
                    WorldLabel.this.released(e);
            }
        });
    }

    class LabelImage extends DragImage {
        public LabelImage() {
            super(Transparent);
            include(WorldLabel.this); fixImage();
        }
    }

    private int startx, starty, lastx, lasty;
    private boolean firstDrag;
    private LabelImage labelImage;
    private Class targetClass;
    private LabelTarget over;
    
    private void pressed(MouseEvent e) {
        startx = e.getX(); starty = e.getY(); firstDrag = true;
    }

    protected void dragged(MouseEvent e) {
        if (firstDrag) {
            firstDrag = false;
            try {
                targetClass = Class.forName("LabelTarget");
            } catch (ClassNotFoundException exn) {
                Alert.abort("can't make LabelTarget a Class");
            }
            over = null;
            if (labelImage==null)
                labelImage = new LabelImage();
            layeredPane.add(labelImage, JLayeredPane.DRAG_LAYER);
            labelImage.setLocation(SwingUtilities.convertPoint(this, e.getX()-startx,
                                                              e.getY()-starty, layeredPane));
            if (drag_tracing)
                System.err.println("; dragged label at "+labelImage.getX()+","+labelImage.getY());
            labelImage.repaint();
        }
        else {
            if (drag_tracing)
                System.err.print("mouse dragged to "+e.getX()+","+e.getY());
            labelImage.moveBy(e.getX()-lastx, e.getY()-lasty);
            if (drag_tracing)
                System.err.println("; dragged label now at "+labelImage.getX()+","+labelImage.getY());
            Point p = SwingUtilities.convertPoint(this, e.getX(), e.getY(), contentPane);
            LabelTarget target = (LabelTarget)japeserver.findTargetAt(targetClass, contentPane, p.x, p.y);
            if (target!=over) {
                if (over!=null) {
                    over.dragExit(world, text); over=null;
                }
                if (target!=null && target.dragEnter(world, text))
                    over = target;
            }
        }
        lastx = e.getX(); lasty = e.getY();
    }

    protected void released(MouseEvent e) {
        if (drag_tracing)
            System.err.println("mouse released at "+e.getX()+","+e.getY()+
                               "; dragged label at "+labelImage.getX()+","+labelImage.getY());
        if (over==null)
            new Flyback(labelImage, labelImage.getLocation(),
                        SwingUtilities.convertPoint(this, 0, 0, layeredPane)) {
                protected void finishFlyback() { finishDrag(); }
            };
        else {
            over.drop(world, text);
            finishDrag();
        }
    }

    protected void finishDrag() {
        layeredPane.remove(labelImage);
        layeredPane.repaint();
    }
}
