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

/*
   This exists because it seems to be impossible to force a JScrollPane to leave the position of
   its view alone, and on the other hand impossible to anchor the view to a corner or a midpoint
   in the viewport when resizing.

   It's ok, but it would be nice to be able to do resizing more efficiently.
 */

import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.Color;
import java.awt.Component;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import javax.swing.JComponent;
import javax.swing.JPanel;
import java.awt.Point;
import javax.swing.JScrollBar;
import java.awt.LayoutManager;
import java.awt.Rectangle;
import javax.swing.ScrollPaneConstants;

public class AnchoredScrollPane extends Container {
    private Component view;
    private Rectangle viewBounds;
    private Container viewport;
    private JScrollBar vsb,
                       hsb;

    public AnchoredScrollPane() {
        super();
        hsb = new JScrollBar(JScrollBar.HORIZONTAL);
        hsb.setUnitIncrement(10); hsb.setBlockIncrement(100); // for now
        hsb.addAdjustmentListener(new H());
        vsb = new JScrollBar(JScrollBar.VERTICAL);
        vsb.setUnitIncrement(10); vsb.setBlockIncrement(100); // for now
        vsb.addAdjustmentListener(new V());
        viewport = new JPanel();
        viewport.setLayout(null);
        view = null; // unnecessary, but it makes me feel better
        setLayout(new AnchoredScrollPaneLayout());
        super.add(hsb); super.add(vsb); super.add(viewport);
        viewport.setBackground(Color.white);
    }

    public Rectangle getViewportBounds() {
        return viewport.getBounds();
    }
    
    private class H implements AdjustmentListener {
        public void adjustmentValueChanged(AdjustmentEvent e) {
            switch (e.getAdjustmentType()) {
                case AdjustmentEvent.TRACK:
                    if (view!=null) {
                        Point p = view.getLocation();
                        p.x -= hsb.getValue();
                        view.setLocation(p);
                        validate(); repaint();
                    }
                    break;
                default:
                    System.err.println("H sees AdjustmentEvent "+e.getAdjustmentType());
                    break;
            }
        }
    }

    private class V implements AdjustmentListener {
        public void adjustmentValueChanged(AdjustmentEvent e) {
            switch (e.getAdjustmentType()) {
                case AdjustmentEvent.TRACK:
                    if (view!=null) {
                        Point p = view.getLocation();
                        p.y -= vsb.getValue();
                        view.setLocation(p);
                        validate(); repaint();
                    }
                    break;
                default:
                    System.err.println("V sees AdjustmentEvent "+e.getAdjustmentType());
                    break;
            }
        }
    }

    public static final int ANCHOR_NORTH = 0;
    public static final int ANCHOR_NORTHEAST = 1;
    public static final int ANCHOR_EAST = 2;
    public static final int ANCHOR_SOUTHEAST = 3;
    public static final int ANCHOR_SOUTH = 4;
    public static final int ANCHOR_SOUTHWEST = 5;
    public static final int ANCHOR_WEST = 6;
    public static final int ANCHOR_NORTHWEST = 7;

    private int anchor = ANCHOR_NORTHWEST;

    public Component add(Component c) {
        return add(c, ANCHOR_NORTHWEST);
    }

    public Component add(Component c, int anchor) {
        Rectangle bounds = viewport.getBounds();
        if (view!=null)
            viewport.remove(view);
        view=c; viewBounds=view.getBounds();
        viewport.add(c);
        c.addComponentListener(new ComponentAdapter() {
            public void componentMoved(ComponentEvent e) {
                validate();
            }
            public void componentResized(ComponentEvent e) {
                validate();
            }
        });
        setanchor(anchor);
        return c;
    }

    public void setanchor(int anchor) {
        /* we don't do anything with the anchor at this point: anchoring is a resizing action */
        switch (anchor) {
            case ANCHOR_NORTH:
            case ANCHOR_NORTHEAST:
            case ANCHOR_EAST:
            case ANCHOR_SOUTHEAST:
            case ANCHOR_SOUTH:
            case ANCHOR_SOUTHWEST:
            case ANCHOR_WEST:
            case ANCHOR_NORTHWEST:
                break;
            default:
                anchor = ANCHOR_NORTHWEST;
        }
        this.anchor = anchor;
        
    }
    public void validate() {
        if (view==null) {
            hsb.setValues(0,0,0,0);
            vsb.setValues(0,0,0,0);
        }
        else {
            Rectangle act = view.getBounds();
            Rectangle vis = viewport.getBounds();
            if (vis.x<=act.x && act.x+act.width<=vis.x+vis.width)
                hsb.setValues(vis.x,0,vis.x,vis.x);
            else
                hsb.setValues(vis.x, vis.width,
                              Math.min(vis.x,act.x), Math.max(vis.x+vis.width,act.x+act.width));
            if (vis.y<=act.y && act.y+act.height<=vis.y+vis.height)
                vsb.setValues(vis.y,0,vis.y,vis.y);
            else
                vsb.setValues(vis.y, vis.height,
                              Math.min(vis.y,act.y), Math.max(vis.y+vis.height,act.y+act.height));
            
        }
    }

    /* this is where the anchor policy bites */
    public void setBounds(int x, int y, int w, int h) {
        if (getWidth()!=0 && getHeight()!=0) { /* only for true resizing operations */
            // System.err.print("setBounds "+x+","+y+","+w+","+h+"; "+getBounds()+
            //                  "; "+view.getLocation());
            Point oldPos = view.getLocation();
            switch (anchor) {
                case ANCHOR_NORTH:
                    oldPos.x += (w-getWidth())/2; break;
                case ANCHOR_NORTHEAST:
                    oldPos.x += w-getWidth(); break;
                case ANCHOR_EAST:
                    oldPos.x += w-getWidth(); oldPos.y += (h-getHeight())/2; break;
                case ANCHOR_SOUTHEAST:
                    oldPos.x += w-getWidth(); oldPos.y += h-getHeight(); break;
                case ANCHOR_SOUTH:
                    oldPos.x += (w-getWidth())/2; oldPos.y += h-getHeight(); break;
                case ANCHOR_SOUTHWEST:
                    oldPos.y += h-getHeight(); break;
                case ANCHOR_WEST:
                    oldPos.y += (h-getHeight())/2; break;
                case ANCHOR_NORTHWEST:
                default:
                    break;
            }
            // System.err.println(" => "+oldPos);
            view.setLocation(oldPos);
        }
        super.setBounds(x,y,w,h);
    }
    
    private class AnchoredScrollPaneLayout implements LayoutManager {

        /* Called by the Container add methods. Layout managers that don't associate
        * strings with their components generally do nothing in this method.
        */
        public void addLayoutComponent(String s, Component c) { }

        /* Called by the Container remove and removeAll methods. Many layout managers
        * do nothing in this method, relying instead on querying the container for its
        * components, using the Container getComponents method.
        */
        public void removeLayoutComponent(Component c) { }

        /* Called by the Container getPreferredSize method, which is itself called under
        * a variety of circumstances. This method should calculate and return the ideal
        * size of the container, assuming that the components it contains will be at or
        * above their preferred sizes. This method must take into account the container's
        * internal borders, which are returned by the getInsets method.
        */
        public Dimension preferredLayoutSize(Container c) {
            layoutContainer(c);
            return getSize();
        }

        /* Called by the Container getMinimumSize method, which is itself called under
         * a variety of circumstances. This method should calculate and return the minimum
         * size of the container, assuming that the components it contains will be at or
         * above their minimum sizes. This method must take into account the container's
         * internal borders, which are returned by the getInsets method.
         */
        private Dimension minSize;
        public Dimension minimumLayoutSize(Container c) {
            if (minSize==null) {
                minSize = vsb.getMinimumSize();
                minSize.width+=minSize.height; minSize.height=minSize.width;
            }
            return new Dimension(Math.max(40,minSize.width), Math.max(40,minSize.height));
        }

        /* Called when the container is first displayed, and each time its size changes.
         * A layout manager's layoutContainer method doesn't actually draw components.
         * It simply invokes each component's resize, move, and reshape methods to set
         * the component's size and position. This method must take into account the
         * container's internal borders, which are returned by the getInsets method.
         * You can't assume that the preferredLayoutSize or minimumLayoutSize method
         * will be called before layoutContainer is called.
         */
        public void layoutContainer(Container c) {
            Dimension size = getSize();
            int vsbWidth = vsb.getPreferredSize().width;
            int hsbHeight = hsb.getPreferredSize().height;
            int sparewidth = size.width-vsbWidth;
            int spareheight = size.height-hsbHeight;

            vsb.setBounds(sparewidth, 0, vsbWidth, spareheight);
            hsb.setBounds(0, spareheight, sparewidth, hsbHeight);
            viewport.setBounds(0, 0, sparewidth, spareheight);
            if (view!=null && view instanceof Clickable)
                ((Clickable)view).declareViewportSize(sparewidth, spareheight);
            validate();
        }
    }
}
